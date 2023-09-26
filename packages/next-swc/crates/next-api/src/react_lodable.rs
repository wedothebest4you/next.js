use std::collections::HashMap;

use anyhow::{bail, Result};
use indexmap::IndexMap;
use turbo_tasks::{
    graph::{GraphTraversal, NonDeterministic},
    TryFlatJoinIterExt, ValueToString, Vc,
};
use turbopack_binding::{
    swc::core::{
        css::modules::imports,
        ecma::{
            ast::{
                CallExpr, Callee, Expr, Ident, ImportDefaultSpecifier, Lit, Program, Prop,
                PropName, PropOrSpread,
            },
            visit::{Visit, VisitWith},
        },
    },
    turbopack::{
        core::{module::Module, reference::primary_referenced_modules},
        ecmascript::{chunk::EcmascriptChunkPlaceable, parse::ParseResult, EcmascriptModuleAsset},
    },
};

pub(crate) async fn create_react_lodable_manifest(
    entry: Vc<Box<dyn EcmascriptChunkPlaceable>>,
) -> Result<()> {
    /*
    let all_actions = NonDeterministic::new()
        .skip_duplicates()
        .visit([Vc::upcast(entry)], get_referenced_modules)
        .await
        .completed()?
        .into_inner()
        .into_iter()
        .map(parse_actions_filter_map)
        .try_flat_join()
        .await?
        .into_iter()
        .collect::<IndexMap<_, _>>(); */

    let import_mappings = NonDeterministic::new()
        .skip_duplicates()
        .visit([Vc::upcast(entry)], get_referenced_modules)
        .await
        .completed()?
        .into_inner()
        .into_iter()
        .map(collect_dynamic_imports)
        .try_flat_join()
        .await?
        .into_iter();

    println!("all_actions: {:?}", all_actions);
    Ok(())
}

async fn get_referenced_modules(
    parent: Vc<Box<dyn Module>>,
) -> Result<impl Iterator<Item = Vc<Box<dyn Module>>> + Send> {
    primary_referenced_modules(parent)
        .await
        .map(|modules| modules.clone_value().into_iter())
}

#[turbo_tasks::function]
async fn parse_imports(module: Vc<Box<dyn Module>>) -> Result<Vc<OptionActionMap>> {
    let Some(ecmascript_asset) =
        Vc::try_resolve_downcast_type::<EcmascriptModuleAsset>(module).await?
    else {
        return Ok(OptionActionMap::none());
    };

    let id = &*module.ident().to_string().await?;
    let ParseResult::Ok { program, .. } = &*ecmascript_asset.parse().await? else {
        bail!("failed to parse module '{id}'");
    };

    let Some(actions) = parse_lodable_imports(id.as_str(), &program) else {
        return Ok(OptionActionMap::none());
    };

    let mut actions = IndexMap::from_iter(actions.into_iter());
    actions.sort_keys();
    Ok(Vc::cell(Some(Vc::cell(actions))))
}

struct LodableImportVisitor {
    in_dynamic_call: bool,
    dynamic_ident: Option<Ident>,
    pub import_sources: Vec<String>,
}

impl LodableImportVisitor {
    fn new() -> Self {
        Self {
            in_dynamic_call: false,
            import_sources: vec![],
            dynamic_ident: None,
        }
    }
}

struct CollectImportSourceVisitor {
    import_source: Option<String>,
}

impl CollectImportSourceVisitor {
    fn new() -> Self {
        Self {
            import_source: None,
        }
    }
}

impl Visit for CollectImportSourceVisitor {
    fn visit_call_expr(&mut self, call_expr: &CallExpr) {
        // find import source from import('path/to/module')
        // [NOTE]: Turbopack does not support webpack-specific comment directives, i.e
        // import(/* webpackChunkName: 'hello1' */ '../../components/hello3')
        // Renamed chunk in the comment will be ignored.
        if let Callee::Import(import) = call_expr.callee {
            if let Some(arg) = call_expr.args.first() {
                if let Expr::Lit(lit) = &*arg.expr {
                    if let Lit::Str(str_) = &lit {
                        self.import_source = Some(str_.value.to_string());
                    }
                }
            }
        }

        call_expr.visit_children_with(self);
    }
}

impl Visit for LodableImportVisitor {
    fn visit_import_decl(&mut self, decl: &turbopack_binding::swc::core::ecma::ast::ImportDecl) {
        // find import decl from next/dynamic, i.e import dynamic from 'next/dynamic'
        if decl.src.value == *"next/dynamic" {
            if let Some(specifier) = decl.specifiers.first().map(|s| s.as_default()).flatten() {
                self.dynamic_ident = Some(specifier.local.clone());
            }
        }
    }

    fn visit_call_expr(&mut self, call_expr: &CallExpr) {
        if let Callee::Expr(ident) = &call_expr.callee {
            if let Expr::Ident(ident) = &**ident {
                if let Some(dynamic_ident) = &self.dynamic_ident {
                    if ident.sym == *dynamic_ident.sym {
                        self.in_dynamic_call = true;
                    }
                }
            }
        }

        let mut collect_import_source_visitor = CollectImportSourceVisitor::new();
        call_expr.visit_children_with(&mut collect_import_source_visitor);

        if let Some(import_source) = collect_import_source_visitor.import_source {
            self.import_sources.push(import_source);
        }

        if self.in_dynamic_call {
            self.in_dynamic_call = false;
        }

        call_expr.visit_children_with(self);
    }
}

pub fn parse_lodable_imports(id: &str, program: &Program) -> Option<ActionsMap> {
    let mut visitor = LodableImportVisitor::new();

    program.visit_with(&mut visitor);

    for import_source in &visitor.import_sources {
        let id = format!("{} -> {}", id, import_source);
        println!("id: {}", id);
    }

    None
}

async fn collect_dynamic_imports(
    module: Vc<Box<dyn Module>>,
) -> Result<Option<(Vc<Box<dyn Module>>, Vc<ActionMap>)>> {
    let imports = parse_imports(module).await?;
    /*
    parse_imports(module).await.map(|option_action_map| {
        option_action_map
            .clone_value()
            .map(|action_map| (module, action_map))
    }) */
}

pub type ActionsMap = HashMap<String, String>;

#[turbo_tasks::value(transparent)]
struct ActionMap(IndexMap<String, String>);

//Intermidiate struct contains dynamic import source and its corresponding module
struct DynamicImportsMap(IndexMap<Vc<Box<dyn Module>>, Vec<String>>);

/// An Option wrapper around [ActionMap].
#[turbo_tasks::value(transparent)]
struct OptionActionMap(Option<Vc<ActionMap>>);

#[turbo_tasks::value_impl]
impl OptionActionMap {
    #[turbo_tasks::function]
    pub fn none() -> Vc<Self> {
        Vc::cell(None)
    }
}

#[turbo_tasks::value(transparent)]
struct ModuleActionMap(IndexMap<Vc<Box<dyn Module>>, Vc<ActionMap>>);

#[turbo_tasks::value_impl]
impl ModuleActionMap {
    #[turbo_tasks::function]
    pub fn empty() -> Vc<Self> {
        Vc::cell(IndexMap::new())
    }
}

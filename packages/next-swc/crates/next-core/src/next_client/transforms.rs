use anyhow::Result;
use next_transform_strip_page_exports::ExportFilter;
use turbo_tasks::Vc;
use turbopack_binding::turbopack::turbopack::module_options::ModuleRule;

use crate::{
    mode::NextMode,
    next_client::context::ClientContextType,
    next_config::NextConfig,
    next_shared::transforms::{
        get_next_dynamic_transform_rule, get_next_font_transform_rule, get_next_image_rule,
        get_next_modularize_imports_rule, get_next_pages_transforms_rule,
        get_server_actions_transform_rule, server_actions::ActionsTransform,
    },
};

/// Returns a list of module rules which apply client-side, Next.js-specific
/// transforms.
pub async fn get_next_client_transforms_rules(
    next_config: Vc<NextConfig>,
    context_ty: ClientContextType,
    mode: NextMode,
) -> Result<Vec<ModuleRule>> {
    let mut rules = vec![];

    let modularize_imports_config = &next_config.await?.modularize_imports;
    let enable_server_actions = *next_config.enable_server_actions().await?;
    if let Some(modularize_imports_config) = modularize_imports_config {
        rules.push(get_next_modularize_imports_rule(modularize_imports_config));
    }

    rules.push(get_next_font_transform_rule());

    let pages_dir = match context_ty {
        ClientContextType::Pages { pages_dir } => {
            rules.push(
                get_next_pages_transforms_rule(pages_dir, ExportFilter::StripDataExports).await?,
            );
            Some(pages_dir)
        }
        ClientContextType::App { .. } => {
            if enable_server_actions {
                rules.push(get_server_actions_transform_rule(ActionsTransform::Client));
            }
            None
        }
        ClientContextType::Fallback | ClientContextType::Other => None,
    };

    rules.push(get_next_dynamic_transform_rule(false, false, pages_dir, mode).await?);

    rules.push(get_next_image_rule());

    Ok(rules)
}

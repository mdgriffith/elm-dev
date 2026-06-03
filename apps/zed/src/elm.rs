use zed_extension_api::{self as zed, Result};

const ELM_DEV_BINARY: &str = "elm-dev";

struct ElmDevExtension;

impl ElmDevExtension {
    fn missing_binary_message() -> String {
        "elm-dev must be installed and available on PATH. From the elm-dev repo, run `stack install`.".to_string()
    }
}

impl zed::Extension for ElmDevExtension {
    fn new() -> Self {
        Self
    }

    fn language_server_command(
        &mut self,
        _language_server_id: &zed::LanguageServerId,
        worktree: &zed::Worktree,
    ) -> Result<zed::Command> {
        let command = worktree
            .which(ELM_DEV_BINARY)
            .ok_or_else(Self::missing_binary_message)?;

        Ok(zed::Command {
            command,
            args: vec!["lsp".to_string()],
            env: Default::default(),
        })
    }

    fn context_server_command(
        &mut self,
        _context_server_id: &zed::ContextServerId,
        _project: &zed::Project,
    ) -> Result<zed::Command> {
        Ok(zed::Command {
            command: ELM_DEV_BINARY.to_string(),
            args: vec!["mcp".to_string()],
            env: Default::default(),
        })
    }

    fn context_server_configuration(
        &mut self,
        _context_server_id: &zed::ContextServerId,
        _project: &zed::Project,
    ) -> Result<Option<zed::ContextServerConfiguration>> {
        Ok(Some(zed::ContextServerConfiguration {
            installation_instructions: Self::missing_binary_message(),
            settings_schema: "{}".to_string(),
            default_settings: "{}".to_string(),
        }))
    }
}

zed::register_extension!(ElmDevExtension);

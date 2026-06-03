# Elm Dev for Zed

Local Zed dev extension for Elm support powered by `elm-dev`.

This extension is based on the published Zed Elm extension, but it launches:

- `elm-dev lsp` for language server support
- `elm-dev mcp` as a Zed context server

## Install Locally

1. Install `elm-dev` onto your PATH:

   ```sh
   stack install
   ```

2. In Zed, run `zed: install dev extension`.

3. Select this directory:

   ```text
   apps/zed
   ```

The extension uses `id = "elm"`, so it should override the published Elm extension while installed as a dev extension.

If startup fails, check `zed: open log`.

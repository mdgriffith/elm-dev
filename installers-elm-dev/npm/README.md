## Elm Dev

```bash
npm install -g elm-dev
```

Elm Dev is an Elm compiler with:
- Language Server Protocol (LSP) support
- Model Context Protocol (MCP) support  
- Hot reloading
- VS Code/Cursor plugin
- A full app framework


### Install in VS Code / Cursor
- Open the Extensions view and install the **Elm Dev** extension.

### Set up MCP in VS Code / Cursor
- **Cursor**: create `~/.cursor/mcp.json` with:
```json
{
  "mcpServers": {
    "elm-dev": {
      "command": "elm-dev",
      "args": ["mcp"]
    }
  }
}
```
- **VS Code**: if you use an MCP-capable AI extension, add a server named `elm-dev` with command `elm-dev` and args `["mcp"]`.

### Set up MCP in Codex and Claude Code
- Add an MCP server named `elm-dev` using command `elm-dev` with args `["mcp"]` via each client's MCP settings UI or config file.
```json
{
  "mcpServers": {
    "elm-dev": { "command": "elm-dev", "args": ["mcp"] }
  }
}
```

### CLI help
```
Welcome to Elm Dev

  elm-dev init ...................... Create a new Elm project
  elm-dev make [module] ............... Build your Elm project
  elm-dev install [author/project] ......... Install a package

Add to your Elm app:

  elm-dev add page <url> ...................... Add a new page
  elm-dev add store <name> ................... Add a new store
  elm-dev add effect <name> ................. Add a new effect
  elm-dev add listener <name> ............. Add a new listener

Move an elm-dev-generated file into your project:

  elm-dev customize <module> .... Customize project components

Testing:

  elm-dev test .......... Discover, compile, and run Elm tests
  elm-dev test init ............................ Setup testing
  elm-dev test install <author/project> ..... Install test dep

Development:

  elm-dev dev serve ........... Run the Elm Dev dev (internal)
  elm-dev dev start ................. Start dev if not running
  elm-dev dev stop .................................. Stop dev
  elm-dev dev status .................. Show dev server status
  elm-dev mcp ................... Start the Elm Dev MCP server
  elm-dev lsp ................... Start the Elm Dev LSP server
```

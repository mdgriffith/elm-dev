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
- Open the Extensions view and install the [**Elm Dev**](https://marketplace.visualstudio.com/items?itemName=ElmDev.elm-dev-vscode) extension.

### MCP Setup in VS Code/Cursor/Claude Code

To use the elm-dev mcp, you just need your agent to run `elm-dev mcp`.

In Cursor, this means creating `~/.cursor/mcp.json` with:
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

```

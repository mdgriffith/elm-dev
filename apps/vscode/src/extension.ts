import * as vscode from "vscode";
import { TextEncoder } from "util";
import { log as elmLog } from "./utils/logging";
import * as LSPClient from "./lsp";
import * as MCPClient from "./mcp";
import { stopElmDevDaemon } from "./elmDev";



const AGENT_ELM_DOC = `<!-- elm-dev-mcp-agent-doc -->
# Agents and Elm

This doc is meant for agents that are helping with this Elm project.

As an agent, the Elm Dev MCP can provide you with a lot of information about this project and codebase.  It can frequently be faster and more targeted than scanning the codebase directly.

Examples of data available via the MCP (discoverable by introspection):
- Architecture overview and choices of this specific Elm project.
- Project-specific Elm best practices and guidance for this repository.
- Package documentation with docs and type signatures.
- File-scoped explanations and diagnostics tailored to the current file.

The MCP is likely more efficient than scanning the codebase directly for a lot of things.  If you don't need to read the code, use the MCP first to understand things.
`;


// this method is called when your extension is activated
export async function activate(context: vscode.ExtensionContext) {
  await LSPClient.startLanguageServer(context);
  await MCPClient.startMCPServer(context);

  // Status bar item to show disconnected state and allow restart
  const statusItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
  statusItem.name = 'Elm Dev';
  statusItem.command = 'elm.restart';
  context.subscriptions.push(statusItem);

  // Prevent status updates from overriding the explicit Restarting message
  let isRestarting = false;

  function showDisconnected(message: string) {
    statusItem.text = 'Elm Dev Disconnected (click to restart)';
    statusItem.tooltip = message;
    statusItem.backgroundColor = new vscode.ThemeColor('statusBarItem.errorBackground');
    statusItem.show();
  }

  function showRestarting() {
    statusItem.text = 'Elm Dev Restarting....';
    statusItem.tooltip = 'Elm Dev Restarting....';
    statusItem.backgroundColor = new vscode.ThemeColor('statusBarItem.errorBackground');
    statusItem.show();
  }

  function hideStatus() {
    statusItem.text = 'Elm Dev';
    statusItem.tooltip = 'Elm Dev (click to restart)';
    statusItem.backgroundColor = new vscode.ThemeColor('statusBarItem.background');
    statusItem.hide();
  }

  // Listen for LSP/MCP status changes
  context.subscriptions.push(
    LSPClient.onStatus((evt) => {
      if (isRestarting) return;
      if (evt.connected) {
        hideStatus();
      } else {
        showDisconnected(evt.error ?? 'LSP disconnected');
      }
    })
  );

  context.subscriptions.push(
    MCPClient.onStatus((evt) => {
      if (isRestarting) return;
      if (evt.connected) {
        hideStatus();
      } else {
        showDisconnected(evt.error ?? 'MCP disconnected');
      }
    })
  );

  // Unified restart command: stop LSP, MCP, daemon; then start LSP and MCP again
  context.subscriptions.push(
    vscode.commands.registerCommand("elm.restart", async () => {
      // Show restarting state in status bar with normal background
      isRestarting = true;
      showRestarting();

      elmLog('🔄 Elm Dev Restarting....');
      try {
        elmLog('🛑 Stopping LSP...');
        await LSPClient.stopLanguageServer();

        elmLog('🛑 Stopping MCP...');
        await MCPClient.stopMCPServer();

        await stopElmDevDaemon();

        // Small delay to ensure ports/processes are released
        await new Promise((resolve) => setTimeout(resolve, 2000));

        elmLog('🚀 Starting LSP...');
        await LSPClient.startLanguageServer(context);

        elmLog('🚀 Starting MCP...');
        await MCPClient.startMCPServer(context);

        elmLog('✅ Elm Dev components restarted successfully');
        vscode.window.showInformationMessage('Elm Dev restarted successfully');
        hideStatus();
      } catch (error) {
        isRestarting = false;
        elmLog(`❌ Elm Dev restart failed: ${error}`);
        console.error('Elm Dev restart failed:', error);
        showDisconnected('Elm Dev restart failed');
      } finally {
        isRestarting = false;
      }
    })
  );

  // Generate docs/agent.elm.md with MCP-first guidance for agents
  context.subscriptions.push(
    vscode.commands.registerCommand("elm.generateAgentDoc", async () => {
      try {
        const active = vscode.window.activeTextEditor?.document.uri;
        const folders = vscode.workspace.workspaceFolders;
        let root: vscode.Uri | undefined;

        if (active) {
          const wf = vscode.workspace.getWorkspaceFolder(active);
          root = wf?.uri;
        }

        if (!root && folders && folders.length > 0) {
          root = folders[0].uri;
        }

        if (!root) {
          vscode.window.showErrorMessage('No workspace folder found to write docs/agent.elm.md');
          return;
        }

        const docsUri = vscode.Uri.joinPath(root, 'docs');
        try { await vscode.workspace.fs.createDirectory(docsUri); } catch { }

        const fileUri = vscode.Uri.joinPath(docsUri, 'agent.elm.md');
        const encoded = new TextEncoder().encode(AGENT_ELM_DOC);
        await vscode.workspace.fs.writeFile(fileUri, encoded);
        vscode.window.showInformationMessage(`Wrote ${fileUri.fsPath}`);
      } catch (err) {
        vscode.window.showErrorMessage(`Failed to write docs/agent.elm.md: ${err}`);
      }
    })
  );
}

export async function deactivate() {
  await LSPClient.stopLanguageServer();
  await MCPClient.stopMCPServer();
  await stopElmDevDaemon();
}

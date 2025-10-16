import * as vscode from "vscode";
import { log as elmLog } from "./utils/logging";
import * as LSPClient from "./lsp";
import * as MCPClient from "./mcp";
import { stopElmDevDaemon } from "./elmDev";


// this method is called when your extension is activated
export async function activate(context: vscode.ExtensionContext) {
  await LSPClient.startLanguageServer(context);
  await MCPClient.startMCPServer(context);

  // Status bar item to show disconnected state and allow restart
  const statusItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Left, 100);
  statusItem.name = 'Elm Dev';
  statusItem.command = 'elm.restart';
  context.subscriptions.push(statusItem);

  function showDisconnected(message: string) {
    statusItem.text = 'Elm Dev Disconnected (click to restart)';
    statusItem.tooltip = message;
    statusItem.backgroundColor = new vscode.ThemeColor('statusBarItem.errorBackground');
    statusItem.show();
  }

  function showRestarting() {
    statusItem.text = 'Elm Dev Restarting....';
    statusItem.tooltip = 'Elm Dev Restarting....';
    statusItem.backgroundColor = undefined;
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
      if (evt.connected) {
        hideStatus();
      } else {
        showDisconnected(evt.error ?? 'LSP disconnected');
      }
    })
  );

  context.subscriptions.push(
    MCPClient.onStatus((evt) => {
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
        vscode.window.showInformationMessage('✅ Elm Dev restarted successfully');
        hideStatus();
      } catch (error) {
        elmLog(`❌ Elm Dev restart failed: ${error}`);
        console.error('❌ Elm Dev restart failed:', error);

      }
    })
  );
}

export async function deactivate() {
  await LSPClient.stopLanguageServer();
  await MCPClient.stopMCPServer();
  await stopElmDevDaemon();
}

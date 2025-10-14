import * as vscode from "vscode";
import { spawn } from "child_process";
import { log as elmLog } from "./utils/logging";
import * as LSPClient from "./lsp";
import * as MCPClient from "./mcp";


async function setupLsp(context: vscode.ExtensionContext) {
  // Start the LSP client
  try {
    elmLog('🔄 Starting LSP client...');
    await LSPClient.startLanguageServer(context);
    elmLog('✅ LSP client started successfully');
  } catch (error) {
    elmLog(`❌ LSP startup failed: ${error}`);
    vscode.window.showErrorMessage(`Failed to start Elm Dev LSP: ${error}`);
    // Continue with other functionality even if LSP fails
  }
}

async function setupMcp(context: vscode.ExtensionContext) {
  // Start the MCP server
  try {
    elmLog('🔄 Starting MCP server...');
    await MCPClient.startMCPServer(context);
    elmLog('✅ MCP server started successfully');
  } catch (error) {
    elmLog(`❌ MCP startup failed: ${error}`);
    vscode.window.showErrorMessage(`Failed to start Elm Dev MCP: ${error}`);
    // Continue even if MCP fails
  }
}

async function stopElmDevDaemon(): Promise<void> {
  return await new Promise((resolve) => {
    try {
      const proc = spawn('elm-dev', ['dev', 'stop'], {
        stdio: 'pipe',
        env: process.env,
      });

      proc.stdout.on('data', (chunk: Buffer | string) => {
        const text = chunk.toString();
        elmLog(`daemon ▶ ${text.trimEnd()}`);
      });

      proc.stderr.on('data', (chunk: Buffer | string) => {
        const text = chunk.toString();
        elmLog(`daemon ⚠ ${text.trimEnd()}`);
      });

      const timeout = setTimeout(() => {
        elmLog('⏱️ Timeout waiting for daemon to stop, proceeding...');
        try { proc.kill('SIGKILL'); } catch (_) { }
        resolve();
      }, 8000);

      proc.on('exit', (code) => {
        clearTimeout(timeout);
        elmLog(`🧹 Daemon stop command exited with code: ${code}`);
        resolve();
      });

      proc.on('error', (error) => {
        clearTimeout(timeout);
        elmLog(`💥 Error running 'elm-dev dev stop': ${error}`);
        resolve();
      });
    } catch (error) {
      elmLog(`💥 Exception while stopping daemon: ${error}`);
      resolve();
    }
  });
}

// this method is called when your extension is activated
export async function activate(context: vscode.ExtensionContext) {
  await setupLsp(context);
  await setupMcp(context);

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
        await new Promise((resolve) => setTimeout(resolve, 500));

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
        vscode.window.showErrorMessage(`❌ Failed to restart Elm Dev: ${error}`);
      }
    })
  );
}

export async function deactivate() {
  await LSPClient.stopLanguageServer();
  await MCPClient.stopMCPServer();
}

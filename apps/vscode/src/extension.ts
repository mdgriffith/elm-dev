import * as vscode from "vscode";
import { log as elmLog, logAndShow } from "./utils/logging";
import * as LSPClient from "./lsp/client";
import * as MCPClient from "./mcp/client";


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

  // Add command to restart LSP
  context.subscriptions.push(
    vscode.commands.registerCommand("elm.restartLSP", async () => {
      logAndShow('🔄 Restart LSP command triggered');
      vscode.window.showInformationMessage('🔄 Restarting Elm Dev LSP...');
      try {
        elmLog('🛑 Stopping existing LSP...');
        await LSPClient.stopLanguageServer();
        // Add a small delay to ensure cleanup completes
        await new Promise(resolve => setTimeout(resolve, 1000));
        elmLog('🚀 Starting new LSP...');
        await LSPClient.startLanguageServer(context);
        elmLog('✅ LSP restarted successfully');
        vscode.window.showInformationMessage('✅ Elm Dev LSP restarted successfully');
      } catch (error) {
        elmLog(`❌ LSP restart failed: ${error}`);
        console.error('❌ LSP restart failed:', error);
        vscode.window.showErrorMessage(`❌ Failed to restart Elm Dev LSP: ${error}`);
      }
    })
  );
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

  // Add commands to control MCP
  context.subscriptions.push(
    vscode.commands.registerCommand("elm.restartMCP", async () => {
      logAndShow('🔄 Restart MCP command triggered');
      vscode.window.showInformationMessage('🔄 Restarting Elm Dev MCP...');
      try {
        elmLog('🛑 Stopping existing MCP...');
        await MCPClient.stopMCPServer();
        // Ensure cleanup completes
        await new Promise(resolve => setTimeout(resolve, 500));
        elmLog('🚀 Starting new MCP...');
        await MCPClient.startMCPServer(context);
        elmLog('✅ MCP restarted successfully');
        vscode.window.showInformationMessage('✅ Elm Dev MCP restarted successfully');
      } catch (error) {
        elmLog(`❌ MCP restart failed: ${error}`);
        console.error('❌ MCP restart failed:', error);
        vscode.window.showErrorMessage(`❌ Failed to restart Elm Dev MCP: ${error}`);
      }
    })
  );
}

// this method is called when your extension is activated
export async function activate(context: vscode.ExtensionContext) {
  logAndShow('🚀 Elm Dev extension is activating!');
  vscode.window.showInformationMessage('Elm Dev extension activated!');

  await setupLsp(context);
  await setupMcp(context);
}

export async function deactivate() {
  await LSPClient.stopLanguageServer();
  await MCPClient.stopMCPServer();
}

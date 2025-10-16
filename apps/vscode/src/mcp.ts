import * as vscode from 'vscode';
import { ChildProcess } from 'child_process';
import { log as elmLog } from './utils/logging';
import { startElmDevProcess, gracefulStop, isProcessRunning } from './elmDev';

// Emit status updates for MCP so the extension can react
const statusEmitter = new vscode.EventEmitter<{ connected: boolean; error?: string }>();
export const onStatus = statusEmitter.event;

let mcpProcess: ChildProcess | undefined;
let isStarting = false;

export async function startMCPServer(_context: vscode.ExtensionContext): Promise<void> {

  if (isStarting) {
    elmLog('⚠️ MCP server is already starting, skipping...');
    return;
  }

  if (isProcessRunning(mcpProcess)) {
    elmLog('✅ MCP server already running');
    return;
  }

  isStarting = true;
  try {
    const proc = startElmDevProcess('MCP', ['mcp'], undefined, {
      onStatus: (s) => statusEmitter.fire(s),
      onError: (error: Error) => {
        elmLog(`💥 MCP process error: ${error.message}`);
        statusEmitter.fire({ connected: false, error: error.message });
      },
    });

    mcpProcess = proc;

  } catch (error) {
    elmLog(`💥 Error starting MCP server: ${error}`);
    statusEmitter.fire({ connected: false, error: String(error) });
    throw error;
  } finally {
    isStarting = false;
    elmLog('🏁 startMCPServer finished, isStarting set to false');
  }
}

export async function stopMCPServer(): Promise<void> {
  if (!mcpProcess) {
    elmLog('ℹ️ stopMCPServer: no running process');
    return;
  }

  try {
    elmLog('🛑 Stopping MCP server...');
    await gracefulStop(mcpProcess, 'MCP', 5000);
    mcpProcess = undefined;
  } catch (error) {
    elmLog(`💥 Error stopping MCP server: ${error}`);
    // Ensure state is cleared so a subsequent start can proceed
    try { mcpProcess?.kill('SIGKILL'); } catch (_) { }
    mcpProcess = undefined;
  }
}


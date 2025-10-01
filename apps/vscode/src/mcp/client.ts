import * as vscode from 'vscode';
import { spawn, ChildProcess } from 'child_process';
import { log as elmLog, logAndShow } from '../utils/logging';

let mcpProcess: ChildProcess | undefined;
let isStarting = false;

function isProcessRunning(proc: ChildProcess | undefined): boolean {
    return !!proc && !proc.killed;
}

export async function startMCPServer(_context: vscode.ExtensionContext): Promise<void> {
    elmLog('🔧 startMCPServer called');
    elmLog(`  isStarting: ${isStarting}`);
    elmLog(`  running: ${isProcessRunning(mcpProcess)}`);

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
        const cwd = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
        elmLog(`🏗️ Spawning MCP server (cwd: ${cwd ?? 'undefined'})...`);

        const proc = spawn('elm-dev', ['mcp'], {
            cwd,
            stdio: 'pipe',
            env: process.env,
        });

        mcpProcess = proc;

        proc.stdout.on('data', (chunk: Buffer | string) => {
            const text = chunk.toString();
            elmLog(`MCP ▶ ${text.trimEnd()}`);
        });

        proc.stderr.on('data', (chunk: Buffer | string) => {
            const text = chunk.toString();
            elmLog(`MCP ⚠ ${text.trimEnd()}`);
        });

        proc.on('error', (error: Error) => {
            elmLog(`💥 MCP process error: ${error.message}`);
            vscode.window.showErrorMessage(`Elm Dev MCP server error: ${error.message}`);
        });

        proc.on('exit', (code: number | null, signal: NodeJS.Signals | null) => {
            elmLog(`🛑 MCP process exited (code: ${code}, signal: ${signal})`);
            mcpProcess = undefined;
        });

        logAndShow('🚀 Elm Dev MCP server starting...');
    } catch (error) {
        elmLog(`💥 Error starting MCP server: ${error}`);
        vscode.window.showErrorMessage(`Failed to start Elm Dev MCP server: ${error}`);
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
        const proc = mcpProcess;
        return await new Promise((resolve) => {
            const timeout = setTimeout(() => {
                elmLog('⏱️ MCP did not exit in time, force killing...');
                try { proc.kill('SIGKILL'); } catch (_) { }
                mcpProcess = undefined;
                resolve();
            }, 5000);

            proc.once('exit', () => {
                clearTimeout(timeout);
                mcpProcess = undefined;
                resolve();
            });

            try {
                proc.kill('SIGTERM');
            } catch (e) {
                elmLog(`💥 Error sending SIGTERM to MCP: ${e}`);
                try { proc.kill('SIGKILL'); } catch (_) { }
            }
        });
    } catch (error) {
        elmLog(`💥 Error stopping MCP server: ${error}`);
        // Ensure state is cleared so a subsequent start can proceed
        try { mcpProcess?.kill('SIGKILL'); } catch (_) { }
        mcpProcess = undefined;
    }
}

export function isMcpRunning(): boolean {
    return isProcessRunning(mcpProcess);
}



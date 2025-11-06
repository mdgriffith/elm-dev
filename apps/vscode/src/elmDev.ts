import { spawn, ChildProcess, SpawnOptionsWithoutStdio, StdioOptions } from 'child_process';
import * as vscode from 'vscode';
import { log as elmLog } from './utils/logging';
import * as fs from 'fs';
import which from 'which';

export function isProcessRunning(proc: ChildProcess | undefined): boolean {
    return !!proc && !proc.killed;
}

export type ElmDevHandlers = {
    onStdout?: (text: string) => void;
    onStderr?: (text: string) => void;
    onError?: (error: Error) => void;
    onExit?: (code: number | null, signal: NodeJS.Signals | null) => void;
    onStatus?: (status: { connected: boolean; error?: string }) => void;
};

export function startElmDevProcess(
    label: string,
    args: string[],
    options?: SpawnOptionsWithoutStdio & { stdio?: StdioOptions },
    handlers?: ElmDevHandlers
): ChildProcess {
    const launch = resolveElmDev();

    const proc = spawn(launch.command, args, {
        stdio: options?.stdio ?? 'pipe',
        env: options?.env ?? process.env,
        cwd: options?.cwd ?? process.cwd(),
        detached: options?.detached ?? false,
        windowsHide: options?.windowsHide ?? true,
        shell: options?.shell ?? launch.useShell ?? false,
    });

    // Only attach stdout listener if a handler is provided, to avoid interfering with LSP streams
    if (handlers?.onStdout) {
        proc.stdout?.on('data', (chunk: Buffer | string) => {
            const text = chunk.toString();
            handlers.onStdout(text);
        });
    }

    proc.stderr?.on('data', (chunk: Buffer | string) => {
        const text = chunk.toString();
        elmLog(`${label} ⚠ ${text.trimEnd()}`);
        handlers?.onStderr?.(text);
    });

    proc.on('error', (error: Error) => {
        elmLog(`💥 ${label} process error: ${error.message}`);
        handlers?.onError?.(error);
        handlers?.onStatus?.({ connected: false, error: error.message });
    });

    proc.on('exit', (code: number | null, signal: NodeJS.Signals | null) => {
        elmLog(`🛑 ${label} process exited (code: ${code}, signal: ${signal})`);
        handlers?.onExit?.(code, signal);
        handlers?.onStatus?.({ connected: false, error: `${label} exited (code: ${code}, signal: ${signal ?? ''})` });
    });

    // Signal connected upon successful spawn
    handlers?.onStatus?.({ connected: true });

    return proc;
}

export async function gracefulStop(
    proc: ChildProcess | undefined,
    label: string,
    timeoutMs: number = 5000
): Promise<void> {
    if (!proc) {
        elmLog(`ℹ️ ${label}: no running process`);
        return;
    }

    return await new Promise((resolve) => {
        const timeout = setTimeout(() => {
            elmLog(`⏱️ ${label} did not exit in time, force killing...`);
            try { proc.kill('SIGKILL'); } catch (_) { }
            resolve();
        }, timeoutMs);

        proc.once('exit', () => {
            clearTimeout(timeout);
            resolve();
        });

        try {
            proc.kill('SIGTERM');
        } catch (e) {
            elmLog(`💥 Error sending SIGTERM to ${label}: ${e}`);
            try { proc.kill('SIGKILL'); } catch (_) { }
        }
    });
}

export async function stopElmDevDaemon(): Promise<void> {
    return await new Promise((resolve) => {
        const launch = resolveElmDev();
        const proc = spawn(launch.command, ['dev', 'stop'], {
            stdio: 'pipe',
            env: process.env,
            shell: launch.useShell ?? false,
        });

        proc.stdout?.on('data', (chunk: Buffer | string) => {
            const text = chunk.toString();
            elmLog(`daemon ▶ ${text.trimEnd()}`);
        });

        proc.stderr?.on('data', (chunk: Buffer | string) => {
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
    });
}

export function resolveElmDev(): { command: string; useShell?: boolean, notFoundReason?: string } {

    const cfg = vscode.workspace.getConfiguration('elmDev');
    const configured = (cfg.get<string>('path') || '').trim();
    const isWin = process.platform === 'win32';

    // If a path is configured, use it directly
    if (configured) {
        const exists = fs.existsSync(configured);
        if (!exists) {
            return { command: configured, useShell: isWin, notFoundReason: `Configured elmDev.path not found: ${configured}` };
        }
        try {
            const stats = fs.statSync(configured);
            if (stats.isDirectory()) {
                return { command: configured, useShell: isWin, notFoundReason: `Configured elmDev.path is a directory, expected an executable.` };
            }
            if (!isWin) {
                try {
                    fs.accessSync(configured, fs.constants.X_OK);
                } catch (_) {
                    return { command: configured, useShell: isWin, notFoundReason: `Configured elmDev.path is not executable: ${configured}` };
                }
            }
        } catch (e) {
            return { command: configured, useShell: isWin, notFoundReason: `Unable to access configured elmDev.path: ${configured}` };
        }

        return { command: configured, useShell: isWin };
    }

    // Try to resolve 'elm-dev' on PATH using 'which'
    const cmd = 'elm-dev';
    const resolved = which.sync(cmd, { nothrow: true });
    if (resolved) {
        return { command: resolved, useShell: isWin };
    }

    return { command: cmd, useShell: isWin, notFoundReason: `elm-dev not found on PATH. Install it or set 'elmDev.path' in settings.` };
}



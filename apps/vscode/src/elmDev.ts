import { spawn, ChildProcess, SpawnOptionsWithoutStdio, StdioOptions } from 'child_process';
import { log as elmLog } from './utils/logging';

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
    const proc = spawn('elm-dev', args, {
        stdio: options?.stdio ?? 'pipe',
        env: options?.env ?? process.env,
        cwd: options?.cwd ?? process.cwd(),
        detached: options?.detached ?? false,
        windowsHide: options?.windowsHide ?? true,
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
        const proc = spawn('elm-dev', ['dev', 'stop'], {
            stdio: 'pipe',
            env: process.env,
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



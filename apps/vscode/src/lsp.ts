import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  State,
  ErrorAction,
  CloseAction,
} from 'vscode-languageclient/node';
import { log as elmLog, outputChannel } from './utils/logging';

// Emit status updates so the extension can show a status indicator when disconnected
const statusEmitter = new vscode.EventEmitter<{ connected: boolean; error?: string }>();
export const onStatus = statusEmitter.event;

let client: LanguageClient | undefined;
let isStarting = false;
let stopRequested = false;


export async function startLanguageServer(context: vscode.ExtensionContext): Promise<void> {

  // Prevent multiple simultaneous start attempts
  if (isStarting) {
    elmLog('⚠️ LSP client already starting, skipping...');
    return;
  }

  // If client is already running, don't restart
  if (client && client.state === State.Running) {
    elmLog('✅ LSP client already running');
    return;
  }

  isStarting = true;
  try {
    // Only create a new client if we don't have one
    if (client) {
      elmLog('⚠️ Client already exists, skipping creation');
      isStarting = false;
      return;
    }

    elmLog('🏗️ Creating LSP client...');

    // Let the language client spawn the server as an executable using stdio transport
    const serverOptions: ServerOptions = {
      run: { command: 'elm-dev', args: ['lsp'], options: { env: process.env, shell: true } },
      debug: { command: 'elm-dev', args: ['lsp'], options: { env: process.env, shell: true } },
    };

    const clientOptions: LanguageClientOptions = {
      // Register the server for Elm files
      documentSelector: [{ scheme: 'file', language: 'elm' }],
      synchronize: {
        // Notify the server about file changes to '.elm' files contained in the workspace
        fileEvents: vscode.workspace.createFileSystemWatcher('**/*.elm')
      },
      // Set working directory to the first workspace folder if available
      workspaceFolder: vscode.workspace.workspaceFolders?.[0],
      // Reuse a single shared output channel to avoid duplicate channels after restarts
      outputChannel,
      traceOutputChannel: outputChannel,
      // Add error handler to prevent client from restarting automatically
      errorHandler: {
        error: () => ({ action: ErrorAction.Shutdown }),
        closed: () => ({ action: CloseAction.DoNotRestart })
      }
    };

    // Create the language client
    client = new LanguageClient(
      'elmDevLanguageServer',
      'Elm Dev Language Server',
      serverOptions,
      clientOptions
    );

    // Add detailed event listeners
    client.onDidChangeState((event) => {
      elmLog(`🔄 LSP changed: ${State[event.oldState]} → ${State[event.newState]}`);
      if (event.newState === State.Stopped) {
        // If not an intentional stop, signal disconnection
        if (!stopRequested) {
          statusEmitter.fire({ connected: false, error: 'LSP client stopped unexpectedly' });
        }
      }
    });

    // Start the client. This will also launch the server
    try {
      // Start the client (spawns process via serverOptions) and wait until it is ready
      const timeoutMs = 60000;
      const timeout = new Promise<never>((_, reject) =>
        setTimeout(() => reject(new Error(`LSP client start timed out after ${timeoutMs / 1000} seconds`)), timeoutMs)
      );
      await Promise.race([client!.start(), timeout]);

      elmLog('🎉 LSP client started successfully!');
      statusEmitter.fire({ connected: true });

    } catch (error) {
      elmLog(`💥 LSP client start failed: ${error}`);
      elmLog(`Error details: ${JSON.stringify(error, null, 2)}`);


      client = undefined; // Clear the client on failure
      statusEmitter.fire({ connected: false, error: String(error) });
      throw error;
    }
  } catch (error) {
    elmLog(`💥 Error in startLanguageServer: ${error}`);
    throw error;
  } finally {
    isStarting = false;
  }
}

export async function stopLanguageServer(): Promise<void> {
  if (client) {
    try {
      stopRequested = true;
      const state = client.state;
      elmLog(`🛑 Stopping LSP client in state: ${State[state]}`);

      // Only try to stop if the client is in a running state
      if (state === State.Running) {
        elmLog('🔴 Client is running, stopping and disposing...');
        await client.stop();
        await client.dispose();
      } else {
        elmLog('🔸 Client not in running state, disposing directly...');
        // For non-running states, just dispose
        await client.dispose();
      }
    } catch (error) {
      elmLog(`💥 Error stopping language client: ${error}`);
      await client.dispose();
    } finally {
      client = undefined;
      elmLog('🧹 Client cleared');
      stopRequested = false;
    }
  }
}


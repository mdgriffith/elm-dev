import * as vscode from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
  State,
  ErrorAction,
  CloseAction,
} from 'vscode-languageclient/node';
import { log as elmLog, logAndShow } from './utils/logging';

let client: LanguageClient | undefined;
let isStarting = false;

export async function startLanguageServer(context: vscode.ExtensionContext): Promise<void> {
  elmLog('🔧 startLanguageServer called');
  elmLog(`  isStarting: ${isStarting}`);
  elmLog(`  client state: ${client?.state ? State[client.state] : 'undefined'}`);

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

    // The server is implemented as a separate process
    const serverOptions: ServerOptions = {
      run: { command: 'elm-dev', args: ['lsp'], transport: TransportKind.stdio },
      debug: { command: 'elm-dev', args: ['lsp'], transport: TransportKind.stdio }
    };

    // Options to control the language client
    const clientOptions: LanguageClientOptions = {
      // Register the server for Elm files
      documentSelector: [{ scheme: 'file', language: 'elm' }],
      synchronize: {
        // Notify the server about file changes to '.elm' files contained in the workspace
        fileEvents: vscode.workspace.createFileSystemWatcher('**/*.elm')
      },
      // Set working directory to the first workspace folder if available
      workspaceFolder: vscode.workspace.workspaceFolders?.[0],
      // Add error handler to prevent client from restarting automatically
      errorHandler: {
        error: () => ({ action: ErrorAction.Shutdown }),
        closed: () => ({ action: CloseAction.DoNotRestart })
      }
    };

    // Create the language client
    elmLog('📦 Creating LanguageClient instance...');
    client = new LanguageClient(
      'elmDevLanguageServer',
      'Elm Dev Language Server',
      serverOptions,
      clientOptions
    );
    elmLog('✅ LanguageClient instance created');

    // Add detailed event listeners
    client.onDidChangeState((event) => {
      elmLog(`🔄 Client state changed: ${State[event.oldState]} → ${State[event.newState]}`);
    });

    // Start the client. This will also launch the server
    logAndShow('🚀 Starting LSP client (calling client.start())...');
    try {
      // Add timeout to avoid hanging indefinitely
      const timeout = new Promise((_, reject) =>
        setTimeout(() => reject(new Error('LSP client start timed out after 10 seconds')), 10000)
      );

      await Promise.race([client.start(), timeout]);
      elmLog('🎉 LSP client started successfully!');
      vscode.window.showInformationMessage('Elm Dev Language Server started successfully');
    } catch (error) {
      elmLog(`💥 LSP client start failed: ${error}`);
      elmLog(`Error details: ${JSON.stringify(error, null, 2)}`);

      // Also check if the server process is actually running
      elmLog('🔍 Checking server process...');
      if (client) {
        elmLog(`Client state after failure: ${State[client.state]}`);
      }

      client = undefined; // Clear the client on failure
      vscode.window.showErrorMessage(`Failed to start Elm Dev Language Server: ${error}`);
      throw error;
    }
  } catch (error) {
    elmLog(`💥 Error in startLanguageServer: ${error}`);
    throw error;
  } finally {
    isStarting = false;
    elmLog('🏁 startLanguageServer finished, isStarting set to false');
  }
}

export async function stopLanguageServer(): Promise<void> {
  if (client) {
    try {
      const state = client.state;
      elmLog(`🛑 Stopping LSP client in state: ${State[state]}`);

      // Only try to stop if the client is in a running state
      if (state === State.Running) {
        elmLog('🔴 Client is running, stopping...');
        await client.stop();
      } else {
        elmLog('🔸 Client not in running state, disposing directly...');
        // For non-running states, just dispose
        client.dispose();
      }
    } catch (error) {
      elmLog(`💥 Error stopping language client: ${error}`);
      // If stopping fails, forcefully dispose
      try {
        client.dispose();
      } catch (disposeError) {
        elmLog(`💥 Error disposing language client: ${disposeError}`);
      }
    } finally {
      client = undefined;
      elmLog('🧹 Client cleared');
    }
  }
}

export function getClient(): LanguageClient | undefined {
  return client;
} 
import * as vscode from "vscode";

import { ElmFormatProvider, ElmRangeFormatProvider } from "./elmFormat";
import * as log from "./utils/log";
import * as VSCodeStyle from "./utils/vscodeColorPalette";
import * as Watchtower from "./watchtower";
import { ElmProjectPane, ElmProjectSerializer } from "./panel/panel";
import * as LSPClient from "./lsp/client";
import { log as elmLog, logAndShow } from "./utils/logging";

import * as PanelMsg from "./panel/messages";

const ElmLanguage: vscode.DocumentFilter = { language: "elm", scheme: "file" };

// this method is called when your extension is activated
export async function activate(context: vscode.ExtensionContext) {
  logAndShow('🚀 Elm Dev extension is activating!');
  console.log('🚀 Elm Dev extension is activating!');
  vscode.window.showInformationMessage('Elm Dev extension activated!');
  
  // Start the LSP client
  try {
    elmLog('🔄 Starting LSP client...');
    await LSPClient.startLanguageServer(context);
    elmLog('✅ LSP client started successfully');
  } catch (error) {
    elmLog(`❌ LSP startup failed: ${error}`);
    console.error('LSP startup failed:', error);
    vscode.window.showErrorMessage(`Failed to start Elm Dev LSP: ${error}`);
    // Continue with other functionality even if LSP fails
  }

  // Elm Format (keep existing formatting functionality)
  context.subscriptions.push(
    vscode.languages.registerDocumentFormattingEditProvider(
      ElmLanguage,
      new ElmFormatProvider()
    )
  );
  context.subscriptions.push(
    vscode.languages.registerDocumentRangeFormattingEditProvider(
      ElmLanguage,
      new ElmRangeFormatProvider()
    )
  );

  /* 
      Keep existing Watchtower functionality alongside LSP
      (you may want to disable some of this if LSP provides the same features)
  */
  // let tower = new Watchtower.Watchtower();
  // vscode.languages.registerCodeLensProvider(
  //   ElmLanguage,
  //   tower.codelensProvider
  // );
  // Note: Commenting out definitions provider since LSP will handle this
  // vscode.languages.registerDefinitionProvider(
  //   ElmLanguage,
  //   tower.definitionsProvider
  // );
  // tower.setup();

  // vscode.commands.registerCommand(
  //   "elm-dev.addTypeSignature",
  //   (doc: vscode.TextDocument, signature: any, onFire: any) => {
  //     onFire();
  //   }
  // );

  // context.subscriptions.push(tower.statusbar);

  // context.subscriptions.push(
  //   vscode.commands.registerCommand("elm.projectPanel", () => {
  //     tower.showPanel(context.extensionPath);
  //   })
  // );

  // context.subscriptions.push(
  //   vscode.commands.registerCommand("elm.connect", () => {
  //     tower.connect();
  //   })
  // );

  // Add command to restart LSP
  context.subscriptions.push(
    vscode.commands.registerCommand("elm.restartLSP", async () => {
      logAndShow('🔄 Restart LSP command triggered');
      console.log('🔄 Restart LSP command triggered');
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

  // Helpful utility to generate a stylesheet with all the vscode colors
  // const styleString = await vscode.workspace.openTextDocument(
  //   vscode.Uri.parse("vscode://schemas/workbench-colors")
  // );
  // const styleItems = JSON.parse(styleString.getText());
  // context.subscriptions.push(VSCodeStyle.register(styleItems));

  // vscode.window.registerWebviewPanelSerializer(
  //   ElmProjectPane.viewType,
  //   new ElmProjectSerializer(context.extensionPath)
  // );
}

export async function deactivate() {
  await LSPClient.stopLanguageServer();
}

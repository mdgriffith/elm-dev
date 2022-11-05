import * as vscode from "vscode";

import { ElmFormatProvider, ElmRangeFormatProvider } from "./elmFormat";
import * as log from "./utils/log";
import * as Watchtower from "./watchtower";
import { ElmProjectPane, ElmProjectSerializer } from "./panel/panel";

import * as PanelMsg from "./panel/messages";

const ElmLanguage: vscode.DocumentFilter = { language: "elm", scheme: "file" };

// this method is called when your extension is activated
export function activate(context: vscode.ExtensionContext) {
  // Elm Format
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
      Start watch tower
  */
  let tower = new Watchtower.Watchtower();
  vscode.languages.registerCodeLensProvider(
    ElmLanguage,
    tower.codelensProvider
  );
  vscode.languages.registerDefinitionProvider(
    ElmLanguage,
    tower.definitionsProvider
  );
  tower.setup();

  vscode.commands.registerCommand(
    "elm-dev.addTypeSignature",
    (doc: vscode.TextDocument, signature: any, onFire: any) => {
      onFire();
    }
  );

  context.subscriptions.push(
    vscode.commands.registerCommand("elm.projectPanel", () => {
      ElmProjectPane.createOrShow(context.extensionPath);
    })
  );

  vscode.window.registerWebviewPanelSerializer(
    ElmProjectPane.viewType,
    new ElmProjectSerializer(context.extensionPath)
  );

  /*  Send editor visibility msgs to the Panel */

  vscode.window.onDidChangeActiveTextEditor((editor) => {
    ElmProjectPane.send(PanelMsg.sendEditorVisibility());
  });

  vscode.window.onDidChangeTextEditorSelection((selection) => {
    ElmProjectPane.send(PanelMsg.sendEditorVisibility());
  });

  vscode.window.onDidChangeTextEditorVisibleRanges((visibleRanges) => {
    ElmProjectPane.send(PanelMsg.sendEditorVisibility());
  });

  if (vscode.window.activeTextEditor) {
    ElmProjectPane.send(PanelMsg.sendEditorVisibility());
  }
}

export function deactivate() {}

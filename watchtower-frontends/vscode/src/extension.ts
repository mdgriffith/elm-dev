import * as vscode from "vscode";

import { ElmFormatProvider, ElmRangeFormatProvider } from "./elmFormat";
import * as log from "./utils/log";
import * as watchtower from "./watchtower";
import { ElmProjectPane, ElmProjectSerializer } from "./panel/panel";

const ElmLanguage: vscode.DocumentFilter = { language: "elm", scheme: "file" };

// this method is called when your extension is activated
export function activate(context: vscode.ExtensionContext) {
  log.log(`Watchtower activating`);
  const elmFormatStatusBar = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Left
  );
  elmFormatStatusBar.backgroundColor = new vscode.ThemeColor(
    "statusBarItem.errorBackground"
  );

  // Elm Format
  context.subscriptions.push(
    vscode.languages.registerDocumentFormattingEditProvider(
      ElmLanguage,
      new ElmFormatProvider(elmFormatStatusBar)
    )
  );
  context.subscriptions.push(
    vscode.languages.registerDocumentRangeFormattingEditProvider(
      ElmLanguage,
      new ElmRangeFormatProvider(elmFormatStatusBar)
    )
  );

  /* 
      Start watch tower
  */
  let tower = new watchtower.Watchtower();
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
    "elm-watchtower.addTypeSignature",
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
}

export function deactivate() {}

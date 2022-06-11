import * as vscode from "vscode";

import { ElmFormatProvider, ElmRangeFormatProvider } from "./elmFormat";
import * as log from "./utils/log";
import * as watchtower from "./watchtower";

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

  // var iconCache = icons.createIcons(context);

  // context.subscriptions.push(
  //   vscode.commands.registerCommand("elm.liveErrorView", () => {
  //     ElmErrorPanel.createOrShow(context.extensionPath, iconCache);
  //   })
  // );

  // if (vscode.window.registerWebviewPanelSerializer) {
  //   // Make sure we register a serilizer in activation event
  //   vscode.window.registerWebviewPanelSerializer(ElmErrorPanel.viewType, {
  //     async deserializeWebviewPanel(
  //       webviewPanel: vscode.WebviewPanel,
  //       state: any
  //     ) {
  //       log.log(`Got state: ${state}`);
  //       ElmErrorPanel.revive(webviewPanel, context.extensionPath);
  //     },
  //   });
  // }
}

export function deactivate() {}

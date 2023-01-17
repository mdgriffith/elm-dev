import * as vscode from "vscode";

import { ElmFormatProvider, ElmRangeFormatProvider } from "./elmFormat";
import * as log from "./utils/log";
import * as VSCodeStyle from "./utils/vscodeColorPalette";
import * as Watchtower from "./watchtower";
import { ElmProjectPane, ElmProjectSerializer } from "./panel/panel";

import * as PanelMsg from "./panel/messages";

const ElmLanguage: vscode.DocumentFilter = { language: "elm", scheme: "file" };

// this method is called when your extension is activated
export async function activate(context: vscode.ExtensionContext) {
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
  vscode.languages.registerCodeLensProvider(
    ElmLanguage,
    tower.diagnostics.codeLenses
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

  context.subscriptions.push(tower.statusbar);

  context.subscriptions.push(
    vscode.commands.registerCommand("elm.projectPanel", () => {
      tower.showPanel(context.extensionPath);
    })
  );

  // Helpful utility to generate a stylesheet with all the vscode colors
  // const styleString = await vscode.workspace.openTextDocument(
  //   vscode.Uri.parse("vscode://schemas/workbench-colors")
  // );
  // const styleItems = JSON.parse(styleString.getText());
  // context.subscriptions.push(VSCodeStyle.register(styleItems));

  vscode.window.registerWebviewPanelSerializer(
    ElmProjectPane.viewType,
    new ElmProjectSerializer(context.extensionPath)
  );
}

export function deactivate() {}

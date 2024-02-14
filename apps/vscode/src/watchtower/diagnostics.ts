import * as vscode from "vscode";
import * as log from "../utils/log";

export class WatchTowerDiagnostics {
  private diagnostics: vscode.DiagnosticCollection;
  private errorHighlight;

  constructor() {
    const self = this;
    self.diagnostics = vscode.languages.createDiagnosticCollection("elmDev");
    self.createDecorationStyle();
  }

  private createDecorationStyle() {
    const self = this;
    self.errorHighlight = vscode.window.createTextEditorDecorationType({
      overviewRulerColor: new vscode.ThemeColor("minimap.errorHighlight"),
      isWholeLine: true,
      borderWidth: "0px 0px 0px 3px",
      borderColor: new vscode.ThemeColor("inputValidation.errorBorder"),
      borderStyle: "solid",
    });
  }

  public clear() {
    const self = this;
    self.diagnostics.clear();

    // Clear all decorations
    self.errorHighlight.dispose();
    // Recreate the decoration so it can be made anew
    self.createDecorationStyle();
  }

  public set(uri, problems) {
    const self = this;

    self.diagnostics.set(uri, problems);

    const editor = getEditorMatching(uri);
    if (editor != null) {
      const decorations = [];
      for (const prob of problems) {
        decorations.push({
          range: prob.fullRange,
          hoverMessage: "Elm error",
        });
      }
      editor.setDecorations(self.errorHighlight, decorations);
    }
  }
}

function getEditorMatching(uri: vscode.Uri): vscode.TextEditor | null {
  const editor = vscode.window.activeTextEditor;

  if (editor) {
    if (editor.document.uri.toString() == uri.toString()) {
      return editor;
    }
  }

  return null;
}

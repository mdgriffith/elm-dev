import * as vscode from "vscode";
import * as log from "../utils/log";

export class WatchTowerDiagnostics {
  private diagnostics: vscode.DiagnosticCollection;
  private errorHighlight;
  public codeLenses: ElmErrorCodeLens;

  constructor() {
    const self = this;
    self.diagnostics = vscode.languages.createDiagnosticCollection("elmDev");
    self.createDecorationStyle();
    self.codeLenses = new ElmErrorCodeLens();
  }

  private createDecorationStyle() {
    const self = this;
    self.errorHighlight = vscode.window.createTextEditorDecorationType({
      overviewRulerColor: "#ff00ff",
      isWholeLine: true,
      borderWidth: "0px 0px 0px 3px",
      borderColor: new vscode.ThemeColor("inputValidation.errorBackground"),
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

    self.codeLenses.clear();
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
      self.codeLenses.setErrors(uri, problems);
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

/*                DIAGNOSTIC CODE LENS               */

/**
 * CodelensProvider
 */
export class ElmErrorCodeLens implements vscode.CodeLensProvider {
  private _onDidChangeCodeLenses: vscode.EventEmitter<void> =
    new vscode.EventEmitter<void>();
  public readonly onDidChangeCodeLenses: vscode.Event<void> =
    this._onDidChangeCodeLenses.event;

  private errors;

  constructor() {
    this.errors = [];
  }

  public clear() {
    this.errors = [];
    this._onDidChangeCodeLenses.fire();
  }

  public setErrors(uri, problems) {
    const lenses = [];
    for (const prob of problems) {
      let newLens = new vscode.CodeLens(
        new vscode.Range(
          prob.range.start.line,
          // prob.range.start.column,

          16,
          // prob.range.end.line,
          prob.range.start.line,
          80
        ),
        {
          title: prob.title + " -> Show in Elm Dev", //prob.message,
          tooltip: "Show error in Elm Dev",
          command: "elm.projectPanel",
          arguments: [],
        }
      );
      lenses.push(newLens);
    }
    this.errors = lenses;
    this._onDidChangeCodeLenses.fire();
  }

  public provideCodeLenses(
    document: vscode.TextDocument,
    token: vscode.CancellationToken
  ): vscode.CodeLens[] | Thenable<vscode.CodeLens[]> {
    return this.errors;
  }

  public resolveCodeLens(
    codeLens: vscode.CodeLens,
    token: vscode.CancellationToken
  ) {
    const self = this;

    // This function will be called for each visible code lens, usually when scrolling and after calls to compute-lenses.
    // It's for when codelenses are expensive to create
    return codeLens;
  }
}

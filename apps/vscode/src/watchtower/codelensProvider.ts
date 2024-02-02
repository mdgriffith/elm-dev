import * as vscode from "vscode";
import * as EditorCoords from "../utils/editorCoords";
import * as Question from "../watchtower/question";

/* Codelens Provider! */

type SignatureAction = {
  signature: Question.MissingSignature;
  filepath: string;
};

type SignatureFileCache = {
  signatures: MissingSignature[];
};

type MissingSignature = {
  missing: Question.MissingSignature;
  lens: vscode.CodeLens;
};

function getDocumentMatching(uri: vscode.Uri): vscode.TextDocument | null {
  const editor = vscode.window.activeTextEditor;

  if (editor) {
    if (editor.document.uri.toString() == uri.toString()) {
      return editor.document;
    }
  }

  return null;
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

function signatureToLens(
  document: vscode.TextDocument,
  signature: Question.MissingSignature,
  onClick: any,
  editsSinceSave: null | vscode.TextDocumentChangeEvent[]
): vscode.CodeLens {
  let edits = {
    uri: document.uri,
    edits: editsSinceSave,
  };
  if (editsSinceSave == null) {
    edits = null;
  }
  const range = EditorCoords.regionToRange(signature.region, edits);

  let newLens = new vscode.CodeLens(range);

  newLens.command = {
    title: signature.name + " : " + signature.signature,
    tooltip: "Add type signature",
    command: "elm-dev.addTypeSignature",
    arguments: [document, signature, onClick],
  };
  return newLens;
}

/**   DECORATIONS   **/

const unusedValueDecorationType = vscode.window.createTextEditorDecorationType({
  opacity: "0.5",
  // overviewRulerColor: "#ff00ff",
  // isWholeLine: true,
  // overviewRulerLane: vscode.OverviewRulerLane.Full,
});

/**
 * CodelensProvider
 */
export class SignatureCodeLensProvider implements vscode.CodeLensProvider {
  private signatures: { [key: string]: SignatureFileCache } = {};
  private actions: SignatureAction[] = [];
  private _onDidChangeCodeLenses: vscode.EventEmitter<void> =
    new vscode.EventEmitter<void>();
  public readonly onDidChangeCodeLenses: vscode.Event<void> =
    this._onDidChangeCodeLenses.event;

  constructor() {
    /*
        vscode.workspace.onDidChangeConfiguration((_) => {
            this._onDidChangeCodeLenses.fire();
        });
        */
  }

  public clear() {
    this.signatures = {};
    this.actions = [];
  }

  // editsSinceSave is the dumb correction we need to make until
  // the server is fast enough to return before edits can come in.
  public setSignaturesFromWarnings(
    filepath: string,
    warnings: Question.Warning[],
    editsSinceSave: vscode.TextDocumentChangeEvent[]
  ) {
    const self = this;
    const uri = vscode.Uri.file(filepath);
    const decorations: any[] = [];
    const document = getDocumentMatching(uri);

    // TODO! destroy existing codelenses
    const signatures: any[] = [];
    for (const warn of warnings) {
      if (warn.warning == "MissingAnnotation") {
        if (document != null) {
          const sig: Question.MissingSignature = {
            filepath: filepath,
            name: warn.name,
            region: warn.region,
            signature: warn.signature,
          };
          const onClick = () => {
            self.queueAction({
              signature: sig,
              filepath: document.uri.fsPath,
            });
          };
          const lens = signatureToLens(document, sig, onClick, editsSinceSave);
          signatures.push({ missing: sig, lens: lens });
        }
      } else if (warn.warning == "UnusedVariable") {
        const dec = {
          range: EditorCoords.regionToRange(warn.region, {
            uri: uri,
            edits: editsSinceSave,
          }),
          hoverMessage: "This is unused.",
        };
        decorations.push(dec);
      } else if (warn.warning == "UnusedImport") {
        const range = EditorCoords.regionToRange(warn.region, {
          uri: uri,
          edits: editsSinceSave,
        });
        // We do this so that the whole line is highlighted
        // instead of just the import name
        const line = document.lineAt(range.start.line);

        const dec = {
          range: line.range,
          hoverMessage: "This is unused.",
        };
        decorations.push(dec);
      }
    }

    const editor = getEditorMatching(vscode.Uri.file(filepath));
    if (editor != null) {
      editor.setDecorations(unusedValueDecorationType, decorations);
    }

    this.signatures[filepath] = {
      signatures: signatures,
    };
    this._onDidChangeCodeLenses.fire();
  }

  public queueAction(action: SignatureAction) {
    this.actions.push(action);
    this._onDidChangeCodeLenses.fire();
  }

  private getSignaturesFor(filepath: string): MissingSignature[] {
    if (filepath in this.signatures) {
      return this.signatures[filepath].signatures;
    } else {
      return [];
    }
  }

  public lineAdjustment(
    document: vscode.TextDocument,
    filepath: string,
    affectedRange: vscode.Range,
    lineCountChange: number
  ) {
    const self = this;
    if (lineCountChange == 0) {
      return;
    }

    if (filepath in self.signatures) {
      // All signatures passed the affected range will be pushed or pulled.
      if (filepath in self.signatures) {
        for (const sig of this.signatures[filepath].signatures) {
          if (affectedRange.start.line < sig.missing.region.start.line) {
            // adjust signature
            sig.missing.region.start.line += lineCountChange;
            sig.missing.region.end.line += lineCountChange;

            // adjust codelens
            const onClick = () => {
              self.queueAction({
                signature: sig.missing,
                filepath: document.uri.fsPath,
              });
            };
            sig.lens = signatureToLens(document, sig.missing, onClick, null);
          }
        }
      }
    }
  }

  private runActions(document: vscode.TextDocument) {
    const self = this;
    for (const action of self.actions) {
      const editor = vscode.window.activeTextEditor;

      if (editor) {
        const position = new vscode.Position(
          action.signature.region.start.line - 1,
          action.signature.region.start.column - 1
        );
        const newSignature =
          action.signature.name + " : " + action.signature.signature + "\n";
        editor.edit((editBuilder) => {
          editBuilder.insert(position, newSignature);
        });
      }

      // remove the signature from the list
      if (document.uri.fsPath in self.signatures) {
        const newSignatures: any[] = [];
        for (const sig of self.getSignaturesFor(document.uri.fsPath)) {
          if (action.signature.name !== sig.missing.name) {
            newSignatures.push(sig);
          }
        }

        // We don't need to call `setSignatures` because we're already in a render loop.
        let existing = self.signatures[document.uri.fsPath];
        existing.signatures = newSignatures;
      }
    }
    self.actions = [];
  }

  /*
      A Note on the Action thing here!
  
  
      It's slightly non-ideal. So, if your spidey sense is tingling, here's why.
  
      When a codelens is clicked, we want to
        1. First, remove the codelens
        2. Afterwards, insert the new type signature.
  
      If we don't do this carefully, then we have a flash where
      both the codelens and the new typesignature are present
      before the codelens is deleted
      and it looks janky.
  
  
    */
  public provideCodeLenses(
    document: vscode.TextDocument,
    token: vscode.CancellationToken
  ): vscode.CodeLens[] | Thenable<vscode.CodeLens[]> {
    const self = this;

    // provide all code lenses as quickly as possible

    if (document.uri.fsPath in self.signatures) {
      const cache = self.signatures[document.uri.fsPath];
      const lenses: any[] = [];
      for (const sig of cache.signatures) {
        lenses.push(sig.lens);
      }

      if (self.actions.length > 0) {
        self.runActions(document);
      }
      return lenses;
    }

    // we don't have any type signatures
    return [];
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

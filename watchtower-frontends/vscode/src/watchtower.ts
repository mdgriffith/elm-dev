import * as vscode from "vscode";
import * as log from "./utils/log";
import * as JSONSafe from "./utils/json";
import * as Question from "./watchtower/question";
import { ElmProjectPane } from "./panel/panel";
import * as ChildProcess from "child_process";
import * as Interactive from "./interactive";
import * as PanelMsg from "./panel/messages";
import * as path from "path";

var WebSocketClient = require("websocket").client;

type Msg =
  | { msg: "Discover"; details: String }
  | { msg: "Changed"; details: { path: String } }
  | {
      msg: "Watched";
      details: { path: String; warnings: Boolean; docs: Boolean }[];
    };

const discover = (roots: String): Msg => {
  return { msg: "Discover", details: roots };
};

const changed = (filepath: String): Msg => {
  return { msg: "Changed", details: { path: filepath } };
};

const refreshWatch = (): Msg => {
  const editors = vscode.window.visibleTextEditors;
  const items = [];
  for (const index in editors) {
    if (editors[index].document.uri.fsPath.endsWith(".elm")) {
      items.push({
        path: editors[index].document.uri.fsPath,
        warnings: true,
        docs: false,
      });
    }
  }

  return { msg: "Watched", details: items };
};

type Project = {
  root: String;
  entrypoints: String[];
};

function socketConnect(options) {
  const websocket = new WebSocketClient();

  websocket.on("connectFailed", function (error) {
    options.onConnectionFailed(error);
  });

  websocket.on("connect", function (connection) {
    connection.on("error", function (error) {
      log.log("Connection Error: " + error.toString());
    });
    connection.on("close", function () {
      log.log("Connection Closed");
    });
    connection.on("message", function (message) {
      if (message.type === "utf8") {
        options.receive(message.utf8Data);
      }
    });

    options.onJoin(connection);
  });

  websocket.connect(options.url);

  return { websocket: websocket };
}

export class Watchtower {
  private connection;
  private projects: Project[];
  private editsSinceSave: vscode.TextDocumentChangeEvent[];
  private trackEditsSinceSave: boolean;
  private retry;
  public codelensProvider: SignatureCodeLensProvider;
  public diagnostics: vscode.DiagnosticCollection;
  public definitionsProvider: vscode.DefinitionProvider;

  send(msg: Msg) {
    if (this.connection?.connected) {
      log.obj("SENDING", msg);
      this.connection.sendUTF(JSON.stringify(msg));
    } else {
      log.obj("SKIPPING (not connected)", msg);
    }
  }

  constructor() {
    let self = this;
    self.editsSinceSave = [];
    self.trackEditsSinceSave = false;
    self.codelensProvider = new SignatureCodeLensProvider();
    self.definitionsProvider = new ElmDefinitionProvider();
    self.diagnostics = vscode.languages.createDiagnosticCollection("elmDev");

    self.startServer();

    socketConnect({
      url: Question.urls.websocket,
      onJoin: (connection) => {
        self.onJoin(connection);
      },
      onConnectionFailed: (err) => {
        self.onConnectionFailed(err);
      },
      receive: (msg) => {
        self.receive(msg);
      },
    });

    vscode.workspace.onDidSaveTextDocument((document: vscode.TextDocument) => {
      self.editsSinceSave = [];
      self.trackEditsSinceSave = true;
      self.send(changed(document.uri.fsPath));
    });

    vscode.workspace.onDidChangeTextDocument(
      (docChange: vscode.TextDocumentChangeEvent) => {
        if (docChange.document.languageId == "elm") {
          // We track what edits were done since save so that we can correct the source positions on data coming
          // back from the server
          // This is a temporary hack until the server can handle edits being sent to it.
          if (self.trackEditsSinceSave) {
            self.editsSinceSave.push(docChange);
          }

          // Adjust the lines of the code lenses so they stay in sync
          for (const change of docChange.contentChanges) {
            const lineCountChange = lineCountDelta(change);
            self.codelensProvider.lineAdjustment(
              docChange.document,
              docChange.document.uri.fsPath,
              change.range,
              lineCountChange
            );
          }
        }
      }
    );

    vscode.window.onDidChangeVisibleTextEditors((editors) => {
      self.send(refreshWatch());
    });
  }

  private startServer() {
    Question.ask(
      Question.questions.serverHealth,
      (resp) => {
        log.log("Elm Dev server is already running!");
      },
      (err) => {
        log.log("Elm Dev server is not running, starting watchtower 2");
        try {
          const elmDev = ChildProcess.spawn(path.join(__dirname, "elm-dev"), [
            "start",
            `--port=${Question.port}`,
          ]);
          // log.obj("ELM DEV", elmDev);
          elmDev.on("close", function (code) {
            //Here you can get the exit code of the script

            log.log("THE GOOD TIMES ARE OVER code: " + code);
          });

          elmDev.stdout.setEncoding("utf8");
          elmDev.stdout.on("data", function (data) {
            //Here is where the output goes

            log.log("elmout: " + data.toString());

            // data=data.toString();
            // scriptOutput+=data;
          });

          elmDev.stderr.setEncoding("utf8");
          elmDev.stderr.on("data", function (data) {
            //Here is where the error output goes
            log.log("elmerr: " + data.toString());
          });
        } catch (watchTowerErr) {
          log.log("Bundled Elm Dev failed to auto-start");
          log.log(watchTowerErr);
        }
      }
    );
  }

  private onConnectionFailed(error) {
    const self = this;
    this.retry = setTimeout(function () {
      // log.log("Reattempting connection");
      socketConnect({
        url: Question.urls.websocket,
        onJoin: (connection) => {
          self.onJoin(connection);
        },
        onConnectionFailed: (err) => {
          self.onConnectionFailed(err);
        },
        receive: (msg) => {
          self.receive(msg);
        },
      });
    }, 10000);
  }

  private cancelRetry() {
    if (this.retry) {
      clearTimeout(this.retry);
      this.retry = null;
    }
  }

  private onJoin(connection) {
    this.cancelRetry();
    this.connection = connection;
    log.log("Connected!");

    if (
      vscode.workspace.workspaceFolders &&
      vscode.workspace.workspaceFolders.length > 0
    ) {
      const root = vscode.workspace.workspaceFolders[0].uri.fsPath;
      this.send(discover(root));
    }
  }

  private receive(msgString: string) {
    const self = this;
    const msg = JSONSafe.parse(msgString);
    if (msg == null) {
      return;
    }
    ElmProjectPane.send(msg);

    log.log("Received: " + msg["msg"]);
    switch (msg["msg"]) {
      case "Status": {
        self.diagnostics.clear();
        for (const project of msg["details"]) {
          if ("errors" in project.status) {
            for (const error of project.status["errors"]) {
              log.log("   ERROR:" + error.path);
              const uri = vscode.Uri.file(error["path"]);
              let problems: any[] = [];
              for (const prob of error["problems"]) {
                problems.push({
                  code: "elm-compiler",
                  message: formatMessage(prob["message"]),
                  range: regionToRange(prob["region"], {
                    uri: uri,
                    edits: self.editsSinceSave,
                  }),
                  severity: vscode.DiagnosticSeverity.Error,
                  source: "",
                  relatedInformation: [],
                });
              }
              self.diagnostics.set(uri, problems);
            }
          } else if ("compiled" in project.status) {
            // success
          } else {
            // Global error
            log.log(
              "GLOBAL ERROR -> elm-vscode doesn't do anything with this right now, we should!"
            );
          }
        }
        break;
      }
      case "Warnings": {
        self.codelensProvider.setSignaturesFromWarnings(
          msg.details.filepath,
          msg.details.warnings,
          self.editsSinceSave
        );
        break;
      }
      case "Docs": {
        log.log("New docs received!");
        log.log(msg.details.filepath);
        log.log(msg.details.docs[0].name);

        // Turn off the interactive stuff while we get everything working smoothly
        // Interactive.generate(msg.details.docs, (interactiveJs) => {
        //   log.log("Interactive compiled!");
        //   ElmProjectPane.send(PanelMsg.interactiveCodeRefreshed(interactiveJs));
        // });

        break;
      }
      default: {
        log.log("Unknown msg received");
        log.log(msgString);
      }
    }
    // Stop tracking edits for the correction.
    // This isn't totally correct because we're using a websocket and
    //  can receive N messages for one "save" message we send up
    self.trackEditsSinceSave = false;
  }

  setup() {
    // log.log("Setting up! (which means doing nothing right now.");
  }
}

const formatMessage = (items: any): string => {
  let message = "";
  for (const item of items) {
    if (typeof item === "string") {
      const lines = item.split("\n");
      let justStarted = true;
      for (const line of lines) {
        if (line.match(/^\d/)) {
          // starts with a number, then it's a code snippet
          // We want to edit the code snippet out so we have more room for the actual message
          //
        } else {
          if (justStarted) {
            justStarted = false;
          } else {
            if (!message.endsWith("\n\n")) {
              message += "\n";
            }
          }
          message += line;
        }
      }
    } else {
      const str = item["string"];
      if (str.includes("^")) {
        // This is just a highlight string
      } else {
        message += item["string"];
      }
    }
  }

  return message;
};

const extractSource = /^\d*[|](.*)$/;

const formatSource = (items: any): string => {
  let source = "";
  for (const item of items) {
    if (typeof item === "string") {
      const lines = item.split("\n");
      for (const line of lines) {
        if (line.match(/^\d/)) {
          // starts with a number, then it's a code snippet
          const matched = line.match(extractSource);
          if (matched) {
            source += matched[1];
          }
          source += "\n";
        }
      }
    }
  }

  return source;
};

/*   Asking questions!  */

// Definition Provider

export class ElmDefinitionProvider implements vscode.DefinitionProvider {
  constructor() {}
  public provideDefinition(
    document: vscode.TextDocument,
    position: vscode.Position,
    token: vscode.CancellationToken
  ): vscode.ProviderResult<vscode.Definition | vscode.LocationLink[]> {
    const self = this;
    return new Promise((resolve, reject) => {
      Question.ask(
        Question.questions.findDefinition(
          document.uri.fsPath,
          position.line + 1,
          position.character + 1
        ),
        (resp) => {
          if (!resp) {
            resolve(null);
          } else {
            const uri = vscode.Uri.file(resp.definition.path);

            const region = regionToRange(resp.definition.region, null);

            resolve(new vscode.Location(uri, region));
          }
        },
        reject
      );
    });
  }
}

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

function lineCountDelta(edit) {
  const oldLineCount = edit.range.end.line - edit.range.start.line;
  const newLineCount = (edit.text.match(/\n/g) || "").length;
  return newLineCount - oldLineCount;
}

/* Note: If this ever gets an invalid region, e.g. a line or column of 0,
 * then this will eventually hang forever. and cause regions to not display.
 */
function regionToRange(
  region,
  maybeEditsSinceSave: {
    uri: vscode.Uri;
    edits: vscode.TextDocumentChangeEvent[];
  } | null
): vscode.Range {
  let lineOffset = 0;
  if (maybeEditsSinceSave != null) {
    const uri = maybeEditsSinceSave.uri;

    for (const edit of maybeEditsSinceSave.edits) {
      if (edit.document.uri.fsPath == uri.fsPath) {
        for (const change of edit.contentChanges) {
          if (change.range.start.line < region.start.line) {
            lineOffset += lineCountDelta(change);
          }
        }
      }
    }
  }

  return new vscode.Range(
    Math.max(0, region.start.line - 1 + lineOffset),
    Math.max(0, region.start.column - 1),
    Math.max(0, region.end.line - 1 + lineOffset),
    Math.max(0, region.end.column - 1)
  );
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
  const range = regionToRange(signature.region, edits);

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
          range: regionToRange(warn.region, {
            uri: uri,
            edits: editsSinceSave,
          }),
          hoverMessage: "This is unused.",
        };
        decorations.push(dec);
      } else if (warn.warning == "UnusedImport") {
        const range = regionToRange(warn.region, {
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

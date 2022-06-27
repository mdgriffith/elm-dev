// import { setFlagsFromString } from "v8";
import { send } from "process";
import * as vscode from "vscode";
import * as log from "./utils/log";
import * as JSONSafe from "./utils/json";
import * as Question from "./watchtower/question";
import { ElmProjectPane } from "./panel/panel";

var WebSocketClient = require("websocket").client;

type Msg = { msg: "Watch"; details: String[] };

const watch = (roots: String[]): Msg => {
  return { msg: "Watch", details: roots };
};

type Project = {
  root: String;
  entrypoints: String[];
};



export class Watchtower {
  private websocket;
  private connection;
  private projects: Project[];
  public codelensProvider: SignatureCodeLensProvider;
  public diagnostics: vscode.DiagnosticCollection;
  public definitionsProvider: vscode.DefinitionProvider;

  send(msg: Msg) {
    if (this.connection.connected) {
      log.obj("SENDING", msg);
      this.connection.sendUTF(JSON.stringify(msg));
    } else {
      log.log("not connected, attempting to send -> ");
      log.obj("SENDING", msg);
    }
  }

  constructor() {
    log.log("creating watchtower");
    let self = this;
    // create code lens provider
    self.codelensProvider = new SignatureCodeLensProvider();

    self.definitionsProvider = new ElmDefinitionProvider();
    self.diagnostics =
      vscode.languages.createDiagnosticCollection("elmWatchtower");

    // start websocket stuff
    self.websocket = new WebSocketClient();

    self.websocket.on("connectFailed", function (error) {
      log.log("Connect Error: " + error.toString());
    });

    self.websocket.on("connect", function (connection) {
      log.log("WebSocket Client Connected");

      connection.on("error", function (error) {
        log.log("Connection Error: " + error.toString());
      });
      connection.on("close", function () {
        log.log("Connection Closed");
      });
      connection.on("message", function (message) {
        if (message.type === "utf8") {
          self.receive(message.utf8Data);
        }
      });
      self.connection = connection;

      // OnJoin

      if (vscode.workspace.workspaceFolders.length > 0) {
        const root = vscode.workspace.workspaceFolders[0].uri.fsPath;
        log.log("ROOT: " + root)
        Question.ask(Question.questions.discover(root), (resp) => {
          self.projects = resp;

          // Tell watchtower which elm projects to provide updates for
          let projectRoots = [];
          for (const proj of self.projects) {
            projectRoots.push(proj.root);
          }
          log.obj("WATCHING", projectRoots)
          self.send(watch(projectRoots));

          vscode.window.visibleTextEditors.forEach((editor) => {
            // ask for missing type signatures
            // for all visible text editors
            self.refreshCodeLenses(editor.document);
          });
        });
      }
    });

    self.websocket.connect(Question.urls.websocket);

    vscode.workspace.onDidSaveTextDocument((document: vscode.TextDocument) => {
      self.refreshCodeLenses(document);
    });

    vscode.workspace.onDidChangeTextDocument(
      (docChange: vscode.TextDocumentChangeEvent) => {
        if (docChange.document.languageId == "elm") {
          for (const change of docChange.contentChanges) {
            const oldLineCount =
              change.range.end.line - change.range.start.line;
            const newLineCount = (change.text.match(/\n/g) || "").length;
            const lineCountChange = newLineCount - oldLineCount;
            self.codelensProvider.lineAdjustment(
              docChange.document.uri.fsPath,
              change.range,
              lineCountChange
            );
          }
        }
      }
    );

    vscode.window.onDidChangeActiveTextEditor((editor) => {
      if (editor) {
        // Ask for new type signatures
        self.refreshCodeLenses(editor.document);
      }
    });

   
  }
  private receive(msgString: string) {
    const self = this;
    const msg = JSONSafe.parse(msgString);
   
    if (msg == null) {
      return
    }
    ElmProjectPane.send(msg)
    
    log.obj("Received", msg);
    switch (msg["msg"]) {
      case "Status": {
        self.diagnostics.clear();
        
        for (const project of msg["details"]) {
          log.obj("PROJECT", project)
          log.obj("PROJECT.STATUS", project.status)
          if ("errors" in project.status) {
            for (const error of project.status["errors"]) {
              log.obj("ERROR", error)
              const uri = vscode.Uri.file(error["path"]);
              for (const prob of error["problems"]) {
                self.diagnostics.set(uri, [
                  {
                    code: "elm-compiler-error",
                    message: formatMessage(prob["message"]),
                    range: prepareRange(prob["region"]),
                    severity: vscode.DiagnosticSeverity.Error,
                    source: "",
                    relatedInformation: [],
                  },
                ]);
              }
            }
          } else if ("compiled" in project.status) {
            // success
          } else {
            // Global error
            log.log("GLOBAL ERROR -> elm-vscode doesn't do anything with this right now, we should!")
          }

         
        }
        break;
      }
      default: {
        log.log("Unknown msg received");
        log.log(msgString);
      }
    }
  }

  setup() {
    log.log("Setting up! (which means doing nothing right now.");
  }

  refreshCodeLenses(document: vscode.TextDocument) {
    if (document.languageId != "elm") {
      // We only care about Elm files
      return 
    }

    const self = this;
    if (document.isDirty) {
      // Only ask for type signatures if the file is as it is on disk
      // so the elm-compiler can read it without fuss
      return;
    }
    const filepath = document.uri.fsPath;
    Question.ask(Question.questions.listMissingSignatures(filepath), (resp) => {
      if (resp != null) {
        self.codelensProvider.setSignatures(filepath, resp);
      }
    });
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
          source += line.match(extractSource)[1];
          source += "\n";
        }
      }
    }
  }

  return source;
};

/*   Asking questions!  */

function prepareRange(region) {
  return new vscode.Range(
    preparePosition(region["start"]),
    preparePosition(region["end"])
  )
}

function preparePosition(pos: any) {
  return new vscode.Position(pos["line"] - 1, pos["column"] - 1);
}

function prepareRanges(ranges) {
  const rangeLength = ranges.length;

  var prepared = [];
  for (var i = 0; i < rangeLength; i++) {
    const start = {
      character: ranges[i].start.character,
      line: ranges[i].start.line,
    };
    const end = {
      character: ranges[i].end.character,
      line: ranges[i].end.line,
    };
    prepared.push({ start: start, end: end });
  }
  return prepared;
}

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
          document.uri.path,
          position.line,
          position.character
        ),
        (resp) => {
          log.log("FOUND DEFINITION!");
          log.log(JSON.stringify(resp));
          if (!resp) {
            resolve(null);
          } else {
            const uri = vscode.Uri.file(resp.definition.path);
            const start: vscode.Position = preparePosition(
              resp.definition.region.start
            );
            const end: vscode.Position = preparePosition(
              resp.definition.region.end
            );
            resolve(new vscode.Location(uri, new vscode.Range(start, end)));
          }
        }
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
  signatures: Question.MissingSignature[];
  lenses: vscode.CodeLens[];
  lensesAreCurrent: Boolean;
};

function signatureToLens(
  document: vscode.TextDocument,
  signature: Question.MissingSignature,
  onClick: any
): vscode.CodeLens {
  const range = new vscode.Range(
    signature.region.start.line - 1,
    signature.region.start.column - 1,
    signature.region.end.line - 1,
    signature.region.end.column - 1
  );

  let newLens = new vscode.CodeLens(range);

  newLens.command = {
    title: signature.name + " : " + signature.signature,
    tooltip: "Add type signature",
    command: "elm-watchtower.addTypeSignature",
    arguments: [document, signature, onClick],
  };
  return newLens;
}

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
  public setSignatures(filepath: string, sigs: Question.MissingSignature[]) {
    this.signatures[filepath] = {
      signatures: sigs,
      lensesAreCurrent: false,
      lenses: [],
    };
    this._onDidChangeCodeLenses.fire();
  }

  public queueAction(action: SignatureAction) {
    this.actions.push(action);
    if (action.filepath in this.signatures) {
      this.signatures[action.filepath].lensesAreCurrent = false;
    }
    this._onDidChangeCodeLenses.fire();
  }

  private getSignaturesFor(filepath: string) {
    if (filepath in this.signatures) {
      return this.signatures[filepath].signatures
    } else {
      return []
    }
  }

  public lineAdjustment(
    filepath: string,
    affectedRange: vscode.Range,
    lineCountChange: number
  ) {
    const self = this;
    if (lineCountChange == 0) {
      return;
    }

    if (filepath in self.signatures) {
      let newSigs = [];
      // All signatures passed the affected range will be pushed or pulled.
      for (const sig of self.getSignaturesFor(filepath)) {
        const startingPoint = new vscode.Position(
          sig.region.start.line - 1,
          sig.region.start.column - 1
        );
        if (affectedRange.start.line < sig.region.start.line) {
          sig.region.start.line += lineCountChange;
          sig.region.end.line += lineCountChange;
          newSigs.push(sig);
        } else {
          newSigs.push(sig);
        }
      }
      self.setSignatures(filepath, newSigs);
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
        const newSignatures = [];
        for (const sig of self.getSignaturesFor(document.uri.fsPath)) {
          if (action.signature.name !== sig.name) {
            newSignatures.push(sig);
          }
        }

        // We don't need to call `setSignatures` because we're already in a render loop.
        let existing = self.signatures[document.uri.fsPath]
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

      if (cache.lensesAreCurrent) {
        // Lenses are current, don't do anything
        return cache.lenses;
      } else {
        // Recreate them!
        cache.lenses = [];
        
 
        for (const sig of cache.signatures) {
          var skip = false;
          // if this signature matches an action, then don't do anything with it
          // It will be deleted
          for (const action of self.actions) {
            if (action.signature.name === sig.name) {
              log.log(`->  Skipping ${action.signature.name}`);
              skip = true;
              break;
            }
          }
          if (skip) {
            continue;
          }

          const onClick = () => {
            self.queueAction({
              signature: sig,
              filepath: document.uri.fsPath,
            });
          };
          cache.lenses.push(signatureToLens(document, sig, onClick));
        }
        cache.lensesAreCurrent = true;

        // Run any queued actions.
        // This is usually nothing as the action list is empty.
        // We do this action queueing so that removing the codelens and inserting the typesignature
        // have the highest chance of happening at the exact same time.
        self.runActions(document);
        return cache.lenses;
      }
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
    // Who knows why it's here :/
    return codeLens;
  }
}

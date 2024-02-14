import * as vscode from "vscode";
import * as log from "./utils/log";
import * as JSONSafe from "./utils/json";
import * as Question from "./watchtower/question";
import { ElmProjectPane } from "./panel/panel";

import * as PanelMsg from "./panel/messages";
import * as EditorCoords from "./utils/editorCoords";
import * as SignatureCodeLens from "./watchtower/codelensProvider";
import * as ElmDevDiagnostics from "./watchtower/diagnostics";

var WebSocketClient = require("websocket").client;

type Msg =
  | { msg: "Discover"; details: { root: String; watching: Watching[] } }
  | { msg: "Changed"; details: { path: String } }
  | {
      msg: "Watched";
      details: Watching[];
    }
  | {
      msg: "EditorVisibilityChanged";
      details: { visible: PanelMsg.EditorVisibility[] };
    };

type Watching = { path: String; warnings: Boolean; docs: Boolean };

const discover = (roots: String): Msg => {
  const items = getWatchedItems();
  return { msg: "Discover", details: { root: roots, watching: items } };
};

const changed = (filepath: String): Msg => {
  return { msg: "Changed", details: { path: filepath } };
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

function getWatchedItems() {
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
  return items;
}

export class Watchtower {
  private connection;
  private editsSinceSave: vscode.TextDocumentChangeEvent[];
  private trackEditsSinceSave: boolean;

  // The core model of our Elm project
  private elmEditorVisibility: {
    visible: PanelMsg.EditorVisibility[];
  };
  private elmStatus: PanelMsg.ProjectStatus[];
  private elmWarninigs: { warnings: Question.Warning[]; filepath: string };

  // Providers and diagnostics
  public codelensProvider: SignatureCodeLens.SignatureCodeLensProvider;
  public diagnostics: ElmDevDiagnostics.WatchTowerDiagnostics;
  public definitionsProvider: vscode.DefinitionProvider;
  public statusbar: vscode.StatusBarItem;

  private send(msg: Msg) {
    if (this.connection?.connected) {
      log.log("SENDING: " + JSON.stringify(msg));
      this.connection.sendUTF(JSON.stringify(msg));
    } else {
      log.obj("SKIPPING (not connected)", msg);
      this.diagnostics.clear();
      this.codelensProvider.clear();
      this.connect();
    }
  }

  constructor() {
    let self = this;
    self.editsSinceSave = [];
    self.trackEditsSinceSave = false;
    self.codelensProvider = new SignatureCodeLens.SignatureCodeLensProvider();
    self.definitionsProvider = new ElmDefinitionProvider();
    self.diagnostics = new ElmDevDiagnostics.WatchTowerDiagnostics();

    self.statusbar = vscode.window.createStatusBarItem(
      vscode.StatusBarAlignment.Right,
      10
    );

    self.connect();

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
            const lineCountChange = EditorCoords.lineCountDelta(change);
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

    /*  Send editor visibility msgs to the Panel
        And sometimes to the watchtower
    */
    vscode.window.onDidChangeVisibleTextEditors((_) => {
      self.refreshWatching();
    });

    vscode.window.onDidChangeActiveTextEditor((editor) => {
      self.editorVisibilityUpdated(PanelMsg.sendEditorVisibility());
    });

    // vscode.window.onDidChangeTextEditorSelection((selection) => {
    //   // if (
    //   //   feature.explain &&
    //   //   selection.selections.length == 1 &&
    //   //   selection.selections[0].isEmpty &&
    //   //   selection.textEditor.document.fileName.endsWith(".elm")
    //   // ) {
    //   //   const point = selection.selections[0].anchor;
    //   //   self.askExplanation(
    //   //     selection.textEditor.document.fileName,
    //   //     point.line - 1,
    //   //     point.character - 1
    //   //   );
    //   // }
    //   // self.editorVisibilityUpdated(PanelMsg.sendEditorVisibility());
    // });

    // vscode.window.onDidChangeTextEditorVisibleRanges((visibleRanges) => {
    //   self.editorVisibilityUpdated(PanelMsg.sendEditorVisibility());
    // });
    // Send up an initial visibility message
    self.editorVisibilityUpdated(PanelMsg.sendEditorVisibility());
  }

  connect() {
    const self = this;
    if (this.connection?.connected) {
      return;
    }
    this.statusConnecting();

    socketConnect({
      url: Question.urls.websocket,
      onJoin: (connection) => {
        self.statusNoErrors();
        self.onJoin(connection);
      },
      onConnectionFailed: (err) => {
        self.onConnectionFailed(err);
      },
      receive: (msg) => {
        self.receive(msg);
      },
    });
  }

  private statusNoErrors() {
    this.statusbar.text = `Open Elm Dev`;
    this.statusbar.show();
    // @ts-ignore
    this.statusbar.backgroundColor = null;
    this.statusbar.command = "elm.projectPanel";
  }

  private statusConnecting() {
    this.statusbar.text = "Connecting to Elm Dev";
    this.statusbar.show();
    this.statusbar.command = null;
  }

  private statusUnableToConnect() {
    this.statusbar.text = "Unable to connect to Elm Dev";
    this.statusbar.show();

    // @ts-ignore
    this.statusbar.backgroundColor = new vscode.ThemeColor(
      "statusBarItem.errorBackground"
    );
  }

  private statusDanger(errorCount: number) {
    if (errorCount < 1) {
      return;
    }

    const plural = errorCount == 1 ? "" : "s";
    const message = `${errorCount} Elm error${plural}`;
    this.statusbar.text = message;
    this.statusbar.show();

    // @ts-ignore
    this.statusbar.backgroundColor = new vscode.ThemeColor(
      "statusBarItem.errorBackground"
    );
    this.statusbar.command = "elm.projectPanel";
  }

  private statusWarning(warningCount: number) {
    if (warningCount < 1) {
      return;
    }
    const plural = warningCount == 1 ? "" : "s";
    const message = `${warningCount} Elm suggestion${plural}`;
    this.statusbar.text = message;
    this.statusbar.show();
    this.statusbar.command = "elm.projectPanel";

    // @ts-ignore
    this.statusbar.backgroundColor = new vscode.ThemeColor(
      "statusBarItem.warningBackground"
    );
    this.statusbar.command = "elm.projectPanel";
  }

  private refreshWatching() {
    const items = getWatchedItems();
    if (items.length != 0) {
      this.send({ msg: "Watched", details: items });
    } else {
      this.statusNoErrors();
    }
  }

  private onConnectionFailed(error) {
    this.statusUnableToConnect();
  }

  private onJoin(connection) {
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
        self.elmStatus = msg.details;
        for (const project of msg["details"]) {
          if ("errors" in project.status) {
            for (const error of project.status["errors"]) {
              log.log("   ERROR:" + error.path);
              const uri = vscode.Uri.file(error["path"]);
              let problems: any[] = [];
              for (const prob of error["problems"]) {
                const fullRange = EditorCoords.regionToRange(prob["region"], {
                  uri: uri,
                  edits: self.editsSinceSave,
                });
                problems.push({
                  code: "elm-compiler",
                  title: prob["title"],
                  message: prob["title"], // formatMessage(prob["message"]),
                  fullRange: fullRange,
                  range: toSingleLine(fullRange),
                  severity: vscode.DiagnosticSeverity.Error,
                  source: "",
                  relatedInformation: [],
                });
              }
              self.statusDanger(problems.length);

              self.diagnostics.set(uri, problems);
            }
          } else if ("compiled" in project.status) {
            // success
            self.statusNoErrors();
            self.diagnostics.clear();
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
        self.elmWarninigs = msg.details;

        self.statusWarning(msg.details.warnings.length);
        self.codelensProvider.setSignaturesFromWarnings(
          msg.details.filepath,
          msg.details.warnings,
          self.editsSinceSave
        );
        break;
      }
      case "Docs": {
        log.log("New docs received!");

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

  // Panel Management
  showPanel(extensionPath) {
    const self = this;
    const msgs = [];
    if (self.elmStatus) {
      msgs.push(PanelMsg.status(self.elmStatus));
    }
    if (self.elmWarninigs) {
      msgs.push(PanelMsg.warnings(self.elmWarninigs));
    }
    if (self.elmEditorVisibility) {
      msgs.push(PanelMsg.visibility(self.elmEditorVisibility));
    }

    ElmProjectPane.createOrShow(extensionPath, msgs);
  }

  editorVisibilityUpdated(visibility: {
    msg: "EditorVisibilityChanged";
    details: { visible: PanelMsg.EditorVisibility[] };
  }) {
    this.elmEditorVisibility = visibility.details;
    ElmProjectPane.send(visibility);
    this.send(visibility);
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

function toSingleLine(range) {
  if (range.start.line == range.end.line) {
    return range;
  } else {
    return new vscode.Range(range.start.line, 0, range.start.line, 80);
  }
}

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

            const region = EditorCoords.regionToRange(
              resp.definition.region,
              null
            );

            resolve(new vscode.Location(uri, region));
          }
        },
        reject
      );
    });
  }
}

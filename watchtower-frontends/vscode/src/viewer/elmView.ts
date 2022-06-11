import * as vscode from "vscode";
import * as path from "path";
import * as script from "../utils/script";
import * as which from "which";
import * as log from "../utils/log";
import * as watchtower from "../watchtower";
var errorIcon = null;

/**
 * Manages Elm ErrorView webview panels
 */
export class ElmErrorPanel {
  /**
   * Track the currently panel. Only allow a single panel to exist at a time.
   */
  public static currentPanel: ElmErrorPanel | undefined;

  public static readonly viewType = "elmErrorView";

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionPath: string;
  private _watchtower: watchtower.Watchtower;
  private _disposables: vscode.Disposable[] = [];

  private constructor(panel: vscode.WebviewPanel, extensionPath: string) {
    this._watchtower = new watchtower.Watchtower();
    this._panel = panel;
    this._extensionPath = extensionPath;
    log.log("constructing webview");
    // Set the webview's initial html content
    this._update();

    // Listen for when the panel is disposed
    // This happens when the user closes the panel or when the panel is closed programatically
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    // Update the content based on view changes
    // this._panel.onDidChangeViewState(e => {
    //     //  NOTE: This resets all state, which ain't great.
    //     // if (this._panel.visible) {
    //     //     this._update()
    //     // }
    // }, null, this._disposables);

    // Handle messages from the webview
    this._panel.webview.onDidReceiveMessage(
      (message) => {
        // log.log(message);
        // switch (message.command) {
        //     case 'highlight':
        //         // vscode.window.showErrorMessage(message.text);
        //         console.log("Message found")
        //         console.log(message);
        //         return;
        // }
      },
      null,
      this._disposables
    );
  }

  public static createOrShow(extensionPath: string, icons) {
    let self = this;
    // If we already have a panel, show it.
    if (ElmErrorPanel.currentPanel) {
      log.log("showing existing panel");
      ElmErrorPanel.currentPanel._panel.reveal(vscode.ViewColumn.Two);
      return;
    }
    log.log("create panel");
    // Otherwise, create a new panel.
    const panel = vscode.window.createWebviewPanel(
      ElmErrorPanel.viewType,
      "Live Elm Errors",
      vscode.ViewColumn.Two,
      {
        // Enable javascript in the webview
        enableScripts: true,

        // And restric the webview to only loading content from our extension's `media` directory.
        localResourceRoots: [
          vscode.Uri.file(path.join(extensionPath, "media")),
        ],
      }
    );

    // **Note** not worried about workspace folders at the moment
    // vscode.workspace.onDidChangeWorkspaceFolders((editor) => {
    //   var workfolders = [];

    //   const folderCount = vscode.workspace.workspaceFolders.length;
    //   for (var i = 0; i < folderCount; i++) {
    //     workfolders.push({
    //       name: folderCount[i].name,
    //       path: folderCount[i].path,
    //     });
    //   }

    //   ElmErrorPanel.currentPanel.sendMessage({
    //     command: "RefreshWorkspaceFolders",
    //     folders: workfolders,
    //   });
    // });

    vscode.window.onDidChangeActiveTextEditor((editor) => {
      if (editor) {
        ElmErrorPanel.currentPanel.sendMessage({
          command: "CodeViewChanged",
          fileName: editor.document.fileName,
          ranges: prepareRanges(editor.visibleRanges),
          selections: editor.selections,
        });
      }
    });

    vscode.window.onDidChangeTextEditorVisibleRanges((visibleRanges) => {
      if (visibleRanges) {
        ElmErrorPanel.currentPanel.sendMessage({
          command: "CodeViewChanged",
          fileName: visibleRanges.textEditor.document.fileName,
          ranges: prepareRanges(visibleRanges.visibleRanges),
          selections: visibleRanges.textEditor.selections,
        });
      }
    });

    // Don't need selection right now, but we might!
    // vscode.window.onDidChangeTextEditorSelection((editor) => {
    //   if (editor) {
    //     ElmErrorPanel.currentPanel.sendMessage({
    //       command: "EditorSelection",
    //       fileName: editor.textEditor.document.fileName,
    //     //   selections: editor.selections,
    //       ranges: prepareRanges(editor.visibleRanges),
    //       selections:editor.selections ,
    //     });
    //   }
    // });

    const workspaceRoot = vscode.workspace.workspaceFolders[0].uri.path;

    let localErrorViewFiles = new vscode.RelativePattern(
      workspaceRoot,
      "**/*.{emu,elm}"
    );

    //  watch files and update errors if they've changed on disk
    let watcher = vscode.workspace.createFileSystemWatcher(
      localErrorViewFiles,
      false,
      false,
      false
    );
    watcher.onDidChange((event) => {
      if (!event.path.includes("elm-stuff")) {
        log.log("File changed");
        log.log(event.path);
        // let elmMake = which.sync("elm");

        const folders = vscode.workspace.workspaceFolders;

        const newFolders = [];
        const folderCount = vscode.workspace.workspaceFolders.length;
        for (var i = 0; i < folderCount; i++) {
          newFolders.push({ name: folders[i].name, path: folders[i].uri.path });
        }

        // ElmErrorPanel.currentPanel.sendMessage({
        //   command: "RefreshWorkspaceFolders",
        //   folders: newFolders,
        // });

        if (folders.length > 0) {
          const projectRoot = folders[0].uri.path;

          //ElmErrorPanel.currentPanel._watchtower.elmMake(
          //  projectRoot,
          //  event.path
          //);
          // script
          //   .executeExpectJson(elmMake, ["make", "--report=json", event.path], {
          //     cwd: projectRoot,
          //     env: process.env,
          //   })
          //   .then((json) => {
          //     ElmErrorPanel.currentPanel.sendMessage({
          //       command: "ElmMake",
          //       json: json,
          //     });
          //     ElmErrorPanel.currentPanel.highlight(json, icons);
          //   })
          //   .catch((err) => {
          //     log.log("ERRORORORORORO");
          //     log.log(err);
          //     var json = JSON.parse(err);

          //     ElmErrorPanel.currentPanel.sendMessage({
          //       command: "ElmMake",
          //       json: json,
          //     });
          //     ElmErrorPanel.currentPanel.highlight(json, icons);
          //   });
        }
      }
    });

    ElmErrorPanel.currentPanel = new ElmErrorPanel(panel, extensionPath);

    if (vscode.window.activeTextEditor) {
      ElmErrorPanel.currentPanel.sendMessage({
        command: "CodeViewChanged",
        fileName: vscode.window.activeTextEditor.document.fileName,
        ranges: prepareRanges(vscode.window.activeTextEditor.visibleRanges),
        selections: vscode.window.activeTextEditor.selections,
      });
    }

    let elmMake = which.sync("elm");

    const folders = vscode.workspace.workspaceFolders;

    if (folders.length > 0) {
      const projectRoot = folders[0].uri.path;
      const fileToCompile = vscode.window.activeTextEditor.document.fileName;

      // script
      //   .executeExpectJson(elmMake, ["make", "--report=json", fileToCompile], {
      //     cwd: projectRoot,
      //     env: process.env,
      //   })
      //   .then((json) => {
      //     ElmErrorPanel.currentPanel.sendMessage({
      //       command: "ElmMake",
      //       json: json,
      //     });
      //     ElmErrorPanel.currentPanel.highlight(json, icons);
      //   })
      //   .catch((err) => {
      //     var json = JSON.parse(err);

      //     ElmErrorPanel.currentPanel.sendMessage({
      //       command: "ElmMake",
      //       json: json,
      //     });
      //     ElmErrorPanel.currentPanel.highlight(json, icons);
      //   });
    }
  }

  public static revive(panel: vscode.WebviewPanel, extensionPath: string) {
    log.log("revive");
    ElmErrorPanel.currentPanel = new ElmErrorPanel(panel, extensionPath);
  }

  public sendMessage(event) {
    // log.log("MSG");
    // log.log(JSON.stringify(event));
    this._panel.webview.postMessage(event);
  }

  public highlight(errors, icons) {
    if (errors == null) {
      if (errorAreaHighlight != null) {
        errorAreaHighlight.dispose();
      }
    }
    toDiagnostics(errors.errors, icons);

    // collection.set(vscode.Uri.file(), [])
    // let ranges = findRanges(errors);
    // let decorationType = vscode.window.createTextEditorDecorationType({
    //     border: '1px dashed rgba(255,0,0,0.7)',
    //     borderStyle: 'none none dashed none'
    // });

    // if (vscode.window.activeTextEditor) {
    //     vscode.window.activeTextEditor.setDecorations(decorationType, ranges);
    // }
  }

  public dispose() {
    ElmErrorPanel.currentPanel = undefined;

    // Clean up our resources
    this._panel.dispose();

    while (this._disposables.length) {
      const x = this._disposables.pop();
      if (x) {
        x.dispose();
      }
    }
  }

  private _update() {
    this._panel.webview.html = this._getHtmlForWebview();
  }

  private _getHtmlForWebview() {
    // Local path to main script run in the webview
    const scriptPathOnDisk = vscode.Uri.file(
      path.join(this._extensionPath, "media", "elmViewer.js")
    );

    // And the uri we use to load this script in the webview
    const scriptUri = scriptPathOnDisk.with({ scheme: "vscode-resource" });

    // Use a nonce to whitelist which scripts can be run
    const nonce = getNonce();

    return `<!DOCTYPE html>
            <html lang="en">
            <head>
                <meta charset="UTF-8">

                <!--
                Use a content security policy to only allow loading images from https or from our extension directory,
                and only allow scripts that have a specific nonce.
                -->
                <meta http-equiv="Content-Security-Policy" content="default-src 'none'; connect-src ws:; img-src vscode-resource: https:; script-src 'nonce-${nonce}'; style-src 'unsafe-inline';">

                <meta name="viewport" content="width=device-width, initial-scale=1.0">
                <script nonce="${nonce}" src="${scriptUri}"></script>
                <title>Elm ErrorView Live View</title>
            </head>
            <body>
                <script nonce="${nonce}">
                    console.log("boot?!");
                    console.log(Elm);
                    console.log("Connect to websocket!");
                    var websocket = new WebSocket("ws://localhost:3030/metrics");
                    console.log("connected!");

                    const vscode = acquireVsCodeApi();
                    var pending = null;

                  
                    websocket.onopen = function (evt) {
                        // console.log("open", evt);
                        if (pending != null){
                          websocket.send(pending);
                          pending = null;
                        }
                        
                    };
                    // websocket.onclose = function (evt) {
                    //     console.log("close", evt);
                    // };


                    var app = Elm.Main.init();
                    websocket.onmessage = function (evt) {
                        console.log("Incoming");
                        
                        console.log(evt.data);
                        app.ports.editorChange.send(JSON.parse(evt.data));
                    };
                    
                    // websocket.onerror = function (evt) {
                    //     console.log("err", evt);
                    // };
                    // const oldState = vscode.getState();
                
                    // Handle messages sent from the extension to the webview
                    window.addEventListener('message', event => {
                        const message = event.data; // The json data that the extension sent
                        app.ports.editorChange.send(message);
                    });
                    
                    app.ports.notify.subscribe(function(message) {
                        console.log("Outgoing");
                        console.log(JSON.stringify(message));

                        // vscode.postMessage(message);
                        if (websocket.readyState == 1){
                          websocket.send(JSON.stringify(message));
                        } else {
                          pending = message
                        }
                        

                    });

                </script>
            </body>
            </html>`;
  }
}

function getNonce() {
  let text = "";
  const possible =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  for (let i = 0; i < 32; i++) {
    text += possible.charAt(Math.floor(Math.random() * possible.length));
  }
  return text;
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

var errorAreaHighlight = null;
function createErrorHighlight(icons) {
  // https://code.visualstudio.com/api/references/vscode-api#DecorationRenderOptions
  // https://code.visualstudio.com/api/references/icons-in-labels
  log.log(icons);
  var decoration = vscode.window.createTextEditorDecorationType({
    isWholeLine: true,
    gutterIconPath: icons.error,
    overviewRulerColor: "red",
    borderWidth: "0 0 0 4px",
    borderColor: "red",
    borderStyle: "solid",
    overviewRulerLane: vscode.OverviewRulerLane.Right,
    light: {
      // this color will be used in light color themes
      backgroundColor: "rgba(255,0,0,0.1)",
    },
    dark: {
      // this color will be used in dark color themes
      backgroundColor: "rgba(255,0,0,0.1)",
    },
  });

  return decoration;
}

/*  Create diagnostics  */

function toDiagnostics(errors, icons) {
  if (errorAreaHighlight === null) {
    errorAreaHighlight = createErrorHighlight(icons);
  } else {
    errorAreaHighlight.dispose();
    errorAreaHighlight = createErrorHighlight(icons);
  }

  // var editors = vscode.window.visibleTextEditors;
  // const editorCount = editors.length;

  // var activeEditor = vscode.window.activeTextEditor;
  var decorations = {};
  const rangeLength = errors.length;
  for (var i = 0; i < rangeLength; i++) {
    var ranges = [];
    const uri = vscode.Uri.file(errors[i].path);

    let diagnostics = [];
    const errLen = errors[i].problems.length;
    for (var j = 0; j < errLen; j++) {
      let current = errors[i].problems[j];

      let start = new vscode.Position(
        current.region.start.line - 1,
        current.region.start.column - 1
      );
      let end = new vscode.Position(
        current.region.end.line - 1,
        current.region.end.column
      );

      let newRange = new vscode.Range(start, end);
      let message = current.title;

      ranges.push(newRange);
      // let diag = new vscode.Diagnostic(newRange, message, vscode.DiagnosticSeverity.Error)
      // diagnostics.push(diag);
    }
    // activeEditor.setDecorations(errorAreaHighlight, ranges)
    decorations[errors[i].path] = ranges;
    // Diagnostics should only be shown for single line errors
    // collection.set(uri, diagnostics)
  }
  setEditorDecorations(errorAreaHighlight, decorations);
  return true;
}

function setEditorDecorations(highlighting, decorations) {
  var editors = vscode.window.visibleTextEditors;
  const editorCount = editors.length;
  for (var i = 0; i < editorCount; i++) {
    var editor = editors[i];

    if (editor.document.uri.path in decorations) {
      var decs = decorations[editor.document.uri.path];
      editor.setDecorations(highlighting, decs);
    }
  }
}

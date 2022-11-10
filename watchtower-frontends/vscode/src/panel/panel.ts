import * as vscode from "vscode";
import * as path from "path";
import * as log from "../utils/log";
import * as Message from "./messages";

export class ElmProjectPane {
  /**
   * Track the current panel. Only allow a single panel to exist at a time.
   */
  public static currentPanel: ElmProjectPane | undefined;

  public static readonly viewType = "elmProjectView";

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionPath: string;

  private _disposables: vscode.Disposable[] = [];

  private constructor(panel: vscode.WebviewPanel, extensionPath: string) {
    this._panel = panel;
    this._extensionPath = extensionPath;
    // Set the webview's initial html content
    this._update();

    // Listen for when the panel is disposed
    // This happens when the user closes the panel or when the panel is closed programatically
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    // Handle messages from the webview
    this._panel.webview.onDidReceiveMessage(
      (message) => {
        switch (message.msg) {
          case "Jump":
            let jumped = false;
            const start = new vscode.Position(
              message.details.region.start.line,
              message.details.region.start.column
            );
            const end = new vscode.Position(
              message.details.region.end.line,
              message.details.region.end.column
            );
            const targetRange = new vscode.Range(start, end);
            for (const editorIndex in vscode.window.visibleTextEditors) {
              const editor = vscode.window.visibleTextEditors[editorIndex];
              if (editor.document.uri.path == message.details.path) {
                editor.revealRange(
                  targetRange,
                  vscode.TextEditorRevealType.InCenterIfOutsideViewport
                );
                jumped = true;
                break;
              }
            }
            if (!jumped) {
              const uri = vscode.Uri.file(message.details.path);
              vscode.window
                .showTextDocument(uri, { viewColumn: vscode.ViewColumn.One })
                .then((newEditor) => {
                  newEditor.revealRange(
                    targetRange,
                    vscode.TextEditorRevealType.InCenterIfOutsideViewport
                  );
                });
            }

            break;

          default:
            log.log("Unrecognized msg from elm panel");
            log.obj("WEB-RECEIVED", message);
            break;
        }
      },
      null,
      this._disposables
    );
  }

  public static createOrShow(extensionPath: string) {
    // If we already have a panel, show it.
    if (ElmProjectPane.currentPanel) {
      ElmProjectPane.currentPanel._panel.reveal(vscode.ViewColumn.Two);
      return;
    }
    // Otherwise, create a new panel.
    const panel = vscode.window.createWebviewPanel(
      ElmProjectPane.viewType,
      "Elm Project",
      vscode.ViewColumn.Two,
      {
        // Enable javascript in the webview
        enableScripts: true,

        // restrict the webview to only loading content
        // from our extension's `media` directory.
        localResourceRoots: [
          vscode.Uri.file(path.join(extensionPath, "media")),
        ],
      }
    );

    ElmProjectPane.currentPanel = new ElmProjectPane(panel, extensionPath);
  }

  public static revive(panel: vscode.WebviewPanel, extensionPath: string) {
    ElmProjectPane.currentPanel = new ElmProjectPane(panel, extensionPath);
  }

  public static send(msg: Message.ToProjectPanel) {
    if (ElmProjectPane.currentPanel) {
      ElmProjectPane.currentPanel._panel.webview.postMessage(msg);
    } else {
      // no panel, dropping msg
      // log.log("No panel, dropping msg");
    }
  }

  public dispose() {
    ElmProjectPane.currentPanel = undefined;

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
      path.join(this._extensionPath, "media", "panel.js")
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
                  <meta http-equiv="Content-Security-Policy" content="default-src 'none'; 
                    connect-src ws:;
                    img-src vscode-resource: https:;
                    script-src 'nonce-${nonce}' 'unsafe-eval';
                    style-src 'unsafe-inline';" 
                  />
  
                  <meta name="viewport" content="width=device-width, initial-scale=1.0">
                  <script nonce="${nonce}" src="${scriptUri}"></script>
                  <title>Elm Live Project</title>
              </head>
              <body>
                  <div style="position:fixed; left:0; top:0; width: 100vw;height:100vw;">
                    <div id="main" style="width: 100vw;height:100vw;"></div>
                  </div>
                  <!-- The second div is our interactive App, which can be reloaded arbitrarily -->
                  <div id="live" style="display:none; position:fixed; left:0; top:0; width: 100vw;height:100vw;">
                    <div id="interactive"></div>
                  </div>
                  <script nonce="${nonce}">

                      var interactive_app = {}
                      
                      function scoped_eval(scope, script) {
                        return Function('"use strict";' + script).bind(scope)();
                      }

                      function load_interactive(node, js_string) {
                        // Kill existing Elm App
                        if ("Elm" in interactive_app) {
                          console.log("Killing live app")
                          interactive_app.app.ports.unmount.send(null)
                          interactive_app = {}
                        }
                        console.log("NEW APP")
                        const evaled = scoped_eval(interactive_app, js_string)
                        var instance = interactive_app.Elm.Live.init({node: node, flags: {}})
                        interactive_app.app = instance
                       
                      }            

                      const vscode = acquireVsCodeApi();
                      
                      var app = Elm.Main.init({node: document.getElementById('main')});
                      

                      // Handle messages sent from the extension to the webview
                      window.addEventListener('message', event => {
                          // if (event.data.msg == "InteractiveCodeRefreshed") {
                          //   console.log("LIVE JS RECEIVED")
                          //   load_interactive(document.getElementById('live').firstChild, event.data.details.js)  
                          // } else {
                          app.ports.toElm.send(event.data);
                          // }
                      });
                      
                      app.ports.toWorld.subscribe(function(message) {
                          vscode.postMessage(message);
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

//In charge of reviving a webview when a workspace is reladed
export class ElmProjectSerializer implements vscode.WebviewPanelSerializer {
  private readonly _extensionPath: string;
  constructor(extensionPath: string) {
    this._extensionPath = extensionPath;
  }
  async deserializeWebviewPanel(webviewPanel: vscode.WebviewPanel, state: any) {
    // `state` is the state persisted using `setState` inside the webview
    console.log(`Deserializing panel state: ${state}`);

    // Restore the content of our webview.
    //
    // Make sure we hold on to the `webviewPanel` passed in here and
    // also restore any event listeners we need on it.
    ElmProjectPane.revive(webviewPanel, this._extensionPath);
  }
}

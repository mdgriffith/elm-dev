import * as vscode from "vscode";
import * as path from "path";
import * as log from "../utils/log";
import * as watchtower from "../watchtower";

export class ElmProjectPane {
  /**
   * Track the current panel. Only allow a single panel to exist at a time.
   */
  public static currentPanel: ElmProjectPane | undefined;

  public static readonly viewType = "elmProjectView";

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

    // Handle messages from the webview
    this._panel.webview.onDidReceiveMessage(
      (message) => {
        log.log(message);
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

  public static createOrShow(extensionPath: string) {
    let self = this;
    // If we already have a panel, show it.
    if (ElmProjectPane.currentPanel) {
      log.log("showing existing panel");
      ElmProjectPane.currentPanel._panel.reveal(vscode.ViewColumn.Two);
      return;
    }
    log.log("create panel");
    // Otherwise, create a new panel.
    const panel = vscode.window.createWebviewPanel(
      ElmProjectPane.viewType,
      "Elm Project",
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

    ElmProjectPane.currentPanel = new ElmProjectPane(panel, extensionPath);
  }

  public static revive(panel: vscode.WebviewPanel, extensionPath: string) {
    log.log("revive");
    ElmProjectPane.currentPanel = new ElmProjectPane(panel, extensionPath);
  }

  public sendMessage(event) {
    // log.log("MSG");
    // log.log(JSON.stringify(event));
    this._panel.webview.postMessage(event);
  }

  public dispose() {
    log.log("DISPOSING WEBVIEW");
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
      path.join(this._extensionPath, "media", "test.js")
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
                  <title>Elm Project</title>
              </head>
              <body>
                  <div> Hello! </div>
                  <script nonce="${nonce}">
                      console.log("boot?!");
                    
                      const vscode = acquireVsCodeApi();
                      
                      console.log("Hello!")
  
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
    console.log(`Got state: ${state}`);

    // Restore the content of our webview.
    //
    // Make sure we hold on to the `webviewPanel` passed in here and
    // also restore any event listeners we need on it.
    ElmProjectPane.revive(webviewPanel, this._extensionPath);
  }
}




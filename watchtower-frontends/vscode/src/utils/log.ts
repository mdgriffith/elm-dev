import * as vscode from "vscode";

const watchtower = vscode.window.createOutputChannel("Elm Watchtower");

export function log(str) {
  watchtower.appendLine(str);
}

export function obj(o) {
  for (const [key, value] of Object.entries(o)) {
    watchtower.appendLine("  " + key + ": " + value);
  }
}

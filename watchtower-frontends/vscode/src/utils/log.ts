import * as vscode from "vscode";

const watchtower = vscode.window.createOutputChannel("Elm Watchtower");

export function log(str) {
  watchtower.appendLine(str);
}

export function obj(name, o) {
  watchtower.appendLine(name);
  for (const [key, value] of Object.entries(o)) {

    if (Array.isArray(value)) {
      let first = true
      watchtower.appendLine("  " + key);
      if (value.length == 0) {
        watchtower.appendLine("    []");
      } else {
        for (const subval in value) {
          if (first) {
            watchtower.appendLine("    [ " + subval);
            first = false
          } else {
            watchtower.appendLine("    , " + subval);
          }
          
        }
        watchtower.appendLine("    ]");
      }
    } else if (
      typeof value === 'object' &&
      value !== null
    ) {
      watchtower.appendLine("  " + key + ": ");
      watchtower.appendLine(Object.keys(value).join("\n"));
      for (const [subKey, subValue] of Object.entries(value)) {
        watchtower.appendLine("    " + subKey + ": " + subValue);
      }

    } else {
      watchtower.appendLine("  " + key + ": " + value);
    }
    
  }
}

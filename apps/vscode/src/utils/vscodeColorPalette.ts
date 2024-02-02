import * as vscode from "vscode";

export function register(styleItems) {
  vscode.commands.registerCommand("elm.vscodeColors", () => {
    if (vscode.window.activeTextEditor) {
      const selection = vscode.window.activeTextEditor.selection;
      if (selection) {
        vscode.window.activeTextEditor.edit((editor) => {
          const vscodeStylesheet = [];

          const styleMap = new Map();

          for (const [key, value] of Object.entries(styleItems.properties)) {
            const colorName = key.replace(/\./g, "-");
            const names = key.split(".");
            let namespace = "global";
            let name = key.replace(/\./g, "_");

            if (names.length == 1) {
              name = names[0];
            } else if (names.length == 2) {
              namespace = names[0];
              name = names[1];
            } else {
              namespace = names[0];
              names.shift();
              name = names.join("_");
            }

            const existing = styleMap.get(namespace);
            if (existing) {
              existing.push([name, colorName]);
            } else {
              styleMap.set(namespace, [[name, colorName]]);
            }

            if (key.includes("oreground")) {
              vscodeStylesheet.push(
                `.${colorName} { color: var(--vscode-${colorName}) !important; }`
              );
            } else if (key.includes("ackground")) {
              vscodeStylesheet.push(
                `.${colorName} { background-color:  var(--vscode-${colorName}) !important; }`
              );
            } else if (key.includes("order")) {
              vscodeStylesheet.push(
                `.${colorName} { border-color:  var(--vscode-${colorName}) !important; }`
              );
            }
          }

          let elmStyleSheet = `module VSCode.Colors exposing (..)

{-| This file is generated via a command from elm-dev!
-}


import Element
import Html.Attributes as Attr

`;

          elmStyleSheet += `stylesheet : String\nstylesheet = """${vscodeStylesheet.join(
            ""
          )}"""`;

          elmStyleSheet += `\n\n\n`;

          for (const [namespace, names] of styleMap.entries()) {
            elmStyleSheet += `\n\n${namespace} =`;

            let first = true;
            for (const i in names) {
              if (first) {
                elmStyleSheet += `\n    { `;
              } else {
                elmStyleSheet += `\n    , `;
              }
              elmStyleSheet += `${names[i][0]} = Element.htmlAttribute (Attr.class "${names[i][1]}")`;

              first = false;
            }
            elmStyleSheet += "\n    }";
          }

          editor.insert(selection.start, elmStyleSheet);
        });
      }
    }
  });
}

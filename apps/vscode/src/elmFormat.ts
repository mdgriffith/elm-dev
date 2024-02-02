import * as vscode from "vscode";
import * as utils from "./utils/elmUtils";
import * as path from "path";
import { Range, TextEdit } from "vscode";
import { execCmd } from "./utils/elmUtils";

export class ElmFormatProvider
  implements vscode.DocumentFormattingEditProvider
{
  provideDocumentFormattingEdits(
    document: vscode.TextDocument,
    options?: vscode.FormattingOptions,
    token?: vscode.CancellationToken
  ): Thenable<TextEdit[]> {
    return elmFormat(document).then(({ stdout }) => {
      const lastLineId = document.lineCount - 1;
      const wholeDocument = new Range(
        0,
        0,
        lastLineId,
        document.lineAt(lastLineId).text.length
      );
      return [TextEdit.replace(wholeDocument, stdout)];
    });
  }
}

export class ElmRangeFormatProvider
  implements vscode.DocumentRangeFormattingEditProvider
{
  /*
    Formatting range is the same as formatting whole document,
    rather than user's current selection.
    */
  provideDocumentRangeFormattingEdits(
    document: vscode.TextDocument,
    range: vscode.Range,
    options?: vscode.FormattingOptions,
    token?: vscode.CancellationToken
  ): Thenable<TextEdit[]> {
    return elmFormat(document).then(({ stdout }) => {
      const lastLineId = document.lineCount - 1;
      const wholeDocument = new Range(
        0,
        0,
        lastLineId,
        document.lineAt(lastLineId).text.length
      );
      return [TextEdit.replace(wholeDocument, stdout)];
    });
  }
}

function elmFormat(document: vscode.TextDocument) {
  // Use the elm-format that is installed with this package
  const formatCommand: string = path.join(__dirname, "elm-format");
  const dummyPath = path.join(vscode.workspace.rootPath, "dummyfile");
  const [_, elmVersion] = utils.detectProjectRootAndElmVersion(
    dummyPath,
    vscode.workspace.rootPath
  );
  const args = utils.isElm019(elmVersion)
    ? ["--stdin", "--elm-version 0.19", "--yes"]
    : ["--stdin", "--elm-version 0.18", "--yes"];
  const options = {
    cmdArguments: args,
    notFoundText: "Install Elm-format from https://github.com/avh4/elm-format",
  };
  const format = execCmd(formatCommand, options);

  format.stdin.write(document.getText());
  format.stdin.end();

  return format;
}

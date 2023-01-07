import * as vscode from "vscode";

export function lineCountDelta(edit) {
  const oldLineCount = edit.range.end.line - edit.range.start.line;
  const newLineCount = (edit.text.match(/\n/g) || "").length;
  return newLineCount - oldLineCount;
}

/* Note: If this ever gets an invalid region, e.g. a line or column of 0,
 * then this will eventually hang forever. and cause regions to not display.
 */
export function regionToRange(
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

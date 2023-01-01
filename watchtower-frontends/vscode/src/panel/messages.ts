import * as Code from "vscode";
import * as Question from "../watchtower/question";

//
export type ToProjectPanel =
  | {
      msg: "EditorVisibilityChanged";
      details: { active: EditorVisibility | null; visible: EditorVisibility[] };
    }
  | { msg: "Status"; details: ProjectStatus[] }
  | {
      msg: "Warnings";
      details: { warnings: Question.Warning[]; filepath: string };
    }
  | {
      msg: "CallGraph";
      details: { filepath: string; callgraph: CallGraphNode[]; };
    }
  | {
      msg: "Explanation";
      details: { filepath: string; explanation: Explanation; };
    }
  | {
      msg: "InteractiveCodeRefreshed";
      details: { js: string };
    };

export type Explanation = {
  definition: {name : string, type: string | null, recursive: Boolean, region: Range}
  facts: Fact[]
}

export type EditorVisibility = {
  fileName: string;
  ranges: Range[];
  selections: Selection[];
  unsavedChanges: Boolean;
};

export type ProjectStatus = {
  root: string;
  status: any;
};

export type Selection = {
  start: Position;
  end: Position;
  cursor: Position;
};

export type Range = {
  start: Position;
  end: Position;
};

export type Position = {
  line: number;
  column: number;
};


// CallGraph

export type CallGraphNode = {
  id: string;
  recursive: boolean;
  calls: Call[];
}

export type Call = {
  id: string
  callType: CallType
}

export type CallType = 
    "top-level"  | "local" | "foreign" 
      | "constructor" | "debug" | "operator"



// Explanation

export type Fact = {
  module: Module,
  name: string,
  type: string
}

export type Module = {
  pkg: string
  module: string
}


// Focus clicked:
//     like when a type error is clicked
//     Or a card in the autodoc.
export type FromProjectPanel = {
  msg: "FocusClicked";
  fileName: string;
  range: Range;
};

/* Message constructors */

export const interactiveCodeRefreshed = (js: string): ToProjectPanel => {
  return {
    msg: "InteractiveCodeRefreshed",
    details: { js: js },
  };
};

export const sendEditorVisibility = (): ToProjectPanel => {
  let active = null;
  if (Code.window.activeTextEditor) {
    active = prepareVisibility(Code.window.activeTextEditor);
  }
  const visible = [];
  for (const index in Code.window.visibleTextEditors) {
    visible.push(prepareVisibility(Code.window.visibleTextEditors[index]));
  }

  return {
    msg: "EditorVisibilityChanged",
    details: {
      active: active,
      visible: visible,
    },
  };
};

const prepareVisibility = (editor: Code.TextEditor): EditorVisibility => {
  return {
    fileName: editor.document.fileName,
    ranges: prepareRanges(editor.visibleRanges),
    selections: prepareSelections(editor.selections),
    unsavedChanges: editor.document.isDirty,
  };
};

/* Helpers */
function prepareSelections(selections: readonly Code.Selection[]): Selection[] {
  const len = selections.length;

  var prepared = [];
  for (var i = 0; i < len; i++) {
    const selection = selections[i];
    const start = {
      character: selection.start.character,
      line: selection.start.line + 1,
    };
    const end = {
      character: selection.end.character,
      line: selection.end.line + 1,
    };
    let cursorPosition = end;
    if (selection.isReversed) {
      cursorPosition = start;
    }

    prepared.push({ start: start, end: end, cursor: cursorPosition });
  }
  return prepared;
}

function prepareRanges(ranges: readonly Code.Range[]): Range[] {
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


export function status(details: ProjectStatus[]): ToProjectPanel {
  return { msg: "Status", details: details }
}

export function warnings(details: { warnings: Question.Warning[]; filepath: string }): ToProjectPanel {
  return { msg: "Warnings", details: details }
}

export function visibility(details: { active: EditorVisibility | null; visible: EditorVisibility[] }): ToProjectPanel {
  return { msg: "EditorVisibilityChanged", details: details }
}

export function callgraph(details: { filepath: string; callgraph: CallGraphNode[]; }): ToProjectPanel {
  return { msg: "CallGraph", details: details }
}

export function explanation(details: { filepath: string; explanation: Explanation; }): ToProjectPanel {
  return { msg: "Explanation", details: details }
}
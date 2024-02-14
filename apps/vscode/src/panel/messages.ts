import * as Code from "vscode";
import * as Question from "../watchtower/question";

//
export type ToProjectPanel =
  | {
      msg: "EditorVisibilityChanged";
      details: { visible: EditorVisibility[] };
    }
  | { msg: "Status"; details: ProjectStatus[] }
  | {
      msg: "Warnings";
      details: { warnings: Question.Warning[]; filepath: string };
    }
  | {
      msg: "CallGraph";
      details: { filepath: string; callgraph: CallGraphNode[] };
    }
  | {
      msg: "Explanation";
      details: { filepath: string; explanation: Explanation };
    }
  | {
      msg: "InteractiveCodeRefreshed";
      details: { js: string };
    };

export type Explanation = {
  definition: {
    name: string;
    type: string | null;
    recursive: Boolean;
    region: Range;
  };
  facts: Fact[];
};

export type EditorVisibility = {
  filepath: string;
  regions: Range[];
  // selections: Selection[];
  active: Boolean;
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
};

export type Call = {
  id: string;
  callType: CallType;
};

export type CallType =
  | "top-level"
  | "local"
  | "foreign"
  | "constructor"
  | "debug"
  | "operator";

// Explanation

export type Fact =
  | {
      module: Module;
      name: string;
      type: string;
    }
  | { union: Union }
  | { alias: Alias }
  | { definition: Definition };

export type Module = {
  pkg: string;
  module: string;
};

export type Union = {
  name: string;
  args: Arg[];
  comment: string | null;
  cases: [string, ElmType][];
};

export type Alias = {
  name: string;
  args: Arg[];
  comment: string | null;
  type: ElmType;
};

export type Definition = {
  comment: string;
  type: ElmType | null;
};

export type Arg = {
  name: string;
  type: ElmType;
};

type ElmType = any;

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

export const sendEditorVisibility = (): {
  msg: "EditorVisibilityChanged";
  details: { visible: EditorVisibility[] };
} => {
  const visible = [];
  for (const index in Code.window.visibleTextEditors) {
    visible.push(prepareVisibility(Code.window.visibleTextEditors[index]));
  }

  return {
    msg: "EditorVisibilityChanged",
    details: {
      visible: visible,
    },
  };
};

const prepareVisibility = (editor: Code.TextEditor): EditorVisibility => {
  let isActive = false;
  if (Code.window.activeTextEditor) {
    isActive =
      editor.document.fileName ===
      Code.window.activeTextEditor.document.fileName;
  }

  return {
    filepath: editor.document.fileName,
    regions: prepareRanges(editor.visibleRanges),
    // selections: prepareSelections(editor.selections),
    unsavedChanges: editor.document.isDirty,
    active: isActive,
  };
};

/* Helpers */
function prepareSelections(selections: readonly Code.Selection[]): Selection[] {
  const len = selections.length;

  var prepared: Selection[] = [];
  for (var i = 0; i < len; i++) {
    const selection = selections[i];
    const start = {
      column: selection.start.character,
      line: selection.start.line + 1,
    };
    const end = {
      column: selection.end.character,
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

  var prepared: Range[] = [];
  for (var i = 0; i < rangeLength; i++) {
    const start = {
      column: ranges[i].start.character,
      line: ranges[i].start.line,
    };
    const end = {
      column: ranges[i].end.character,
      line: ranges[i].end.line,
    };
    prepared.push({ start: start, end: end });
  }
  return prepared;
}

export function status(details: ProjectStatus[]): ToProjectPanel {
  return { msg: "Status", details: details };
}

export function warnings(details: {
  warnings: Question.Warning[];
  filepath: string;
}): ToProjectPanel {
  return { msg: "Warnings", details: details };
}

export function visibility(details: {
  visible: EditorVisibility[];
}): ToProjectPanel {
  return { msg: "EditorVisibilityChanged", details: details };
}

export function callgraph(details: {
  filepath: string;
  callgraph: CallGraphNode[];
}): ToProjectPanel {
  return { msg: "CallGraph", details: details };
}

export function explanation(details: {
  filepath: string;
  explanation: Explanation;
}): ToProjectPanel {
  return { msg: "Explanation", details: details };
}

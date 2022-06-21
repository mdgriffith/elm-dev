//
export type ToProjectPanel =
  | {
      msg: "VisibleRangesUpdated";
      fileName: string;
      ranges: Range[];
      selections: Selection[];
    }
  | { msg: "Status" ,
      details: ProjectStatus[]
    }

export type ProjectStatus = {
    root: string
    status: any
}

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

// Focus clicked:
//     like when a type error is clicked
//     Or a card in the autodoc.
export type FromProjectPanel = {
  msg: "FocusClicked";
  fileName: string;
  range: Range;
};

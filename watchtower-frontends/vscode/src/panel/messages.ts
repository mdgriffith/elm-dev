//
type ToProjectPanel =
  | {
      msg: "VisibleRangesUpdated";
      fileName: string;
      ranges: Range[];
      selections: Selection[];
    }
  | { msg: "Status" ,
      details: ProjectStatus[]
    }

type ProjectStatus = {
    root: string
    status: any
}

type Selection = {
  start: Position;
  end: Position;
  cursor: Position;
};

type Range = {
  start: Position;
  end: Position;
};

type Position = {
  line: number;
  column: number;
};

// Focus clicked:
//     like when a type error is clicked
//     Or a card in the autodoc.
type FromProjectPanel = {
  msg: "FocusClicked";
  fileName: string;
  range: Range;
};

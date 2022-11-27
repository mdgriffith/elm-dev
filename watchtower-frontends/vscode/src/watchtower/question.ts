import * as log from "../utils/log";
import * as http from "http";
import * as JSONSafe from "../utils/json";

export type MissingSignature = {
  filepath: String;
  name: String;
  region: Region;
  signature: String;
};

export type Warning =
  | { warning: "UnusedVariable"; region: Region; context: string; name: string }
  | {
      warning: "MissingAnnotation";
      region: Region;
      name: String;
      signature: String;
    }
  | {
      warning: "UnusedImport";
      region: Region;
      name: String;
    };

type Region = {
  start: Position;
  end: Position;
};

type Position = {
  line: number;
  column: number;
};

type Question =
  | { msg: "Discover"; directory: String }
  | { msg: "ServerHealth" }
  | { msg: "Warnings"; filepath: String }
  | { msg: "FindDefinition"; filepath: string; line: number; char: number }
  | {
      msg: "FindInstances";
      filepath: string;
      line: number;
      char: number;
      includeDeclaration: boolean;
    };

const serverHealth: Question = {
  msg: "ServerHealth",
};

export const questions = {
  serverHealth: serverHealth,
  discover: (dir: String): Question => {
    return {
      msg: "Discover",
      directory: dir,
    };
  },
  warnings: (filepath: String): Question => {
    return {
      msg: "Warnings",
      filepath: filepath,
    };
  },
  findDefinition: (filepath: string, line: number, char: number): Question => {
    return {
      msg: "FindDefinition",
      filepath: filepath,
      line: line,
      char: char,
    };
  },
  findInstances: (
    filepath: string,
    line: number,
    char: number,
    includeDeclaration: boolean
  ): Question => {
    return {
      msg: "FindInstances",
      filepath: filepath,
      line: line,
      char: char,
      includeDeclaration,
    };
  },
};

export const port = "51213";
const domain = `localhost:${port}`;

export const urls = {
  websocket: "ws://" + domain + "/ws",
  question: (path) => {
    return "http://" + domain + path;
  },
};

export const ask = (question: Question, onSuccess: any, onError: any) => {
  switch (question.msg) {
    case "ServerHealth": {
      http
        .get(urls.question("/health"), captureRequest(onSuccess))
        .on("error", onError);

      break;
    }
    case "Discover": {
      http
        .get(
          urls.question("/discover?dir=" + question.directory),
          captureRequest(onSuccess)
        )
        .on("error", (err) => {
          log.log("Error on discovery");
          log.log(err);
          onError(err);
        });

      break;
    }
    case "Warnings": {
      http
        .get(
          urls.question(`/warnings?file=${question.filepath}`),
          captureRequest(onSuccess)
        )
        .on("error", (err) => {
          log.log("Error on requesting warnings");
          log.log(err);
          onError(err);
        });
      break;
    }
    case "FindDefinition": {
      http
        .get(
          urls.question(
            `/definition?file=${question.filepath}&char=${question.char}&line=${question.line}`
          ),
          captureRequest(onSuccess)
        )
        .on("error", (err) => {
          log.log("Error on finding definition");
          log.log(err);
          onError(err);
        });
      break;
    }
    case "FindInstances":
      http
        .get(
          urls.question(
            `/instances?file=${question.filepath}&char=${question.char}&line=${question.line}&decl=${question.includeDeclaration}`
          ),
          captureRequest(onSuccess)
        )
        .on("error", (err) => {
          log.log("Error on finding instances");
          log.log(err);
          onError(err);
        });
      break;
    default: {
      log.log("wut?");
    }
  }
};

const captureRequest = (onCapture: any) => {
  return (response) => {
    var str = "";

    //another chunk of data has been received, so append it to `str`
    response.on("data", function (chunk) {
      str += chunk;
    });

    //the whole response has been received, so we just print it out here
    response.on("end", function () {
      onCapture(JSONSafe.parse(str));
    });
  };
};

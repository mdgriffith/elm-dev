import * as log from "../utils/log";
import * as http from "http";
import * as JSONSafe from "../utils/json";

export type MissingSignature = {
  filepath: String;
  name: String;
  region: Region;
  signature: String;
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
  | { msg: "ListMissingSignaturesPlease"; filepath: String }
  | { msg: "FindDefinition"; filepath: string; line: number; char: number };

export const questions = {
  discover: (dir: String): Question => {
    return {
      msg: "Discover",
      directory: dir,
    };
  },
  listMissingSignatures: (filepath: String): Question => {
    return {
      msg: "ListMissingSignaturesPlease",
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
};

export const port = "4747";
const domain = `localhost:${port}`;

export const urls = {
  websocket: "ws://" + domain + "/ws",
  question: (path) => {
    return "http://" + domain + path;
  },
};

export const ask = (question: Question, callback: any) => {
  switch (question.msg) {
    case "Discover": {
      http
        .get(
          urls.question("/discover?dir=" + question.directory),
          captureRequest(callback)
        )
        .on("error", (err) => {
          log.log("Error on discovery");
          log.log(err);
        });

      break;
    }
    case "ListMissingSignaturesPlease": {
      http
        .get(
          urls.question(`/list-missing-signatures?file=${question.filepath}`),
          captureRequest(callback)
        )
        .on("error", (err) => {
          log.log("Error on listing signatures");
          log.log(err);
        });
      break;
    }
    case "FindDefinition": {
      http
        .get(
          urls.question(
            `/definition?file=${question.filepath}&char=${question.char}&line=${question.line}`
          ),
          captureRequest(callback)
        )
        .on("error", (err) => {
          log.log("Error on finding definition");
          log.log(err);
        });
      break;
    }
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

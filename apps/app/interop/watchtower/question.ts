// Type definitions and functions for communicating with the Elm Dev Server.
//

import * as log from "../utils/log";
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
  | { msg: "CallGraph"; filepath: String }
  | { msg: "FindDefinition"; filepath: string; line: number; char: number }
  | { msg: "Explain"; filepath: string; line: number; char: number };

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
  callgraph: (filepath: String): Question => {
    return {
      msg: "CallGraph",
      filepath: filepath,
    };
  },
  explain: (filepath: string, line: number, char: number): Question => {
    return {
      msg: "Explain",
      filepath: filepath,
      line: line,
      char: char,
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
};

type SendGetFunction = (
  url: string,
  onSuccess: (data: any) => void,
  onError: (error: Error) => void
) => void;

// Class for tracking the server.
export class ElmDev {
  // Define the fields with types
  private port: string;
  private domain: string;
  private sendGet: SendGetFunction;

  // Constructor to initialize the fields
  constructor(port: string, domain: string, sendGet: SendGetFunction) {
    this.port = port;
    this.domain = domain;
    this.sendGet = sendGet;
  }

  get(
    path: string,
    onSuccess: (data: any) => void,
    onError: (error: Error) => void
  ) {
    return this.sendGet(this.questionUrl(path), onSuccess, onError);
  }

  questionUrl(path: string): string {
    return `${this.domain}:${this.port}${path}`;
  }

  websocketUrl(): string {
    return "ws://" + this.domain + "/ws";
  }

  // Method to display information about the instance
  getInfo(): string {
    return `Domain: ${this.domain}, Port: ${this.port}`;
  }

  ask(question: Question, onSuccess: any, onError: any) {
    switch (question.msg) {
      case "ServerHealth": {
        this.get("/health", onSuccess, onError);

        break;
      }
      case "Discover": {
        this.get("/discover?dir=" + question.directory, onSuccess, onError);

        break;
      }
      case "Warnings": {
        this.get(`/warnings?file=${question.filepath}`, onSuccess, onError);

        break;
      }
      case "FindDefinition": {
        this.get(
          `/definition?file=${question.filepath}&char=${question.char}&line=${question.line}`,
          onSuccess,
          onError
        );

        break;
      }
      case "Explain": {
        this.get(
          `/explain?file=${question.filepath}&char=${question.char}&line=${question.line}`,
          onSuccess,
          onError
        );

        break;
      }
      case "CallGraph": {
        this.get(`/callgraph?file=${question.filepath}`, onSuccess, onError);
        break;
      }
      default: {
        log.log("wut?");
      }
    }
  }
}

// export const port = "51213";
// const domain = `localhost:${port}`;

// export const urls = {
//   websocket: "ws://" + domain + "/ws",
//   question: (path) => {
//     return "http://" + domain + path;
//   },
// };

// export const ask = (question: Question, onSuccess: any, onError: any) => {
//   switch (question.msg) {
//     case "ServerHealth": {
//       http
//         .get(urls.question("/health"), captureRequest(onSuccess))
//         .on("error", onError);

//       break;
//     }
//     case "Discover": {
//       http
//         .get(
//           urls.question("/discover?dir=" + question.directory),
//           captureRequest(onSuccess)
//         )
//         .on("error", (err) => {
//           log.log("Error on discovery");
//           log.log(err);
//           onError(err);
//         });

//       break;
//     }
//     case "Warnings": {
//       http
//         .get(
//           urls.question(`/warnings?file=${question.filepath}`),
//           captureRequest(onSuccess)
//         )
//         .on("error", (err) => {
//           log.log("Error on requesting warnings");
//           log.log(err);
//           onError(err);
//         });
//       break;
//     }
//     case "FindDefinition": {
//       http
//         .get(
//           urls.question(
//             `/definition?file=${question.filepath}&char=${question.char}&line=${question.line}`
//           ),
//           captureRequest(onSuccess)
//         )
//         .on("error", (err) => {
//           log.log("Error on finding definition");
//           log.log(err);
//           onError(err);
//         });
//       break;
//     }
//     case "Explain": {
//       http
//         .get(
//           urls.question(
//             `/explain?file=${question.filepath}&char=${question.char}&line=${question.line}`
//           ),
//           captureRequest(onSuccess)
//         )
//         .on("error", (err) => {
//           log.log("Error on finding definition");
//           log.log(err);
//           onError(err);
//         });
//       break;
//     }
//     case "CallGraph": {
//       http
//         .get(
//           urls.question(`/callgraph?file=${question.filepath}`),
//           captureRequest(onSuccess)
//         )
//         .on("error", (err) => {
//           onError(err);
//         });
//       break;
//     }
//     default: {
//       log.log("wut?");
//     }
//   }
// };

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

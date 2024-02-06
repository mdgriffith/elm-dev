import * as Question from "../../../app/interop/watchtower/question";
// import * as Message from "../../../app/interop/messages";
// var WebSocketClient = require("websocket").client;
import WebSocket from "ws";

type WebSocketMsg =
  | { msg: "Discover"; details: { root: String; watching: Watching[] } }
  | { msg: "Changed"; details: { path: String } }
  | {
      msg: "Watched";
      details: Watching[];
    };

type Watching = { path: String; warnings: Boolean; docs: Boolean };

//

export class Watchtower {
  // @ts-ignore
  private connection;
  // @ts-ignore
  private retry;
  // @ts-ignore
  private serverProcess;
  // @ts-ignore
  private server;

  constructor() {
    this.connect();
  }

  private connect() {
    if (this.connection?.connected) {
      return;
    }
    this.log("Elm Dev connecting...");
    // self.startServer();

    // this.websocket = new WebSocket()
    socketConnect({
      url: Question.urls.websocket,
      onJoin: (connection) => {
        this.onJoin(connection);
      },
      onConnectionFailed: (err) => {
        this.onConnectionFailed(err);
      },
      receive: (msg) => {
        this.receive(msg);
      },
    });
  }

  private log(...msg: any) {
    console.log(msg);
  }

  private startServer() {
    Question.ask(
      Question.questions.serverHealth,
      (resp) => {
        this.log("Elm Dev server is already running!");
      },
      (err) => {
        this.log("Elm Dev server is not running, starting watchtower 2");
        try {
          const elmDev = ChildProcess.spawn(path.join(__dirname, "elm-dev"), [
            "start",
            `--port=${Question.port}`,
          ]);
          // log.obj("ELM DEV", elmDev);
          elmDev.on("close", function (code) {
            //Here you can get the exit code of the script

            this.log("THE GOOD TIMES ARE OVER code: " + code);
          });

          elmDev.stdout.setEncoding("utf8");
          elmDev.stdout.on("data", function (data) {
            //Here is where the output goes

            this.log("elmout: " + data.toString());

            // data=data.toString();
            // scriptOutput+=data;
          });

          elmDev.stderr.setEncoding("utf8");
          elmDev.stderr.on("data", function (data) {
            //Here is where the error output goes
            this.log("elmerr: " + data.toString());
          });
        } catch (watchTowerErr) {
          this.log("Bundled Elm Dev failed to auto-start");
          this.log(watchTowerErr);
        }
      }
    );
  }

  private onConnectionFailed(error) {
    const self = this;
    this.retry = setTimeout(function () {
      // log.log("Reattempting connection");
      self.log("Unable to connect to Elm Dev");
      socketConnect({
        url: Question.urls.websocket,
        onJoin: (connection) => {
          self.statusNoErrors();
          self.onJoin(connection);
        },
        onConnectionFailed: (err) => {
          self.onConnectionFailed(err);
        },
        receive: (msg) => {
          self.receive(msg);
        },
      });
    }, 10000);
  }

  private cancelRetry() {
    if (this.retry) {
      clearTimeout(this.retry);
      this.retry = null;
    }
  }

  private onJoin(connection) {
    this.cancelRetry();
    this.connection = connection;
    this.log("Connected!");
  }

  // Communication on websocket

  private sendToElmDevServer(msg: WebSocketMsg) {
    if (this.connection?.connected) {
      this.log("SENDING", msg);
      this.connection.sendUTF(JSON.stringify(msg));
    } else {
      this.log("SKIPPING (not connected)", msg);
      this.connect();
    }
  }

  private receive(msgString: string) {
    const msg = JSONSafe.parse(msgString);
    if (msg == null) {
      return;
    }
    ElmProjectPane.send(msg);
  }
}

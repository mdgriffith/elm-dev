// @ts-ignore
import { Elm } from "../../app/src/Main.elm";
import * as Question from "../../app/interop/watchtower/question";
import * as Message from "../../app/interop/messages";
import * as JSONSafe from "../../app/interop/utils/json";
import { platform } from "@tauri-apps/api/os";

async function boot() {
  const platformString = await platform();
  // Boot up the Elm App
  const app = Elm.Main.init({
    node: document.getElementById("elm-dev"),
    flags: { platform: platformString },
  });

  app.ports.toWorld.subscribe(function (message: any) {
    console.log("Message from Elm", message);
    switch (message.msg) {
      case "ConnectToServer":
        break;
      case "Jump":
        break;
      case "FillTypeSignatures":
        break;
      default:
        console.log("Unknown message", message);
        break;
    }
  });

  // Talk to Elm Dev

  const sendHttpGet = (
    url: string,
    onSuccess: (data: any) => void,
    onError: (error: Error) => void
  ) => {
    fetch(url)
      .then((resp) => {
        resp.json().then(onSuccess);
      })
      .catch(onError);
  };

  const elmDev = new Question.ElmDev("51213", "localhost", sendHttpGet);

  // Initial Health Check
  elmDev.ask(
    Question.questions.serverHealth,
    (resp: any) => {
      // Server is healthy
      app.ports.toElm.send(
        Message.serverStatus({
          status: "Connected",
          port: "51213",
          host: "localhost",
          version: "0.0.1",
        })
      );
    },
    (err: any) => {
      console.log(err);
    }
  );

  elmDev.connectWebSocket(
    (event: any) => {
      console.log("Connected", event);
    },
    (msg: any) => {
      console.log("Elm Dev WS Message:", msg);
      const parsed = JSONSafe.parse(msg.data);
      if (parsed == null) {
        return;
      }
      app.ports.toElm.send(parsed);
    },
    (event: any) => {
      console.log("Error", event);
    }
  );
}

boot();

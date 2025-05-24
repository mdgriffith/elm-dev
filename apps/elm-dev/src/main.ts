// @ts-ignore
import { Elm } from "../../app/src/Main.elm";
import * as Question from "../../app/interop/watchtower/question";
import * as Message from "../../app/interop/messages";
import * as JSONSafe from "../../app/interop/utils/json";
import { platform } from "@tauri-apps/plugin-os";
// import { moveWindow, Position } from "@tauri-apps/plugin-positioner";
import { getCurrentWebviewWindow } from "@tauri-apps/api/webviewWindow";
const appWindow = getCurrentWebviewWindow();

async function boot() {
  const platformString = await platform();
  // await moveWindow(Position.TopRight);
  console.log(platformString);
  // Boot up the Elm App
  const app = Elm.Main.init({
    node: document.getElementById("elm-dev"),
    flags: { platform: platformString },
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

  app.ports.toWorld.subscribe(function (message: any) {
    console.log("Message from Elm", message);
    switch (message.msg) {
      case "ConnectToServer":
        break;
      case "RequestProjectList":
        elmDev.ask(
          Question.questions.projectList,
          (resp: any) => {
            console.log("Project List", resp);
            app.ports.toElm.send(resp);
          },
          (err: any) => {
            console.log(err);
          }
        );

        break;
      case "Jump":
        break;
      case "FillTypeSignatures":
        break;
      case "WindowMinimize":
        appWindow.minimize();
        break;
      case "WindowMaximize":
        appWindow.toggleMaximize();
        break;
      case "WindowClose":
        appWindow.close();
        break;
      default:
        console.log("Unknown message", message);
        break;
    }
  });

  // Initial Health Check
  elmDev.ask(
    Question.questions.serverHealth,
    (resp: any) => {
      console.log("Server Health", resp);
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
      console.log("Parsed", parsed);
      if (parsed == null) {
        return;
      }
      console.log("Sending to Elm");
      app.ports.toElm.send(parsed);
    },
    (event: any) => {
      console.log("Error", event);
    }
  );
}

boot();

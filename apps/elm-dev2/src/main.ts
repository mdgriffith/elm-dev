// @ts-expect-error
import Main from "./src/app/Main.elm";
import * as LocalStorage from "./js/effect/local-storage";
import * as Effects from "./js/effects";
import Webcomponents from "./js/webcomponents";
import * as JSON from "./js/util/json";

// Mount Elm App
// Import all generated CSS files
import.meta.glob("../elm-stuff/generated/**/*.css", { eager: true });

// Include any custom elements we need.
Webcomponents();

// Boot up the Elm App
const app = Main.init({
  flags: { now: Date.now(), localStorage: LocalStorage.getAll() },
});

// Connect all effects
Effects.connect(app, {});

// Connect listeners
const domain = "localhost";
const port = "51213";
const websocket = new WebSocket(`ws://${domain}:${port}/ws`);

websocket.onopen = () => {
  console.log("Connected to websocket");
  app.ports.devServer.send({
    msg: "Server",
    details: {
      status: "Connected",
      version: "0.1.0",
      port: port,
      host: domain,
    },
  });
};

websocket.onmessage = (event) => {
  console.log("Message from Elm Dev", event);
  const parsed = JSON.safeParse(event.data);
  console.log("Parsed", parsed);
  if (parsed == null) {
    return;
  }
  app.ports.devServer.send(parsed);
};

websocket.onerror = (event) => {
  console.error("Error on websocket", event);
};

websocket.onclose = () => {
  console.log("Disconnected from websocket");
};
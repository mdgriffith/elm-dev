// @ts-expect-error
import Main from "./src/app/Main.elm";
import * as LocalStorage from "./js/effect/local-storage";
import * as Effects from "./js/effects";
import Webcomponents from "./js/webcomponents";
import * as JSON from "./js/util/json";
import { listen } from "@tauri-apps/api/event";
import { invoke } from "@tauri-apps/api/core";
import { fetch } from "@tauri-apps/plugin-http";
type TauriHttpResponse = { ok: boolean; json: () => Promise<unknown> };

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

// Connect listeners via daemon status from Tauri

type Endpoint = { domain: string; port: number };
type DaemonStatus = {
  pid: number;
  version: string;
  lsp: Endpoint;
  mcp: Endpoint;
  http: Endpoint;
};

let currentSocket: WebSocket | null = null;
let currentBase: string | null = null;

function connectWithStatus(status: DaemonStatus) {
  console.log("Connecting with status", status);
  if (currentSocket != null) return;
  const domain = status.http.domain;
  const port = String(status.http.port);
  currentBase = `http://${domain}:${port}`;
  const websocket = new WebSocket(`ws://${domain}:${port}/ws`);
  currentSocket = websocket;

  websocket.onopen = () => {
    app.ports.devServer.send({
      msg: "Server",
      details: {
        status: "Connected",
        version: status.version,
        port: port,
        host: domain,
      },
    });

    // Prefetch projects via Tauri HTTP (bypasses CORS)
    try {
      void fetch(`http://${domain}:${port}/projectList`, { method: "GET" })
        .then((res: TauriHttpResponse) => (res.ok ? res.json() : null))
        .then((body: unknown) => {
          if (body != null && typeof body === "object") {
            const details = (body as any).details ?? body;
            app.ports.devServer.send({ msg: "Status", details });
          }
        })
        .catch(() => { });
    } catch (_) {
      // plugin not available (non-Tauri env)
    }
  };

  websocket.onmessage = (event) => {
    const parsed = JSON.safeParse(event.data);
    if (parsed == null) return;
    app.ports.devServer.send(parsed);
  };

  websocket.onerror = (event) => {
    console.error("Error on websocket", event);
  };

  websocket.onclose = () => {
    currentSocket = null;
  };
}

async function setupDaemonIntegration() {
  // Listen for async status event
  try {
    const unlisten = await listen<DaemonStatus>("daemon://status", (e) => {
      connectWithStatus(e.payload);
    });
    void unlisten;
  } catch (_) {
    // no-op if event system is unavailable
  }

  // Immediately try to fetch current status (in case event already fired)
  try {
    const status = await invoke<DaemonStatus | null>("get_daemon_status");
    if (status != null) connectWithStatus(status);
  } catch (_) {
    // no-op if command is unavailable
  }
}

setupDaemonIntegration();

// Listen for Elm asks and fulfill via Tauri HTTP
type AskMessage = { route: string };
app.ports?.ask?.subscribe?.((msg: AskMessage) => {
  console.log("Ask", msg, currentBase);
  if (!currentBase) return;
  switch (msg?.route) {
    case "ProjectList": {
      void fetch(`${currentBase}/projectList`, { method: "GET" })
        .then((res: TauriHttpResponse) => (res.ok ? res.json() : null))
        .then((body: unknown) => {
          console.log("ProjectList response", body);
          if (body != null && typeof body === "object") {
            const details = (body as any).details ?? body;
            console.log("ProjectList TS side", details);
            app.ports.devServer.send({ msg: "Status", details });
          }
        })
        .catch((err) => {

          console.log("ProjectList error", err);
        });
      break;
    }
    default:
      break;
  }
});
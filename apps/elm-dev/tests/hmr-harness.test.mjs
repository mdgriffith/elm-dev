import assert from "node:assert/strict";
import os from "node:os";
import path from "node:path";
import { mkdtemp, mkdir, rm, writeFile } from "node:fs/promises";
import test from "node:test";
import { chromium } from "playwright";
import { createServer } from "vite";
import elmDevPlugin from "../../ts-tool/vite.mjs";

const ELM_JSON = JSON.stringify(
  {
    type: "application",
    "source-directories": ["src"],
    "elm-version": "0.19.1",
    dependencies: {
      direct: {
        "elm/browser": "1.0.2",
        "elm/core": "1.0.5",
        "elm/html": "1.0.0"
      },
      indirect: {
        "elm/json": "1.1.3",
        "elm/time": "1.0.0",
        "elm/url": "1.0.0",
        "elm/virtual-dom": "1.0.3"
      }
    },
    "test-dependencies": {
      direct: {},
      indirect: {}
    }
  },
  null,
  2
);

function mainElm(mainToken) {
  return `module Main exposing (main)

import Browser
import Html exposing (Html, text)
import Shared

type alias Model = ()

type Msg
    = NoOp

main : Program () Model Msg
main =
    Browser.sandbox
        { init = ()
        , update = update
        , view = view
        }

update : Msg -> Model -> Model
update _ model =
    model

view : Model -> Html Msg
view _ =
    text ("${mainToken}:" ++ Shared.message)
`;
}

function sharedElm(sharedToken) {
  return `module Shared exposing (message)

message : String
message =
    "${sharedToken}"
`;
}

const MAIN_TS = `// @ts-expect-error
import Main from "./Main.elm";
import { version } from "./version.ts";

const node = document.getElementById("app");
const counterButton = document.getElementById("counter-button");
const counterValue = document.getElementById("counter-value");

if (!node) {
  throw new Error("Missing #app node");
}

if (!counterButton || !counterValue) {
  throw new Error("Missing counter controls");
}

console.info("[hmr-harness] main.ts boot");

let counter = 0;

function renderCounter() {
  counterValue.textContent = String(counter);
  window.__hmrHarnessCounter = counter;
}

counterButton.addEventListener("click", () => {
  counter += 1;
  renderCounter();
});

window.__hmrHarnessBoots = (window.__hmrHarnessBoots || 0) + 1;
renderCounter();

node.setAttribute("data-ts-version", version);
Main.init({ node });
`;

const INDEX_HTML = `<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>elm-dev HMR harness</title>
  </head>
  <body>
    <button id="counter-button">increment</button>
    <span id="counter-value">0</span>
    <div id="app"></div>
    <script type="module" src="/src/main.ts"></script>
  </body>
</html>
`;

async function writeFixtureFiles(root, { mainToken, sharedToken, tsToken }) {
  await mkdir(path.join(root, "src"), { recursive: true });
  await writeFile(path.join(root, "elm.json"), ELM_JSON);
  await writeFile(path.join(root, "index.html"), INDEX_HTML);
  await writeFile(path.join(root, "src", "main.ts"), MAIN_TS);
  await writeFile(path.join(root, "src", "version.ts"), `export const version = "${tsToken}";\n`);
  await writeFile(path.join(root, "src", "Main.elm"), mainElm(mainToken));
  await writeFile(path.join(root, "src", "Shared.elm"), sharedElm(sharedToken));
}

async function startHarness(initialTokens = { mainToken: "MAIN_001", sharedToken: "SHARED_001", tsToken: "TS_001" }) {
  const root = await mkdtemp(path.join(os.tmpdir(), "elm-dev-hmr-"));
  await writeFixtureFiles(root, initialTokens);

  const server = await createServer({
    root,
    logLevel: "silent",
    server: {
      host: "127.0.0.1",
      port: 0,
      strictPort: false
    },
    plugins: [
      elmDevPlugin({
        debug: true,
        optimize: false,
        useDevServer: true
      })
    ]
  });

  const wsMessages = [];
  const originalWsSend = server.ws.send.bind(server.ws);
  server.ws.send = (payload, clients) => {
    wsMessages.push(payload);
    return originalWsSend(payload, clients);
  };

  await server.listen();
  const origin = server.resolvedUrls?.local?.[0];

  if (!origin) {
    await server.close();
    await rm(root, { recursive: true, force: true });
    throw new Error("Vite dev server did not expose a local URL");
  }

  return {
    root,
    origin,
    getWsMessages() {
      return wsMessages.slice();
    },
    async dispose() {
      await server.close();
      await rm(root, { recursive: true, force: true });
    }
  };
}

async function fetchPath(origin, filePath) {
  const separator = filePath.startsWith("/") ? "" : "/";
  const url = `${origin.replace(/\/$/, "")}${separator}${filePath}`;
  const response = await fetch(url);
  const body = await response.text();
  return { ok: response.ok, status: response.status, body, url };
}

async function fetchTransformedElmModule(origin) {
  return fetchPath(origin, "/src/Main.elm?import");
}

async function pollUntil(check, {
  timeoutMs = 20000,
  intervalMs = 150,
  failureLabel = "condition"
} = {}) {
  const startedAt = Date.now();
  let lastValue;

  while (Date.now() - startedAt < timeoutMs) {
    lastValue = await check();
    if (lastValue?.ok) {
      return lastValue;
    }
    await new Promise((resolve) => setTimeout(resolve, intervalMs));
  }

  throw new Error(`Timed out waiting for ${failureLabel}. Last value: ${JSON.stringify(lastValue)}`);
}

test("updates when Main.elm changes", { timeout: 120000 }, async () => {
  const harness = await startHarness();

  try {
    const mainPath = path.join(harness.root, "src", "Main.elm");

    await pollUntil(async () => {
      const result = await fetchTransformedElmModule(harness.origin);
      return { ok: result.ok && result.body.includes("MAIN_001") && result.body.includes("SHARED_001") };
    }, { failureLabel: "initial Main.elm compile" });

    await writeFile(mainPath, mainElm("MAIN_002"));

    await pollUntil(async () => {
      const result = await fetchTransformedElmModule(harness.origin);
      return {
        ok: result.ok && result.body.includes("MAIN_002") && result.body.includes("SHARED_001"),
        status: result.status
      };
    }, { failureLabel: "Main.elm token MAIN_002" });
  } finally {
    await harness.dispose();
  }
});

test("updates when Shared.elm dependency changes", { timeout: 120000 }, async () => {
  const harness = await startHarness();

  try {
    const sharedPath = path.join(harness.root, "src", "Shared.elm");

    await pollUntil(async () => {
      const result = await fetchTransformedElmModule(harness.origin);
      return { ok: result.ok && result.body.includes("SHARED_001") };
    }, { failureLabel: "initial shared token" });

    await writeFile(sharedPath, sharedElm("SHARED_002"));

    await pollUntil(async () => {
      const result = await fetchTransformedElmModule(harness.origin);
      return {
        ok: result.ok && result.body.includes("SHARED_002"),
        status: result.status
      };
    }, { failureLabel: "Shared.elm token SHARED_002" });
  } finally {
    await harness.dispose();
  }
});

test("rapid Shared.elm writes settle on latest version", { timeout: 120000 }, async () => {
  const harness = await startHarness();

  try {
    const sharedPath = path.join(harness.root, "src", "Shared.elm");

    await writeFile(sharedPath, sharedElm("SHARED_BURST_001"));
    await writeFile(sharedPath, sharedElm("SHARED_BURST_002"));
    await writeFile(sharedPath, sharedElm("SHARED_BURST_003"));

    await pollUntil(async () => {
      const result = await fetchTransformedElmModule(harness.origin);
      return {
        ok: result.ok && result.body.includes("SHARED_BURST_003"),
        body: result.body
      };
    }, { failureLabel: "latest burst token SHARED_BURST_003" });

    await new Promise((resolve) => setTimeout(resolve, 600));
    const postSettle = await fetchTransformedElmModule(harness.origin);
    assert.equal(postSettle.ok, true);
    assert.equal(postSettle.body.includes("SHARED_BURST_003"), true);
    assert.equal(postSettle.body.includes("SHARED_BURST_002"), false);
  } finally {
    await harness.dispose();
  }
});

test("Elm compile error recovers after fix", { timeout: 120000 }, async () => {
  const harness = await startHarness();

  try {
    const sharedPath = path.join(harness.root, "src", "Shared.elm");

    await writeFile(
      sharedPath,
      `module Shared exposing (message)

message : String
message =
    BROKEN_SYNTAX
`
    );

    await pollUntil(async () => {
      const result = await fetchTransformedElmModule(harness.origin);
      return { ok: !result.ok, status: result.status };
    }, { failureLabel: "non-OK response after Elm syntax error" });

    await writeFile(sharedPath, sharedElm("SHARED_RECOVERED_001"));

    await pollUntil(async () => {
      const result = await fetchTransformedElmModule(harness.origin);
      return {
        ok: result.ok && result.body.includes("SHARED_RECOVERED_001"),
        status: result.status
      };
    }, { failureLabel: "recovered token SHARED_RECOVERED_001" });
  } finally {
    await harness.dispose();
  }
});

test("TypeScript updates are reflected by Vite", { timeout: 120000 }, async () => {
  const harness = await startHarness();

  try {
    const versionPath = path.join(harness.root, "src", "version.ts");

    await pollUntil(async () => {
      const result = await fetchPath(harness.origin, "/src/version.ts");
      return { ok: result.ok && result.body.includes("TS_001") };
    }, { failureLabel: "initial TS token" });

    await writeFile(versionPath, 'export const version = "TS_002";\n');

    await pollUntil(async () => {
      const result = await fetchPath(harness.origin, "/src/version.ts");
      return {
        ok: result.ok && result.body.includes("TS_002"),
        status: result.status
      };
    }, { failureLabel: "updated TS token TS_002" });
  } finally {
    await harness.dispose();
  }
});

test("Elm updates do not trigger Vite full reload", { timeout: 120000 }, async () => {
  const harness = await startHarness();

  try {
    const mainPath = path.join(harness.root, "src", "Main.elm");

    await pollUntil(async () => {
      const result = await fetchTransformedElmModule(harness.origin);
      return { ok: result.ok && result.body.includes("MAIN_001") };
    }, { failureLabel: "initial Main.elm token" });

    const wsCountBeforeChange = harness.getWsMessages().length;

    await writeFile(mainPath, mainElm("MAIN_NO_FULL_RELOAD_002"));

    await pollUntil(async () => {
      const result = await fetchTransformedElmModule(harness.origin);
      return { ok: result.ok && result.body.includes("MAIN_NO_FULL_RELOAD_002") };
    }, { failureLabel: "updated Main.elm token without full reload" });

    const sentFullReload = harness
      .getWsMessages()
      .slice(wsCountBeforeChange)
      .some((msg) => msg && msg.type === "full-reload");

    assert.equal(sentFullReload, false);
  } finally {
    await harness.dispose();
  }
});

test("Elm update preserves browser state without navigation", { timeout: 120000 }, async () => {
  const harness = await startHarness();

  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();
  let navigationCount = 0;
  const pageErrors = [];
  const consoleMessages = [];
  const responseStatuses = [];
  const requestFailures = [];

  page.on("framenavigated", (frame) => {
    if (frame === page.mainFrame()) {
      navigationCount += 1;
    }
  });
  page.on("pageerror", (err) => {
    pageErrors.push(String(err));
  });
  page.on("console", (msg) => {
    consoleMessages.push(`${msg.type()}: ${msg.text()}`);
  });
  page.on("response", (response) => {
    const url = response.url();
    if (url.includes("/src/main.ts") || url.includes("Main.elm") || url.includes("/@vite/client")) {
      responseStatuses.push({ url, status: response.status() });
    }
  });
  page.on("requestfailed", (request) => {
    requestFailures.push({ url: request.url(), failure: request.failure() });
  });

  try {
    await page.goto(harness.origin, { waitUntil: "networkidle" });

    await page.waitForSelector("#counter-button");
    await page.waitForSelector("#counter-value");
    try {
      await page.waitForFunction(() => window.__hmrHarnessBoots === 1);
    } catch (error) {
      throw new Error(`main.ts did not boot. errors=${JSON.stringify(pageErrors)} console=${JSON.stringify(consoleMessages)} responses=${JSON.stringify(responseStatuses)} failed=${JSON.stringify(requestFailures)}`);
    }

    await page.evaluate(() => {
      document.getElementById("counter-button")?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
      document.getElementById("counter-button")?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
      document.getElementById("counter-button")?.dispatchEvent(new MouseEvent("click", { bubbles: true }));
    });

    await pollUntil(async () => {
      const value = await page.textContent("#counter-value");
      return { ok: value === "3", value };
    }, { failureLabel: "counter increment to 3 before Elm edit" });

    await pollUntil(async () => {
      const appText = await page.textContent("body");
      return { ok: Boolean(appText && appText.includes("MAIN_001:SHARED_001")), appText };
    }, { failureLabel: "initial Elm render token in browser" });

    const bootsBefore = await page.evaluate(() => window.__hmrHarnessBoots);
    const counterBefore = await page.evaluate(() => window.__hmrHarnessCounter);

    await writeFile(path.join(harness.root, "src", "Main.elm"), mainElm("MAIN_BROWSER_STATE_002"));

    await pollUntil(async () => {
      const hasElmUpdateMessage = harness
        .getWsMessages()
        .some((msg) => msg && msg.type === "update" && Array.isArray(msg.updates) && msg.updates.some((u) => String(u.path || "").includes("Main.elm")));

      return { ok: hasElmUpdateMessage };
    }, { failureLabel: "Vite update message for Elm module" });

    await new Promise((resolve) => setTimeout(resolve, 700));

    await pollUntil(async () => {
      const appText = await page.textContent("body");
      return { ok: Boolean(appText && appText.includes("MAIN_BROWSER_STATE_002:SHARED_001")), appText };
    }, { failureLabel: "updated Elm render token in browser" });

    const counterAfter = await page.evaluate(() => window.__hmrHarnessCounter);
    const bootsAfter = await page.evaluate(() => window.__hmrHarnessBoots);

    assert.equal(counterBefore, 3);
    assert.equal(counterAfter, 3);
    assert.equal(bootsBefore, 1);
    assert.equal(bootsAfter, 1);
    assert.equal(navigationCount, 1);
  } finally {
    await page.close();
    await browser.close();
    await harness.dispose();
  }
});

test("repeated Elm HMR updates do not report duplicate Elm.Main", { timeout: 120000 }, async () => {
  const harness = await startHarness();

  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();
  const pageErrors = [];
  const consoleErrors = [];
  const badResponses = [];

  page.on("pageerror", (err) => {
    pageErrors.push(String(err));
  });
  page.on("console", (msg) => {
    if (msg.type() === "error" || msg.type() === "warning") {
      consoleErrors.push(msg.text());
    }
  });
  page.on("response", async (response) => {
    if (response.status() >= 400) {
      const url = response.url();
      let body = "";
      try {
        body = await response.text();
      } catch (_error) {
        body = "<unreadable>";
      }
      badResponses.push({ url, status: response.status(), body });
    }
  });

  try {
    await page.goto(harness.origin, { waitUntil: "networkidle" });
    await page.waitForSelector("body");
    try {
      await page.waitForFunction(() => window.__hmrHarnessBoots === 1);
    } catch (_error) {
      const mainTs = await fetchPath(harness.origin, "/src/main.ts");
      throw new Error(`main.ts did not boot. pageErrors=${JSON.stringify(pageErrors)} consoleErrors=${JSON.stringify(consoleErrors)} badResponses=${JSON.stringify(badResponses)} mainTs=${JSON.stringify({ status: mainTs.status, body: mainTs.body })}`);
    }

    await pollUntil(async () => {
      const appText = await page.textContent("body");
      return { ok: Boolean(appText && appText.includes("MAIN_001:SHARED_001")), appText };
    }, { failureLabel: `initial Elm token before repeated updates. pageErrors=${JSON.stringify(pageErrors)} consoleErrors=${JSON.stringify(consoleErrors)}` });

    const mainPath = path.join(harness.root, "src", "Main.elm");
    const updateTokens = [
      "MAIN_DUPCHECK_002",
      "MAIN_DUPCHECK_003",
      "MAIN_DUPCHECK_004"
    ];

    for (const token of updateTokens) {
      await writeFile(mainPath, mainElm(token));
      await pollUntil(async () => {
        const appText = await page.textContent("body");
        return { ok: Boolean(appText && appText.includes(`${token}:SHARED_001`)), appText };
      }, { failureLabel: `Elm token ${token} after HMR update` });
    }

    const combined = `${pageErrors.join("\n")}\n${consoleErrors.join("\n")}`;
    assert.equal(combined.includes("loading multiple Elm scripts with a module named Elm.Main"), false, combined);
    assert.equal(combined.includes("Failed to reload /elm-stuff/generated/Main.elm"), false, combined);
  } finally {
    await page.close();
    await browser.close();
    await harness.dispose();
  }
});

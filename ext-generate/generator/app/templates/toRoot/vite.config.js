import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import { spawn } from "child_process";
import * as path from "path";

function elmPrefabPlugin() {
  return {
    name: "vite-plugin-elm-prefab",
    configureServer(server) {
      server.watcher.on("change", (file) => {
        if (
          path.basename(file) === "elm.generate.json" ||
          path.extname(file) == ".gql" ||
          path.extname(file) == ".graphql"
        ) {
          console.log("Elm Prefab refreshing...");

          const elmPrefab = spawn("elm-prefab", [], { shell: true });

          elmPrefab.stdout.on("data", (data) => {
            console.log(data);
          });

          elmPrefab.stderr.on("data", (data) => {
            console.error(data);
          });

          elmPrefab.on("close", (code) => {
            if (code === 0) {
              // Trigger a reload after successful elm-prefab execution
              server.ws.send({ type: "full-reload" });
            }
          });
        }
      });
    },
  };
}

export default defineConfig(({ mode }) => {
  const isDev = mode == "development";
  return {
    clearScreen: false,
    server: {
      strictPort: true,
      open: true,
    },

    build: {
      minify: "esbuild",
      outDir: "dist",
    },

    plugins: [
      elmPrefabPlugin(),
      elmPlugin({
        debug: isDev,
        optimize: !isDev,
      }),
    ],
  };
});

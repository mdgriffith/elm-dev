import { defineConfig } from "vite";
import elmDev from "elm-dev/vite";

// https://vitejs.dev/config/
export default defineConfig(({ mode }) => {
  const isDev = mode == "development";
  return {
    build: {
      minify: "esbuild",
      outDir: "dist",
    },
    plugins: [
      elmDev({
        mode: isDev ? "debug" : "optimize",
      }),
    ],
  }
});

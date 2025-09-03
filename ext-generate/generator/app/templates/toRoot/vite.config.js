import { defineConfig } from "vite";
import elmDev from "./viteElm";


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
      elmDev({
        debug: isDev,
        optimize: !isDev,
      }),
    ],
  };
});

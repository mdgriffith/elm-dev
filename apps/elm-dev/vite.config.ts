import { defineConfig } from "vite";
// @ts-ignore
import elmPlugin from "vite-plugin-elm";

// https://vitejs.dev/config/
export default defineConfig(async ({ mode }) => {
  const isDev = mode == "development";
  return {
    // Vite options tailored for Tauri development and only applied in `tauri dev` or `tauri build`
    //
    // 1. prevent vite from obscuring rust errors
    clearScreen: false,
    // 2. tauri expects a fixed port, fail if that port is not available
    server: {
      port: 1420,
      strictPort: true,
      watch: {
        // 3. tell vite to ignore watching `src-tauri`
        ignored: ["**/src-tauri/**"],
        // Watch the elm app
        additionalPaths: ["../app/src/**"],
      },
    },
    plugins: [
      elmPlugin({
        debug: isDev,
        optimize: !isDev,
      }),
    ],
  };
});

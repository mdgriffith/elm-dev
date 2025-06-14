import { defineConfig } from "vite";
// @ts-ignore
import elmPlugin from "vite-plugin-elm";

const host = process.env.TAURI_DEV_HOST;

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
      // if the host Tauri is expecting is set, use it
      host: host || false,

      hmr: host
        ? {
          protocol: 'ws',
          host,
          port: 1421,
        }
        : undefined,


      watch: {
        // 3. tell vite to ignore watching `src-tauri`
        ignored: ["**/src-tauri/**"],
        // Watch the elm app
        additionalPaths: ["../app/src/**"],
      },
    },
    // Env variables starting with the item of `envPrefix` will be exposed in tauri's source code through `import.meta.env`.
    envPrefix: ['VITE_', 'TAURI_ENV_*'],

    plugins: [
      elmPlugin({
        debug: isDev,
        optimize: !isDev,
      }),
    ],
  };
});

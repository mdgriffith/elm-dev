import type { Plugin } from 'vite';
import { spawn } from 'child_process';
import path from 'path';

interface ElmDevPluginOptions {
    debug?: boolean;
    optimize?: boolean;
}

export default function elmDevPlugin(options: ElmDevPluginOptions = {}): Plugin {
    const { debug = false, optimize = false } = options;

    return {
        name: 'vite-plugin-elm-dev',
        enforce: 'pre',

        async resolveId(id: string) {
            if (id.endsWith('.elm')) {
                return id;
            }
            return null;
        },

        async load(id: string) {
            if (!id.endsWith('.elm')) {
                return null;
            }
            const args = ['make', path.join(".", id), '--output=stdout'];
            if (debug) {
                args.push('--debug');
            }
            if (optimize) {
                args.push('--optimize');
            }

            return new Promise((resolve, reject) => {
                const elmDev = spawn('elm-dev', args, { shell: true });
                let output = '';
                let error = '';

                elmDev.stdout.on('data', (data) => {
                    output += data.toString();
                });

                elmDev.stderr.on('data', (data) => {
                    error += data.toString();
                });

                elmDev.on('close', (code) => {
                    if (code === 0) {
                        // Write output to generated.js for debugging

                        const wrappedCode = `
const scope = {};
function start() {
  ${output}
}
start.call(scope);
export default scope.Elm;
                        `;

                        const fs = require('fs');
                        fs.writeFileSync('generated.js', wrappedCode);
                        resolve(wrappedCode);
                        resolve(output);
                    } else {
                        reject(new Error(error));
                    }
                });
            });
        },

        configureServer(server) {
            // Watch for changes in Elm files and trigger rebuilds
            server.watcher.add('**/*.elm');

            // Handle file changes
            server.watcher.on('change', (file) => {
                if (file.endsWith('.elm')) {
                    // Invalidate the module cache for the changed file
                    const module = server.moduleGraph.getModuleById(file);
                    if (module) {
                        server.moduleGraph.invalidateModule(module);
                    }

                    // Trigger a full reload to ensure all dependencies are recompiled
                    server.ws.send({ type: 'full-reload' });
                }
            });
        },
    };
}

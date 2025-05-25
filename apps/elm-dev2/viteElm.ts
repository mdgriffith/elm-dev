/// <reference types="node" />
import type { Plugin } from 'vite';
// @ts-ignore
import { spawn } from 'child_process';
// @ts-ignore
import path from 'path';

interface ElmDevPluginOptions {
    debug?: boolean;
    optimize?: boolean;
}

interface ModuleState {
    code: string | null;
    isCompiling: boolean;
    compilationPromise: Promise<string> | null;
}

function setEmptyCacheFor(id: string, compilationCache: Map<string, ModuleState>): ModuleState {
    const moduleState = {
        code: null,
        isCompiling: false,
        compilationPromise: null
    };
    compilationCache.set(id, moduleState);
    return moduleState;
}

function emptyAllCache(compilationCache: Map<string, ModuleState>) {
    for (const [id] of compilationCache) {
        setEmptyCacheFor(id, compilationCache);
    }
}

async function compileElmModule(id: string, options: { debug: boolean, optimize: boolean }): Promise<string> {
    const { debug, optimize } = options;
    const cwd = process.cwd();
    const entrypoints = [path.join(".", id)];

    try {
        // First try to use the dev server
        const url = new URL('http://localhost:51213/make');
        url.searchParams.append('cwd', cwd);
        url.searchParams.append('entrypoints', entrypoints.join(','));

        const response = await fetch(url);
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }
        return await response.text();
    } catch (error) {
        // If dev server fails, fall back to spawning elm-dev
        return new Promise<string>((resolve, reject) => {
            const args = ['make', path.join(".", id), '--output=stdout'];
            if (debug) {
                args.push('--debug');
            }
            if (optimize) {
                args.push('--optimize');
            }

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
                    resolve(output);
                } else {
                    reject(new Error(error));
                }
            });
        });
    }
}

export default function elmDevPlugin(options: ElmDevPluginOptions = {}): Plugin {
    const { debug = false, optimize = false } = options;
    // Track loaded Elm files and their compiled output
    const compilationCache = new Map<string, ModuleState>();

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

            // Get or create module state
            let moduleState = compilationCache.get(id);
            if (!moduleState) {
                moduleState = setEmptyCacheFor(id, compilationCache);
            }

            // If we have compiled code, return it
            if (moduleState.code) {
                return moduleState.code;
            }

            // If we're already compiling, wait for that compilation
            if (moduleState.isCompiling && moduleState.compilationPromise) {
                return moduleState.compilationPromise;
            }

            // Start new compilation
            moduleState.isCompiling = true;
            const compilationPromise = compileElmModule(id, { debug, optimize })
                .then(output => {
                    const wrappedCode = `
const scope = {};
function start() {
  ${output}
}
start.call(scope);
export default scope.Elm;
                    `;
                    moduleState.code = wrappedCode;
                    moduleState.isCompiling = false;
                    moduleState.compilationPromise = null;
                    return wrappedCode;
                })
                .catch(error => {
                    setEmptyCacheFor(id, compilationCache);
                    throw error;
                });

            moduleState.compilationPromise = compilationPromise;
            return compilationPromise;
        },

        configureServer(server) {
            // Watch for changes in Elm files and trigger rebuilds
            server.watcher.add('**/*.elm');

            // Handle file changes
            server.watcher.on('change', (file) => {
                if (file.endsWith('.elm') && !file.includes('elm-stuff')) {
                    // Invalidate all loaded Elm modules since they might depend on the changed file
                    for (const [loadedId] of compilationCache) {
                        // Clear the cache for the changed file
                        const moduleState = {
                            code: null,
                            isCompiling: false,
                            compilationPromise: null
                        };
                        compilationCache.set(loadedId, moduleState);
                        const modules = server.moduleGraph.getModulesByFile(loadedId);
                        if (modules) {
                            for (const m of modules) {
                                server.moduleGraph.invalidateModule(m);
                            }
                        } else {
                            console.log('Module not found in moduleGraph', modules);
                        }
                    }

                    server.ws.send({ type: 'full-reload' });
                }
            });

            // Also watch for changes in elm.json
            server.watcher.add('elm.json');
            server.watcher.add('elm.generate.json');
            server.watcher.on('change', (file) => {
                if (file === 'elm.json') {
                    emptyAllCache(compilationCache);
                    server.ws.send({ type: 'full-reload' });
                }
            });
        },
    };
}

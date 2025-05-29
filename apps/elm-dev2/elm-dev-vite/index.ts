/// <reference types="node" />
import type { Plugin, ViteDevServer } from 'vite';
import { spawn } from 'child_process';
import path from 'path';
import * as Hot from "./hot-client";
import * as ElmErrorJson from "./elm-error-json";
// import launchEditor from 'launch-editor'

interface ElmDevPluginOptions {
    debug?: boolean;
    optimize?: boolean;
}

interface ModuleState {
    code: string | null;
    error: any | null;
    isCompiling: boolean;
    isDirty: boolean;
    compilationPromise: Promise<string> | null;
}

function setEmptyCacheFor(id: string, compilationCache: Map<string, ModuleState>): ModuleState {
    const moduleState = {
        code: null,
        error: null,
        isCompiling: false,
        compilationPromise: null,
        isDirty: false
    };
    compilationCache.set(id, moduleState);
    return moduleState;
}

function setError(id: string, error: string, compilationCache: Map<string, ModuleState>) {
    const moduleState = compilationCache.get(id);
    if (moduleState) {
        moduleState.error = error;
    }
}

function setDirty(id: string, compilationCache: Map<string, ModuleState>) {
    const moduleState = compilationCache.get(id);
    if (moduleState) {
        moduleState.isDirty = true;
    }
}


type Result =
    | { success: string }
    | { error: any };

async function compileElmModule(root: string, id: string, options: { debug: boolean, optimize: boolean }): Promise<Result> {
    const { debug, optimize } = options;

    const entrypoints = [path.join(".", id)];

    try {
        // First try to use the dev server
        const url = new URL('http://localhost:51213/make');
        url.searchParams.append('cwd', root);
        url.searchParams.append('entrypoints', entrypoints.join(','));
        if (debug) {
            url.searchParams.append('debug', 'true');
        } else if (optimize) {
            url.searchParams.append('optimize', 'true');
        }

        const response = await fetch(url);
        if (!response.ok) {
            throw new Error(`HTTP error! status: ${response.status}`);
        }
        const text = await response.text();
        if (text.startsWith('// success')) {
            return { success: text };
        } else {
            const json = JSON.parse(text);
            return { error: json };
        }
    } catch (error) {
        console.log('Falling back to elm-dev command', error);
        // If dev server fails, fall back to spawning elm-dev
        return new Promise<Result>((resolve, reject) => {
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
                    resolve({ success: output });
                } else {
                    resolve({ error: { error } });
                }
            });
        });
    }
}

/**
 * Attempts to guess the Elm module name from a file path.
 * 
 * This works by:
 * 1. Splitting the path into parts and reversing them
 * 2. Looking at parts from the end until we hit a non-capitalized part
 * 3. Joining the capitalized parts with dots to form the module name
 * 
 * For example:
 * - "src/MyApp/Views/Main.elm" -> "Main.Views.MyApp"
 * - "src/MyApp/views/Main.elm" -> "Main" (stops at "views" since it's lowercase)
 */
function guessElmModuleName(id: string): string {
    const parts = id.split(path.sep).reverse();
    const moduleNameParts: string[] = [];
    for (const part of parts) {
        if (/^[A-Z]/.test(part.replace('.elm', ''))) {
            moduleNameParts.push(part.replace('.elm', ''));
        } else {
            break;
        }
    }
    return moduleNameParts.reverse().join('.');
}

export default function elmDevPlugin(options: ElmDevPluginOptions = {}): Plugin {
    const { debug = false, optimize = false } = options;
    // Track loaded Elm files and their compiled output
    const compilationCache = new Map<string, ModuleState>();
    console.log('ElmDevPlugin', options);


    let server: ViteDevServer | undefined = undefined;
    let viteConfigRoot: string = process.cwd();

    return {
        name: 'vite-plugin-elm-dev',
        enforce: 'pre',

        async resolveId(id: string) {
            if (id.endsWith('.elm')) {
                return id;
            }
            return null;
        },

        configResolved(config) {
            viteConfigRoot = config.root;
        },

        configureServer(server_) {
            server = server_
            // Watch for changes in Elm files and tell vite to invalidate the entrypoints.
            server_.watcher.add('**/*.elm');
            server_.watcher.add('elm.json');
            server_.watcher.add('elm.generate.json');

            server.ws.on('elm:client-ready', ({ id }) => {
                const moduleState = compilationCache.get(id);
                if (moduleState && moduleState.error) {
                    server_.ws.send('elm:error', {
                        id,
                        error: ElmErrorJson.toColoredHtmlOutput(moduleState.error)
                    })
                } else if (moduleState && moduleState.code) {
                    server_.ws.send('elm:success', { id })
                }
            })
            //   server.ws.on('elm:open-editor', ({ filepath }) => {
            //     launchEditor(filepath)
            //   })

            // Handle file changes
            server_.watcher.on('change', (file) => {
                const elmRelatedFile = file.endsWith('.elm') || file.endsWith('elm.json') || file.endsWith('elm.generate.json');
                if (elmRelatedFile && !file.includes('elm-stuff')) {
                    // Invalidate all loaded Elm modules since they might depend on the changed file
                    for (const [loadedId] of compilationCache) {
                        // Clear the cache for the changed file
                        setDirty(loadedId, compilationCache);
                        const modules = server_.moduleGraph.getModulesByFile(loadedId);
                        if (modules) {
                            for (const m of modules) {
                                server_.moduleGraph.invalidateModule(m);
                            }
                        } else {
                            console.log('Module not found in moduleGraph', modules);
                        }
                    }

                    server_.ws.send({ type: 'full-reload' });
                }
            });
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
            if (moduleState.code && !moduleState.isDirty) {
                return moduleState.code;
            }

            // If we're already compiling, wait for that compilation
            if (moduleState.isCompiling && moduleState.compilationPromise && !moduleState.isDirty) {
                return moduleState.compilationPromise;
            }

            // Start new compilation
            moduleState.isCompiling = true;
            const compilationPromise = compileElmModule(viteConfigRoot, id, { debug, optimize })
                .then(result => {
                    if ('error' in result) {
                        console.log("ERROR FROM SERVER", result.error);
                        server?.ws.send('elm:error', {
                            id,
                            error: ElmErrorJson.toColoredHtmlOutput(result.error)
                        })
                        setError(id, result.error, compilationCache);
                        if (moduleState.code) {
                            return moduleState.code
                        } else {
                            // Fake an empty Elm mocule
                            return `export default { init: () => ({}) }; ${Hot.toJs(id, false)}; import.meta.hot.accept()`
                        }
                    }
                    // Get capitalized parts from end of path until we hit a non-capitalized part
                    const elmModuleName = guessElmModuleName(id);
                    server?.ws.send('elm:success', { id })
                    const wrappedCode = `
const scope = {};
function start() {
  ${result.success}
}
start.call(scope);
export default scope.Elm.${elmModuleName};

${Hot.toJs(id, true)}
                    `;
                    moduleState.code = wrappedCode;
                    moduleState.isCompiling = false;
                    moduleState.compilationPromise = null;
                    moduleState.isDirty = false;
                    moduleState.error = null;
                    return wrappedCode;

                })
                .catch(error => {
                    setEmptyCacheFor(id, compilationCache);
                    throw error;
                });

            moduleState.compilationPromise = compilationPromise;

            return compilationPromise;
        },
    };
}

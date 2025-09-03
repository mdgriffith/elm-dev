// @ts-nocheck
import type { Plugin } from 'vite';
import { spawn } from 'node:child_process';
import path from 'node:path';

interface ElmDevPluginOptions {
    debug?: boolean;
    optimize?: boolean;
    useDevServer?: boolean;
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

export default function elmDevPlugin(options: ElmDevPluginOptions = {}): Plugin {
    const { debug = false, optimize = false, useDevServer = true } = options;
    // Track loaded Elm files and their compiled output
    const compilationCache = new Map<string, ModuleState>();
    let devServer: { domain: string; httpPort: number } | null = null;

    async function discoverDevServer(): Promise<{ domain: string; httpPort: number } | null> {
        return new Promise((resolve) => {
            const child = spawn('elm-dev', ['daemon', 'status'], { shell: true });
            let out = '';
            let err = '';
            child.stdout.on('data', (d) => { out += d.toString(); });
            child.stderr.on('data', (d) => { err += d.toString(); });
            child.on('close', () => {
                try {
                    const json = JSON.parse(out);
                    // New shape: { pid, lsp: {domain, port}, mcp: {...}, http: {...}, version }
                    if (json && json.http && json.http.domain && json.http.port) {
                        resolve({ domain: json.http.domain, httpPort: json.http.port });
                    } else if (json && json.httpPort) {
                        // Back-compat fallback
                        resolve({ domain: '127.0.0.1', httpPort: json.httpPort });
                    } else {
                        resolve(null);
                    }
                } catch (_e) {
                    resolve(null);
                }
            });
        });
    }

    async function rpc<T = any>(serverInfo: { domain: string; httpPort: number }, method: string, params: any): Promise<T> {
        const body = { jsonrpc: '2.0', id: 1, method, params };
        const url = `http://${serverInfo.domain}:${serverInfo.httpPort}/`;
        const res = await fetch(url, { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify(body) });
        if (!res.ok) throw new Error(`Dev server HTTP ${res.status}`);
        const data = await res.json();
        if (data && data.result !== undefined) return data.result as T;
        throw new Error(data && data.error ? JSON.stringify(data.error) : 'Invalid JSON-RPC response');
    }

    function clientReloadSnippet(serverInfo: { domain: string; httpPort: number }): string {
        const wsUrl = `ws://${serverInfo.domain}:${serverInfo.httpPort}/ws`;
        return `
(() => {
  try {
    const ws = new WebSocket(${JSON.stringify(wsUrl)});
    ws.onmessage = (evt) => {
      try {
        const msg = JSON.parse(evt.data);
        if (msg && msg.msg === 'Status') {
          window.location.reload();
        }
      } catch (_) {}
    };
  } catch (_) {}
})();
        `;
    }

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
            const compilationPromise = new Promise<string>(async (resolve, reject) => {
                try {
                    if (useDevServer) {
                        if (!devServer) {
                            const discovered = await discoverDevServer();
                            if (discovered) {
                                devServer = { ...discovered };
                            }
                        }
                    }
                    if (useDevServer && devServer) {
                        // Use JSON-RPC Dev.js to compile this file
                        const result = await rpc<{ compiled: boolean; code?: string }>(devServer, 'js', {
                            dir: '.',
                            file: path.join('.', id),
                            debug,
                            optimize,
                        });
                        if (!result.compiled || !result.code) throw new Error('Compilation failed');
                        const wrappedCode = `
const scope = {};
function start() {
  ${result.code}
}
start.call(scope);
export default scope.Elm;
                        `;
                        moduleState.code = wrappedCode;
                        moduleState.isCompiling = false;
                        moduleState.compilationPromise = null;
                        resolve(wrappedCode);
                        return;
                    }

                    // Fallback: spawn elm-dev make --output=stdout
                    const args = ['make', path.join('.', id), '--output=stdout'];
                    if (debug) args.push('--debug');
                    if (optimize) args.push('--optimize');
                    const elmDev = spawn('elm-dev', args, { shell: true });
                    let output = '';
                    let error = '';
                    elmDev.stdout.on('data', (data) => { output += data.toString(); });
                    elmDev.stderr.on('data', (data) => { error += data.toString(); });
                    elmDev.on('close', (code) => {
                        if (code === 0) {
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
                            resolve(wrappedCode);
                        } else {
                            setEmptyCacheFor(id, compilationCache);
                            reject(new Error(error));
                        }
                    });
                } catch (e: any) {
                    setEmptyCacheFor(id, compilationCache);
                    reject(e);
                }
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

            // If configured, connect to dev server websocket and trigger reloads
            if (useDevServer) {
                (async () => {
                    try {
                        const discovered = await discoverDevServer();
                        if (!discovered) return;
                        const wsUrl = `ws://${discovered.domain}:${discovered.httpPort}/ws`;
                        // Dynamically require 'ws' to avoid hard dependency
                        let WSImpl: any;
                        try {
                            WSImpl = (await import('ws')).default;
                        } catch (_e) {
                            // No ws library; skip hooking up
                            return;
                        }
                        const ws = new WSImpl(wsUrl);
                        ws.on('message', (_data: any) => {
                            // On any message, clear caches and full reload
                            for (const [loadedId] of compilationCache) {
                                compilationCache.set(loadedId, { code: null, isCompiling: false, compilationPromise: null });
                            }
                            server.ws.send({ type: 'full-reload' });
                        });
                        ws.on('close', () => { /* noop */ });
                    } catch (_e) {
                        // ignore
                    }
                })();
            }
        },
    };
}

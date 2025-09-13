// Vite plugin for Elm Dev - ESM build
import { spawn } from 'node:child_process';
import path from 'node:path';

function setEmptyCacheFor(id, compilationCache) {
    const moduleState = {
        code: null,
        isCompiling: false,
        compilationPromise: null
    };
    compilationCache.set(id, moduleState);
    return moduleState;
}

function emptyAllCache(compilationCache) {
    for (const [id] of compilationCache) {
        setEmptyCacheFor(id, compilationCache);
    }
}

async function discoverDevServer() {
    return new Promise((resolve) => {
        const child = spawn('elm-dev', ['daemon', 'status'], { shell: true });
        let out = '';
        let err = '';
        child.stdout.on('data', (d) => { out += d.toString(); });
        child.stderr.on('data', (d) => { err += d.toString(); });
        child.on('close', () => {
            try {
                const json = JSON.parse(out);
                if (json && json.http && json.http.domain && json.http.port) {
                    resolve({ domain: json.http.domain, httpPort: json.http.port });
                } else if (json && json.httpPort) {
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

async function rpc(serverInfo, method, params) {
    const body = { jsonrpc: '2.0', id: 1, method, params };
    const url = `http://${serverInfo.domain}:${serverInfo.httpPort}/`;
    const res = await fetch(url, { method: 'POST', headers: { 'content-type': 'application/json' }, body: JSON.stringify(body) });
    if (!res.ok) throw new Error(`Dev server HTTP ${res.status}`);
    const data = await res.json();
    if (data && data.result !== undefined) return data.result;
    throw new Error(data && data.error ? JSON.stringify(data.error) : 'Invalid JSON-RPC response');
}

function wrapCompiledElmCode(id, code) {
    const elmModuleName = guessElmModuleName(id);
    const wrappedCode = `\nconst scope = {};\nfunction start() {\n    ${code}\n}\nstart.call(scope);\nexport default scope.Elm.${elmModuleName};\n`;
    return wrappedCode;
}

async function compileWithDevServer(id, debug, optimize, serverInfo) {
    const options = {
        dir: process.cwd(),
        file: path.isAbsolute(id) ? id : path.resolve(id),
        debug,
        optimize,
    };

    const result = await rpc(serverInfo, 'js', options);
    if (!result || !result.compiled || !result.code) throw new Error('Compilation failed');

    const wrappedCode = wrapCompiledElmCode(id, result.code);
    return wrappedCode;
}

async function compileWithCli(id, debug, optimize) {
    const args = ['make', path.join('.', id), '--output=stdout'];
    if (debug) args.push('--debug');
    if (optimize) args.push('--optimize');
    return await new Promise((resolve, reject) => {
        const elmDev = spawn('elm-dev', args, { shell: true });
        let output = '';
        let error = '';
        elmDev.stdout.on('data', (data) => { output += data.toString(); });
        elmDev.stderr.on('data', (data) => { error += data.toString(); });
        elmDev.on('close', (code) => {
            if (code === 0) {
                const wrappedCode = wrapCompiledElmCode(id, output);
                resolve(wrappedCode);
            } else {
                reject(new Error(error));
            }
        });
    });
}

export default function elmDevPlugin(options = {}) {
    const { debug = false, optimize = false, useDevServer = true } = options;
    const compilationCache = new Map();
    let devServer = null;

    return {
        name: 'vite-plugin-elm-dev',
        enforce: 'pre',

        async resolveId(id) {
            if (id.endsWith('.elm')) {
                return id;
            }
            return null;
        },

        async load(id) {
            if (!id.endsWith('.elm')) {
                return null;
            }

            let moduleState = compilationCache.get(id);
            if (!moduleState) {
                moduleState = setEmptyCacheFor(id, compilationCache);
            }

            if (moduleState.code) {
                return moduleState.code;
            }

            if (moduleState.isCompiling && moduleState.compilationPromise) {
                return moduleState.compilationPromise;
            }

            moduleState.isCompiling = true;
            const compilationPromise = new Promise(async (resolve, reject) => {
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
                        const wrappedCode = await compileWithDevServer(id, debug, optimize, devServer);
                        moduleState.code = wrappedCode;
                        moduleState.isCompiling = false;
                        moduleState.compilationPromise = null;
                        resolve(wrappedCode);
                        return;
                    }

                    const wrappedCode = await compileWithCli(id, debug, optimize);
                    moduleState.code = wrappedCode;
                    moduleState.isCompiling = false;
                    moduleState.compilationPromise = null;
                    resolve(wrappedCode);
                } catch (e) {
                    setEmptyCacheFor(id, compilationCache);
                    reject(e);
                }
            });

            moduleState.compilationPromise = compilationPromise;
            return compilationPromise;
        },

        configureServer(server) {
            server.watcher.add('**/*.elm');

            server.watcher.on('change', (file) => {
                if (file.endsWith('.elm') && !file.includes('elm-stuff')) {
                    for (const [loadedId] of compilationCache) {
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
                            // eslint-disable-next-line no-console
                            console.log('Module not found in moduleGraph', modules);
                        }
                    }

                    server.ws.send({ type: 'full-reload' });
                }
            });

            server.watcher.add('elm.json');
            server.watcher.add('elm.generate.json');
            server.watcher.on('change', (file) => {
                if (file === 'elm.json') {
                    emptyAllCache(compilationCache);
                    server.ws.send({ type: 'full-reload' });
                }
            });

            if (useDevServer) {
                (async () => {
                    try {
                        const discovered = await discoverDevServer();
                        if (!discovered) return;
                        const wsUrl = `ws://${discovered.domain}:${discovered.httpPort}/ws`;
                        let WSImpl;
                        try {
                            WSImpl = (await import('ws')).default;
                        } catch (_e) {
                            return;
                        }
                        const ws = new WSImpl(wsUrl);
                        ws.on('message', (_data) => {
                            for (const [loadedId] of compilationCache) {
                                compilationCache.set(loadedId, { code: null, isCompiling: false, compilationPromise: null });
                            }
                            server.ws.send({ type: 'full-reload' });
                        });
                        ws.on('close', () => { });
                    } catch (_e) {
                        // ignore
                    }
                })();
            }
        },
    };
}

function guessElmModuleName(id) {
    const parts = id.split(path.sep).reverse();
    const moduleNameParts = [];
    for (const part of parts) {
        if (/^[A-Z]/.test(part.replace('.elm', ''))) {
            moduleNameParts.push(part.replace('.elm', ''));
        } else {
            break;
        }
    }
    return moduleNameParts.reverse().join('.');
}



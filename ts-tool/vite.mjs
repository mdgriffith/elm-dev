// Vite plugin for Elm Dev - ESM build
import { spawn } from 'node:child_process';
import path from 'node:path';
// import { toColoredTerminalOutput } from './elm-errors-render.js';

function setEmptyCacheFor(id, compilationCache) {
    const moduleState = {
        code: null,
        isCompiling: false,
        compilationPromise: null
    };
    compilationCache.set(id, moduleState);
    return moduleState;
}

const loggingEnabled = false;
function log(...args) {
    if (loggingEnabled) console.log(...args);
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
                    if (process.env.NODE_ENV !== 'production') {
                        log('discovered dev server', json.http.domain, json.http.port);
                    }
                    resolve({ domain: json.http.domain, httpPort: json.http.port });
                } else if (json && json.httpPort) {
                    if (process.env.NODE_ENV !== 'production') {
                        log('discovered dev server (legacy key) 127.0.0.1', json.httpPort);
                    }
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

// Fetch compiled JS is inlined into compileWithDevServer to allow returning rich errors

function wrapCompiledElmCode(id, code) {
    const elmModuleName = guessElmModuleName(id);
    const wrappedCode = `


function start() {
    ${code}
}
start.call(window);
export default window.Elm.${elmModuleName};

if (import.meta.hot) {
    import.meta.hot.accept(() => {});
}
`;
    return wrappedCode;
}

async function compileWithDevServer(id, debug, optimize, serverInfo) {
    const options = {
        dir: process.cwd(),
        // The dev server expects file paths relative to the project root (dir)
        file: path.isAbsolute(id) ? path.relative(process.cwd(), id) : id,
        debug,
        optimize,
    };

    const params = new URLSearchParams({
        dir: options.dir,
        file: options.file,
        debug: options.debug ? 'true' : 'false',
        optimize: options.optimize ? 'true' : 'false',
    });
    const url = `http://${serverInfo.domain}:${serverInfo.httpPort}/dev/js?${params.toString()}`;
    const res = await fetch(url, { method: 'GET' });
    if (!res.ok) {
        const payload = await res.text();
        return { error: payload || 'Compilation failed' };
    }

    const compiledJs = await res.text();
    if (!compiledJs || compiledJs.length === 0) {
        return { error: 'Empty response from dev server' };
    }

    const wrappedCode = wrapCompiledElmCode(id, compiledJs);
    return { code: wrappedCode };
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
            if (loggingEnabled) log('load', id);

            let moduleState = compilationCache.get(id);
            if (!moduleState) {
                moduleState = setEmptyCacheFor(id, compilationCache);
            }

            if (moduleState.code) {
                if (loggingEnabled) log('cache hit for', id);
                return moduleState.code;
            }

            if (moduleState.isCompiling && moduleState.compilationPromise) {
                if (loggingEnabled) log('already compiling, returning promise for', id);
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
                        if (loggingEnabled) log('compiling with dev server', id);
                        const result = await compileWithDevServer(id, debug, optimize, devServer);
                        if (result && result.error) {
                            setEmptyCacheFor(id, compilationCache);
                            reject(new Error(result.error));
                            return;
                        }
                        const wrappedCode = result && result.code ? result.code : null;
                        if (!wrappedCode) {
                            setEmptyCacheFor(id, compilationCache);
                            reject(new Error('Compilation failed'));
                            return;
                        }
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

        async handleHotUpdate(ctx) {
            const { file, server, modules } = ctx;
            if (!file.endsWith('.elm') || file.includes('elm-stuff')) {
                return;
            }

            // HMR strategy: 
            if (useDevServer && devServer) {
                try {
                    if (debug) log('compiling with dev server', file);
                    const result = await compileWithDevServer(file, debug, optimize, devServer);
                    if (result && result.error && loggingEnabled) {
                        log('dev server compile error during HMR', result.error);
                    }
                } catch (_e) {
                }
                return [];
            }

            // No Dev server
            // Fallback when not using the dev server: invalidate cached Elm entries so next load recompiles
            const modulesToReload = new Set();
            for (const [loadedId] of compilationCache) {
                setEmptyCacheFor(loadedId, compilationCache);

                const byId = server.moduleGraph.getModuleById(loadedId);
                if (byId) modulesToReload.add(byId);

                const byFile = server.moduleGraph.getModulesByFile(loadedId);
                if (byFile) {
                    for (const m of byFile) modulesToReload.add(m);
                }
            }

            server.ws.send({ type: 'full-reload' });
            return Array.from(modulesToReload);
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



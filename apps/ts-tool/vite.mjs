// Vite plugin for Elm Dev - ESM build
import { spawn } from 'node:child_process';
import { existsSync } from 'fs';
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

function stripQueryAndHash(id) {
    return id.split('?')[0].split('#')[0];
}

function isElmRequest(id) {
    return stripQueryAndHash(id).endsWith('.elm');
}

function findProjectRootFor(filePath) {
    let dir = path.dirname(filePath);
    while (true) {
        if (existsSync(path.join(dir, 'elm.json'))) {
            return dir;
        }
        const parent = path.dirname(dir);
        if (parent === dir) {
            return path.dirname(filePath);
        }
        dir = parent;
    }
}

const loggingEnabled = false;
function log(...args) {
    if (loggingEnabled) console.log(...args);
}


// Normalize a possibly-relative file path to an absolute path using the Vite root
function toAbsolute(p) {
    if (!p) return p;
    // Strip query/hash which Vite may append
    const cleaned = p.split('?')[0].split('#')[0];
    // Absolute FS path encoded by Vite
    if (cleaned.startsWith('/@fs/')) {
        return cleaned.slice('/@fs/'.length);
    }
    return path.isAbsolute(cleaned) ? cleaned : path.resolve(process.cwd(), cleaned);
}

async function discoverDevServer() {
    return new Promise((resolve) => {
        log("Starting dev server")
        const child = spawn('elm-dev', ['dev', 'start'], { shell: true });
        let out = '';
        let err = '';
        let settled = false;
        const settle = (val) => {
            if (!settled) {
                settled = true;
                resolve(val);
            }
        };
        child.stdout.on('data', (d) => { out += d.toString(); });
        child.stderr.on('data', (d) => { err += d.toString(); });
        child.on('error', (e) => {
            log('error spawning dev server', e);
            settle(null);
        });
        const onFinish = (code, evt) => {
            try {

                let json = null;
                try { json = JSON.parse(out); } catch (_e) { }
                if (json && json.http && json.http.domain && json.http.port) {
                    log('discovered dev server', json.http.domain, json.http.port);
                    settle({ domain: json.http.domain, httpPort: json.http.port });
                    return;
                }
                log('error starting dev server', out, err);
                settle(null);
            } catch (_e) {
                log('error discovering dev server', _e);
                settle(null);
            }
        };
        child.on('exit', (code) => onFinish(code, 'exit'));
        child.on('close', (code) => onFinish(code, 'close'));
    });
}

// Fetch compiled JS is inlined into compileWithDevServer to allow returning rich errors

function wrapCompiledElmCode(id, code) {
    const elmModuleName = guessElmModuleName(id);
    const codeLiteral = JSON.stringify(code);
    const wrappedCode = `
const __elmCode = ${codeLiteral};
const __elmScope = {};
new Function(__elmCode).call(__elmScope);

const __elmExport = __elmScope.Elm && __elmScope.Elm["${elmModuleName}"] ? __elmScope.Elm["${elmModuleName}"] : undefined;

if (!window.Elm || !window.Elm["${elmModuleName}"]) {
    if (__elmScope.Elm) {
        window.Elm = __elmScope.Elm;
    }
} else if (window.Elm && window.Elm.hot && typeof window.Elm.hot.reload === 'function') {
    window.Elm.hot.reload(__elmScope);
}

export default __elmExport;

if (import.meta.hot) {
    import.meta.hot.accept(() => {});
}
`;
    return wrappedCode;
}

function stripInjectedHotJs(compiledJs) {
    const marker = '[elm-dev][hot]';
    const markerIndex = compiledJs.indexOf(marker);
    if (markerIndex === -1) {
        return compiledJs;
    }

    const startIndex = compiledJs.lastIndexOf('(function () {', markerIndex);
    const endIndex = compiledJs.indexOf('})();', markerIndex);
    if (startIndex === -1 || endIndex === -1) {
        return compiledJs;
    }

    return `${compiledJs.slice(0, startIndex)}${compiledJs.slice(endIndex + 5)}`;
}

async function compileWithDevServer(id, debug, optimize, serverInfo) {
    // Always resolve to an absolute path for the dev server
    const absoluteId = toAbsolute(id);
    const projectRoot = findProjectRootFor(absoluteId);

    const options = {
        dir: projectRoot,
        file: path.relative(projectRoot, absoluteId),
        debug,
        optimize,
    };
    log('compileWithDevServer', serverInfo, options);

    const params = new URLSearchParams({
        dir: options.dir,
        file: options.file,
        debug: options.debug ? 'true' : 'false',
        optimize: options.optimize ? 'true' : 'false',
    });
    const url = `http://${serverInfo.domain}:${serverInfo.httpPort}/dev/js?${params.toString()}`;
    const res = await fetch(url, { method: 'GET' });
    if (!res.ok) {
        log('FAILED', res.status, res.statusText);
        const payload = await res.text();
        return { error: payload || 'Compilation failed' };
    }

    const compiledJs = await res.text();
    if (!compiledJs || compiledJs.length === 0) {
        return { error: 'Empty response from dev server' };
    }

    const wrappedCode = wrapCompiledElmCode(absoluteId, stripInjectedHotJs(compiledJs));
    return { code: wrappedCode };
}

async function notifyFileChanged(file, serverInfo) {
    // Always send absolute paths to the dev server
    const absolutePath = toAbsolute(file);
    const params = new URLSearchParams({
        path: absolutePath,
    });
    log('notifyFileChanged', absolutePath);
    const url = `http://${serverInfo.domain}:${serverInfo.httpPort}/dev/fileChanged?${params.toString()}`;
    try {
        const res = await fetch(url, { method: 'GET' });
        if (!res.ok) {
            if (loggingEnabled) {
                const payload = await res.text();
                log('fileChanged returned non-OK', res.status, payload);
            }
            return { ok: false, compiled: false };
        }

        const payload = await res.json().catch(() => null);
        const compiled = Boolean(payload && payload.compiled);
        return { ok: true, compiled };
    } catch (e) {
        if (loggingEnabled) log('fileChanged request failed', e);
        return { ok: false, compiled: false };
    }
}

async function compileWithCli(id, debug, optimize) {
    // Always compile using an absolute file path
    const absoluteId = toAbsolute(id);
    const args = ['make', absoluteId, '--output=stdout'];
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
                const wrappedCode = wrapCompiledElmCode(absoluteId, output);
                resolve(wrappedCode);
            } else {
                reject(new Error(error));
            }
        });
    });
}

function invalidateElmModules(server, compilationCache) {
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

    for (const moduleNode of modulesToReload) {
        server.moduleGraph.invalidateModule(moduleNode);
    }

    return Array.from(modulesToReload);
}

export default function elmDevPlugin(options = {}) {
    const { debug = false, optimize = false, useDevServer = true } = options;
    const compilationCache = new Map();
    let devServer = null;

    async function compileElmModule(id) {
        const cacheId = toAbsolute(id);

        let moduleState = compilationCache.get(cacheId);
        if (!moduleState) {
            moduleState = setEmptyCacheFor(cacheId, compilationCache);
        }

        if (moduleState.code) {
            if (loggingEnabled) log('cache hit for', cacheId);
            return moduleState.code;
        }

        if (moduleState.isCompiling && moduleState.compilationPromise) {
            if (loggingEnabled) log('already compiling, returning promise for', cacheId);
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
                    if (loggingEnabled) log('compiling with dev server', cacheId);
                    const result = await compileWithDevServer(cacheId, debug, optimize, devServer);
                    if (result && result.error) {
                        setEmptyCacheFor(cacheId, compilationCache);
                        reject(new Error(result.error));
                        return;
                    }
                    const wrappedCode = result && result.code ? result.code : null;
                    if (!wrappedCode) {
                        setEmptyCacheFor(cacheId, compilationCache);
                        reject(new Error('Compilation failed'));
                        return;
                    }
                    moduleState.code = wrappedCode;
                    moduleState.isCompiling = false;
                    moduleState.compilationPromise = null;
                    resolve(wrappedCode);
                    return;
                }

                log('compiling with cli', cacheId);
                const wrappedCode = await compileWithCli(cacheId, debug, optimize);
                moduleState.code = wrappedCode;
                moduleState.isCompiling = false;
                moduleState.compilationPromise = null;
                resolve(wrappedCode);
            } catch (e) {
                setEmptyCacheFor(cacheId, compilationCache);
                reject(e);
            }
        });

        moduleState.compilationPromise = compilationPromise;
        return compilationPromise;
    }

    return {
        name: 'vite-plugin-elm-dev',
        enforce: 'pre',

        // Resolve Elm module ids to filesystem paths using Vite/Rollup's resolver.
        async resolveId(id, importer) {
            if (!isElmRequest(id)) return null;
            const cleanedId = stripQueryAndHash(id);
            const resolved = await this.resolve(cleanedId, importer, { skipSelf: true });
            return resolved ? resolved.id : cleanedId;
        },

        async load(id) {
            if (!isElmRequest(id)) {
                return null;
            }
            if (loggingEnabled) log('load', id);
            return compileElmModule(id);
        },

        async handleHotUpdate(ctx) {
            const { file, server, modules } = ctx;
            if (!file.endsWith('.elm') || file.includes('elm-stuff')) {
                return;
            }

            if (useDevServer && devServer) {
                await notifyFileChanged(file, devServer);
            }

            const modulesToReload = invalidateElmModules(server, compilationCache);
            return modulesToReload.length > 0 ? modulesToReload : modules;
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

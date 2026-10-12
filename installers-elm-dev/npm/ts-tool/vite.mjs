// Vite plugin for Elm Dev - ESM build
import { spawn } from 'node:child_process';
import { existsSync } from 'node:fs';
import { readFile, readdir } from 'node:fs/promises';
import { createRequire } from 'node:module';
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

function elmDevExecutable() {
    if (process.platform === 'win32') {
        // npm's .cmd shim cannot be spawned directly. Use its native binary.
        try {
            return createRequire(import.meta.url).resolve(`@elm_dev_binaries/win32_${process.arch}/elm-dev.exe`);
        } catch {
            // Standalone installations can provide elm-dev.exe on PATH.
        }
    }
    return 'elm-dev';
}

async function discoverDevServer() {
    return new Promise((resolve) => {
        log("Starting dev server")
        const child = spawn(elmDevExecutable(), ['dev', 'start']);
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

if (!window.Elm) {
    window.Elm = __elmScope.Elm || {};
} else if (__elmScope.Elm && !window.Elm["${elmModuleName}"]) {
    Object.assign(window.Elm, __elmScope.Elm);
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

function debuggerModeFor(mode, advancedDebugger) {
    if (mode !== 'debug') return 'none';
    return advancedDebugger ? 'elm-dev' : 'elm';
}

function normalizeMode(options) {
    if (options.mode !== undefined && options.mode !== null) {
        const mode = String(options.mode);
        if (mode === 'debug' || mode === 'dev' || mode === 'optimize' || mode === 'O2' || mode === 'O3') {
            return mode;
        }
        throw new Error(`Invalid elm-dev Vite plugin mode: ${mode}`);
    }

    return 'dev';
}

function modeDebug(mode) {
    return mode === 'debug';
}

function addModeCliFlag(args, mode) {
    switch (mode) {
        case 'debug':
            args.push('--debug');
            break;
        case 'optimize':
            args.push('--optimize');
            break;
        case 'O2':
            args.push('--O2');
            break;
        case 'O3':
            args.push('--O3');
            break;
        case 'dev':
            break;
        default:
            throw new Error(`Invalid elm-dev Vite plugin mode: ${mode}`);
    }
}

async function compileWithDevServer(id, mode, advancedDebugger, serverInfo) {
    // Always resolve to an absolute path for the dev server
    const absoluteId = toAbsolute(id);
    const projectRoot = findProjectRootFor(absoluteId);
    const debug = modeDebug(mode);

    const options = {
        dir: projectRoot,
        file: path.relative(projectRoot, absoluteId),
        mode,
        debugger: debuggerModeFor(mode, advancedDebugger),
    };
    log('compileWithDevServer', serverInfo, options);

    const params = new URLSearchParams({
        dir: options.dir,
        file: options.file,
        mode: options.mode,
        debug: debug ? 'true' : 'false',
        optimize: options.mode === 'optimize' ? 'true' : 'false',
        debugger: options.debugger,
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

async function compileWithCli(id, mode) {
    return wrapCompiledElmCode(id, await compileCliSource(id, mode));
}

async function compileCliSource(id, mode) {
    // Always compile using an absolute file path
    const absoluteId = toAbsolute(id);
    const args = ['make', absoluteId, '--output=stdout'];
    addModeCliFlag(args, mode);
    return await new Promise((resolve, reject) => {
        const elmDev = spawn(elmDevExecutable(), args, { cwd: findProjectRootFor(absoluteId) });
        let output = '';
        let error = '';
        elmDev.stdout.on('data', (data) => { output += data.toString(); });
        elmDev.stderr.on('data', (data) => { error += data.toString(); });
        elmDev.on('error', (err) => {
            reject(new Error(`Could not run elm-dev for ${absoluteId}: ${err.message}`));
        });
        elmDev.on('close', (code, signal) => {
            if (code === 0) {
                if (!output.trim()) {
                    reject(new Error(`elm-dev returned empty JavaScript for ${absoluteId}`));
                } else {
                    resolve(output);
                }
            } else {
                reject(new Error(`elm-dev failed compiling ${absoluteId} (${signal || `exit ${code}`}):\n${error || output || 'No compiler diagnostics were returned.'}`));
            }
        });
    });
}

// Intentionally use a simple matcher for conventional Elm module headers.
// Multiline headers, comments between header tokens, and module declarations
// inside block comments are rare enough not to justify a full Elm lexer here.
// This does not parse comments; the compiler still validates the Elm source.
async function readElmModuleName(id) {
    const source = await readFile(id, 'utf8');
    const match = /^(?:port[ \t]+|effect[ \t]+)?module[ \t]+([\p{Lu}][\p{L}\p{N}_]*(?:\.[\p{Lu}][\p{L}\p{N}_]*)*)(?=[ \t\r\n]|$)/mu.exec(source);
    if (!match) {
        throw new Error(
            `Could not extract the Elm module name from ${id}.\n`
            + 'The Vite plugin uses a simple header matcher: start the declaration at column 1, '
            + 'with "module Main" or "port module Main" on the same line '
            + '(nested names such as Admin.Widget are supported). '
            + 'Separate the keywords and module name with spaces or tabs, without intervening comments.\n'
            + 'Use a conventional header such as "module Main exposing (main)". '
            + 'This matcher does not parse block comments, so avoid module-declaration examples inside them. '
            + 'These are plugin constraints; the Elm compiler accepts more flexible header formatting.'
        );
    }
    return match[1];
}

function objectField(node, name) {
    return node?.type === 'ObjectExpression'
        ? node.properties.find((property) => property.type === 'Property'
            && !property.computed && property.kind === 'init'
            && (property.key.name ?? property.key.value) === name)?.value
        : undefined;
}

function wrapProductionCode(context, id, moduleName, code) {
    // Validate the compiler's export table, without evaluating or rewriting its
    // program. Fail closed if the compiler output contract ever changes.
    let ast;
    try {
        ast = context.parse(code);
    } catch (error) {
        throw new Error(`Invalid JavaScript from elm-dev for ${id}: ${error.message}`);
    }
    const invocation = ast.body.length === 1 && ast.body[0].expression;
    const body = invocation?.type === 'CallExpression'
        && invocation.callee.type === 'FunctionExpression'
        && invocation.arguments.length === 1
        && invocation.arguments[0].type === 'ThisExpression'
        ? invocation.callee.body.body : [];
    const exportCalls = body.filter((statement) => statement.type === 'ExpressionStatement'
        && statement.expression.type === 'CallExpression'
        && statement.expression.callee.type === 'Identifier'
        && statement.expression.callee.name === '_Platform_export');
    let entry = exportCalls.length === 1 ? exportCalls[0].expression.arguments[0] : undefined;
    for (const segment of moduleName.split('.')) entry = objectField(entry, segment);
    const init = objectField(entry, 'init');
    // Elm emits a function for program mains, or a call to VirtualDom.init for
    // Html mains. The compiler checks the type of main before generating either.
    if (!init || !['FunctionExpression', 'CallExpression'].includes(init.type)) {
        throw new Error(`Invalid Elm export for ${id}: expected Elm.${moduleName}.init in compiler output`);
    }
    const access = moduleName.split('.').map((part) => `[${JSON.stringify(part)}]`).join('');
    return `const __elmScope = {};\n(function () {\n${code}\n}).call(__elmScope);\nexport default __elmScope.Elm${access};\n`;
}

async function watchElmSources(context, id) {
    const root = findProjectRootFor(id);
    const configFile = path.join(root, 'elm.json');
    context.addWatchFile(configFile);
    const config = JSON.parse(await readFile(configFile, 'utf8'));
    async function watchDirectory(directory) {
        context.addWatchFile(directory);
        for (const entry of await readdir(directory, { withFileTypes: true })) {
            const filename = path.join(directory, entry.name);
            if (entry.isDirectory() && !['elm-stuff', 'node_modules', '.git'].includes(entry.name)) {
                await watchDirectory(filename);
            } else if (entry.isFile() && entry.name.endsWith('.elm')) {
                context.addWatchFile(filename);
            }
        }
    }
    for (const source of config['source-directories'] || ['src']) {
        await watchDirectory(path.resolve(root, source));
    }
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
    const { useDevServer = true, advancedDebugger = false } = options;
    let mode = normalizeMode(options);
    let isBuild = false;
    const compilationCache = new Map();
    // Vite transforms entries concurrently. Serialize CLI invocations per project
    // because they write the same elm-stuff cache, even for independent entries.
    const productionCompilations = new Map();
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
                    const result = await compileWithDevServer(cacheId, mode, advancedDebugger, devServer);
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
                const wrappedCode = await compileWithCli(cacheId, mode);
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

        configResolved(config) {
            isBuild = config.command === 'build';
            mode = normalizeMode({ ...options, mode: options.mode ?? (isBuild ? 'optimize' : 'dev') });
            if (isBuild && (mode === 'debug' || advancedDebugger)) {
                throw new Error('elm-dev: vite build does not support the Elm debugger or advancedDebugger. Use mode: "dev" for Debug.log support, or an optimized mode.');
            }
        },

        buildStart() {
            compilationCache.clear();
            productionCompilations.clear();
        },

        async buildEnd() {
            await Promise.allSettled(productionCompilations.values());
        },

        shouldTransformCachedModule({ id }) {
            // Build-watch must recompile when an Elm dependency changes, too.
            if (isBuild && isElmRequest(id)) return true;
        },

        // Resolve Elm module ids to filesystem paths using Vite/Rollup's resolver.
        async resolveId(id, importer) {
            if (!isElmRequest(id)) return null;
            const cleanedId = stripQueryAndHash(id);
            const resolved = await this.resolve(cleanedId, importer, { skipSelf: true });
            if (isBuild && (!resolved || resolved.external)) {
                this.error(`Could not resolve Elm entry point ${cleanedId}${importer ? ` imported by ${importer}` : ''}`);
            }
            return resolved ? resolved.id : cleanedId;
        },

        async load(id) {
            if (!isElmRequest(id)) {
                return null;
            }
            if (loggingEnabled) log('load', id);
            if (isBuild) return readFile(toAbsolute(id), 'utf8');
            return compileElmModule(id);
        },

        async transform(_source, id) {
            if (!isElmRequest(id)) return null;
            if (isBuild) {
                const absoluteId = toAbsolute(id);
                // Watch dependencies even when the initial compilation fails.
                if (this.meta.watchMode) await watchElmSources(this, absoluteId);
                // Compile first so source errors retain the compiler diagnostics.
                const root = findProjectRootFor(absoluteId);
                const previous = productionCompilations.get(root) || Promise.resolve();
                const compilation = previous.then(() => compileCliSource(absoluteId, mode));
                productionCompilations.set(root, compilation);
                const code = await compilation;
                const moduleName = await readElmModuleName(absoluteId);
                return { code: wrapProductionCode(this, absoluteId, moduleName, code), map: null };
            }
            return null;
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

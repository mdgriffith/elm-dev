import assert from 'node:assert/strict';
import { once } from 'node:events';
import { createServer } from 'node:http';
import { chmod, mkdir, mkdtemp, readFile, realpath, rm, writeFile } from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import test from 'node:test';
import { chromium } from 'playwright';
import { build, createServer as createViteServer } from 'vite';
import elmDevPlugin from '../../ts-tool/vite.mjs';

const elmJson = {
    type: 'application',
    'source-directories': ['src'],
    'elm-version': '0.19.1',
    dependencies: {
        direct: { 'elm/browser': '1.0.2', 'elm/core': '1.0.5', 'elm/html': '1.0.0', 'elm/json': '1.1.3' },
        indirect: { 'elm/time': '1.0.0', 'elm/url': '1.0.0', 'elm/virtual-dom': '1.0.3' },
    },
    'test-dependencies': { direct: {}, indirect: {} },
};

function elmSource(name, debug = false) {
    return `{- A leading {- nested -} comment -}
port module ${name} exposing (main)

import Browser
import Html
import Html.Attributes

port incoming : (String -> msg) -> Sub msg
port outgoing : String -> Cmd msg

main : Program String String String
main =
    Browser.element
        { init = \\flags -> ( flags, Cmd.none )
        , subscriptions = \\_ -> incoming identity
        , update = \\value _ -> ( ${debug ? 'Debug.log "received" value' : 'value'}, outgoing value )
        , view = \\value ->
            Html.div
                (if value == "" then [] else [ Html.Attributes.style "--accent" value ])
                [ Html.text value ]
        }
`;
}

async function fixture(t, { compiler } = {}) {
    // Spaces and uppercase directory names exercise argument passing and module
    // resolution independently of the process cwd (which is a different project).
    const root = await realpath(await mkdtemp(path.join(os.tmpdir(), 'Elm Production ')));
    const oldPath = process.env.PATH;
    t.after(async () => {
        process.env.PATH = oldPath;
        await rm(root, { recursive: true, force: true });
    });
    await mkdir(path.join(root, 'src', 'Admin'), { recursive: true });
    await mkdir(path.join(root, 'bin'));
    await writeFile(path.join(root, 'elm.json'), JSON.stringify(elmJson));
    await writeFile(path.join(root, 'src', 'Main.elm'), elmSource('Main'));
    await writeFile(path.join(root, 'src', 'Admin', 'Widget.elm'), elmSource('Admin.Widget'));
    await writeFile(path.join(root, 'index.html'), `<!doctype html>
<meta http-equiv="Content-Security-Policy" content="default-src 'self'; script-src 'self'; style-src 'unsafe-inline'">
<div id="main"><div></div></div><div id="widget"><div></div></div>
<script type="module" src="/src/main.js"></script>`);
    await writeFile(path.join(root, 'src', 'main.js'), `
import Main from './Main.elm';
window.main = Main.init({ node: document.querySelector('#main > div'), flags: 'red' });
window.replies = [];
window.main.ports.outgoing.subscribe(value => window.replies.push(value));
window.loadWidget = async () => {
    const { default: Widget } = await import('./Admin/Widget.elm');
    window.widget = Widget.init({ node: document.querySelector('#widget > div'), flags: 'green' });
};
`);
    const log = path.join(root, 'compiler-calls.jsonl');
    const executable = path.join(root, 'bin', 'elm-dev');
    await writeFile(executable, `#!${process.execPath}
const fs = require('node:fs');
const args = process.argv.slice(2);
fs.appendFileSync(${JSON.stringify(log)}, JSON.stringify({args, cwd: process.cwd()}) + '\\n');
if (args[0] !== 'make') throw new Error('Production tried starting a daemon');
${compiler ?? `const { spawnSync } = require('node:child_process');
const result = spawnSync('elm-dev', args, { env: { ...process.env, PATH: ${JSON.stringify(oldPath)} }, stdio: 'inherit' });
if (result.error) throw result.error;
process.exit(result.status ?? 1);`}
`);
    await chmod(executable, 0o755);
    process.env.PATH = `${path.join(root, 'bin')}${path.delimiter}${oldPath}`;
    return {
        root,
        calls: async () => (await readFile(log, 'utf8')).trim().split('\n').map(JSON.parse),
        build: (options = {}, extra = {}) => build({
            root,
            configFile: false,
            logLevel: 'silent',
            mode: 'staging',
            plugins: [elmDevPlugin(options)],
            build: { write: false, minify: 'esbuild', ...extra },
        }),
    };
}

function jsOutput(result) {
    return result.output.filter((entry) => entry.type === 'chunk').map((entry) => entry.code).join('\n');
}

function nextBuild(watcher) {
    return new Promise((resolve, reject) => {
        let buildError;
        const timer = setTimeout(() => finish(new Error('Timed out waiting for build watch')), 20000);
        function finish(error) {
            clearTimeout(timer);
            watcher.off('event', onEvent);
            if (error) reject(error); else resolve();
        }
        function onEvent(event) {
            if (event.code === 'ERROR') buildError = event.error;
            if (event.code === 'END') finish(buildError);
        }
        watcher.on('event', onEvent);
    });
}

async function openBuild(t, result) {
    const assets = new Map(result.output.map((entry) => [
        `/${entry.fileName}`, entry.type === 'chunk' ? entry.code : entry.source,
    ]));
    const server = createServer((req, res) => {
        const url = req.url === '/' ? '/index.html' : req.url;
        if (!assets.has(url)) { res.writeHead(404); res.end(); return; }
        res.setHeader('Content-Type', url.endsWith('.html') ? 'text/html' : 'text/javascript');
        res.end(assets.get(url));
    });
    server.listen(0, '127.0.0.1');
    await once(server, 'listening');
    t.after(() => new Promise((resolve) => server.close(resolve)));
    const browser = await chromium.launch({ headless: true });
    t.after(() => browser.close());
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', (error) => errors.push(error.message));
    await page.addInitScript(() => {
        window.cspViolations = [];
        document.addEventListener('securitypolicyviolation', (event) => {
            window.cspViolations.push(event.violatedDirective);
        });
    });
    await page.goto(`http://127.0.0.1:${server.address().port}/`);
    await page.waitForFunction(() => window.main?.ports);
    return { page, errors };
}

test('optimized builds are static, minifiable, isolated and work under CSP with ports and custom properties', async (t) => {
    const f = await fixture(t);
    const plain = await f.build({}, { minify: false });
    const minified = await f.build();
    const source = jsOutput(plain);
    const compact = jsOutput(minified);
    assert.match(source, /function _VirtualDom/);
    assert.doesNotMatch(compact, /function _VirtualDom/);
    assert.ok(compact.length < source.length * 0.8, 'Vite must minify the Elm program, not just the adapter');
    for (const code of [source, compact]) {
        assert.doesNotMatch(code, /new Function|import\.meta\.hot|window\.Elm|\[elm-dev\]\[hot\]/);
    }
    assert.equal(minified.output.filter((entry) => entry.type === 'chunk').length, 2, 'dynamic entry stays independently loadable');
    const calls = await f.calls();
    assert.equal(calls.length, 4);
    for (const call of calls) {
        assert.equal(call.cwd, f.root);
        assert.equal(call.args[0], 'make');
        assert.ok(call.args.includes('--optimize'));
        assert.equal(call.args.filter((arg) => arg.endsWith('.elm')).length, 1);
    }
    const { page, errors } = await openBuild(t, minified);
    const accent = () => page.$eval('#main > div', (node) => node.style.getPropertyValue('--accent'));
    assert.equal(await accent(), 'red');
    await page.evaluate(() => window.main.ports.incoming.send('blue'));
    await page.waitForFunction(() => document.querySelector('#main > div').style.getPropertyValue('--accent') === 'blue');
    assert.deepEqual(await page.evaluate(() => window.replies), ['blue']);
    await page.evaluate(() => window.loadWidget());
    assert.equal(await page.$eval('#widget > div', (node) => node.style.getPropertyValue('--accent')), 'green');
    assert.equal(await accent(), 'blue');
    await page.evaluate(() => window.main.ports.incoming.send(''));
    await page.waitForFunction(() => document.querySelector('#main > div').style.getPropertyValue('--accent') === '');
    assert.equal(await page.evaluate(() => Object.hasOwn(window, 'Elm')), false);
    assert.deepEqual(await page.evaluate(() => window.cspViolations), []);
    assert.deepEqual(errors, []);
});

test('explicit dev builds allow Debug.log without a debugger or unsafe-eval', async (t) => {
    const f = await fixture(t);
    await writeFile(path.join(f.root, 'src', 'Main.elm'), elmSource('Main', true));
    const result = await f.build({ mode: 'dev' });
    const { page, errors } = await openBuild(t, result);
    await page.evaluate(() => window.main.ports.incoming.send('purple'));
    await page.waitForFunction(() => window.replies.includes('purple'));
    assert.deepEqual(errors, []);
    assert.deepEqual(await page.evaluate(() => window.cspViolations), []);
    for (const call of await f.calls()) assert.equal(call.args.length, 3);
    await assert.rejects(f.build(), /DEBUG REMNANTS|Debug|DEBUG/i);
});

test('real compiler diagnostics and unresolved imports fail the build', async (t) => {
    const f = await fixture(t);
    await writeFile(path.join(f.root, 'src', 'Main.elm'), 'module Main exposing (main)\nmain : Int\nmain = "wrong"\n');
    await assert.rejects(f.build(), /TYPE MISMATCH/);
    await writeFile(path.join(f.root, 'src', 'Main.elm'), 'module Main exposing (value)\nvalue = 1\n');
    await assert.rejects(f.build(), /main/i);
    await writeFile(path.join(f.root, 'src', 'main.js'), 'import Main from "./Missing.elm"; Main.init();');
    await assert.rejects(f.build(), /Could not resolve Elm entry point.*Missing.elm/);
});

test('build rejects debugger options regardless of Vite mode', async (t) => {
    const f = await fixture(t);
    await assert.rejects(f.build({ mode: 'debug' }), /does not support the Elm debugger/);
    await assert.rejects(f.build({ advancedDebugger: true }), /does not support the Elm debugger/);
    await assert.rejects(f.calls(), /ENOENT/);
});

test('compiler launch, output and export failures are build errors', async (t) => {
    const cases = [
        ['empty output', '', /empty JavaScript/],
        ['invalid JS', 'process.stdout.write("not javascript !!");', /Invalid JavaScript/],
        ['missing export', 'process.stdout.write("(function(scope){ })(this);");', /expected Elm\..*\.init/],
        ['invalid init', 'process.stdout.write("(function(scope){ _Platform_export({Main:{init:42}}); })(this);");', /expected Elm\..*\.init/],
        ['compiler error', 'process.stderr.write("helpful compiler diagnostic"); process.exit(1);', /helpful compiler diagnostic/],
    ];
    for (const [name, compiler, expected] of cases) {
        await t.test(name, async (t) => {
            const f = await fixture(t, { compiler });
            await assert.rejects(f.build(), expected);
        });
    }
    await t.test('missing executable', async (t) => {
        const f = await fixture(t);
        process.env.PATH = path.join(f.root, 'empty-path');
        await assert.rejects(f.build(), /Could not run elm-dev.*ENOENT/s);
    });
});

test('explicit optimization levels reach the compiler', async (t) => {
    for (const mode of ['optimize', 'O2', 'O3']) {
        await t.test(mode, async (t) => {
            const f = await fixture(t);
            await f.build({ mode });
            for (const call of await f.calls()) assert.ok(call.args.includes(`--${mode}`));
        });
    }
});

test('build watch recompiles changed Elm dependencies and supports Html entry points', async (t) => {
    const f = await fixture(t);
    await writeFile(path.join(f.root, 'src', 'main.js'), `import Main from './Main.elm'; Main.init({ node: document.querySelector('#main') });`);
    await writeFile(path.join(f.root, 'src', 'Main.elm'), `module Main exposing (main)
import Html
import Shared
main = Html.text Shared.message
`);
    const shared = path.join(f.root, 'src', 'Shared.elm');
    await writeFile(shared, 'module Shared exposing (message)\nmessage = "BEFORE_WATCH_EDIT"\n');
    const watcher = await f.build({}, { watch: {}, write: true });
    t.after(() => watcher.close());
    await nextBuild(watcher);
    const rebuilt = nextBuild(watcher);
    await writeFile(shared, 'module Shared exposing (message)\nmessage = "AFTER_WATCH_EDIT"\n');
    await rebuilt;
    // A final non-watch build isn't a substitute: inspect the watcher's output.
    const html = await readFile(path.join(f.root, 'dist', 'index.html'), 'utf8');
    const script = /src="([^"]+\.js)"/.exec(html)[1];
    const output = await readFile(path.join(f.root, 'dist', script.replace(/^\//, '')), 'utf8');
    assert.match(output, /AFTER_WATCH_EDIT/);
    assert.doesNotMatch(output, /BEFORE_WATCH_EDIT/);
    assert.ok((await f.calls()).length >= 2);
    await watcher.close();
});

test('build watch recovers after fixing an initially broken Elm dependency', async (t) => {
    const f = await fixture(t);
    await writeFile(path.join(f.root, 'src', 'main.js'), `import Main from './Main.elm'; Main.init({ node: document.querySelector('#main') });`);
    await writeFile(path.join(f.root, 'src', 'Main.elm'), `module Main exposing (main)
import Html
import Shared
main = Html.text Shared.message
`);
    const shared = path.join(f.root, 'src', 'Shared.elm');
    await writeFile(shared, 'module Shared exposing (message)\nmessage : String\nmessage = 42\n');
    const watcher = await f.build({}, { watch: {}, write: true });
    t.after(() => watcher.close());
    await assert.rejects(nextBuild(watcher), /TYPE MISMATCH/);

    const rebuilt = nextBuild(watcher);
    await writeFile(shared, 'module Shared exposing (message)\nmessage : String\nmessage = "RECOVERED_BUILD"\n');
    await rebuilt;
    const html = await readFile(path.join(f.root, 'dist', 'index.html'), 'utf8');
    const script = /src="([^"]+\.js)"/.exec(html)[1];
    const output = await readFile(path.join(f.root, 'dist', script.replace(/^\//, '')), 'utf8');
    assert.match(output, /RECOVERED_BUILD/);
    assert.ok((await f.calls()).length >= 2);
    await watcher.close();
});

test('serve retains its development wrapper and default dev compiler mode', async (t) => {
    const f = await fixture(t);
    const server = await createViteServer({
        root: f.root,
        configFile: false,
        logLevel: 'silent',
        plugins: [elmDevPlugin({ useDevServer: false })],
        server: { watch: null },
    });
    t.after(() => server.close());
    const result = await server.transformRequest('/src/Main.elm');
    assert.match(result.code, /new Function/);
    assert.match(result.code, /import\.meta\.hot/);
    const calls = await f.calls();
    assert.equal(calls.length, 1);
    assert.equal(calls[0].args.length, 3);
    await server.close();
});

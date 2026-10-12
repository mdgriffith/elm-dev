## The Vite Plugin.


Awkwardly enough, the package.json file in here is *not* the one that ships to npm.  That's installers-elm-dev/npm/package.json

## Production builds

```js
import { defineConfig } from 'vite';
import elmDev from 'elm-dev/vite';

export default defineConfig({
    plugins: [elmDev()],
});
```

`vite build` invokes `elm-dev make` directly and embeds its output as static
JavaScript. It does not discover/start a daemon, inject HMR or a debugger, publish
`window.Elm`, or evaluate JavaScript strings. Vite can minify the Elm program, and
the Elm integration does not require CSP `unsafe-eval`.

This behavior is selected by Vite's **command**, including builds with custom
modes such as `vite build --mode staging`.

Compiler modes:

- The default is `dev` for the dev server and `optimize` for builds.
- Builds accept explicit `dev`, `optimize`, `O2`, and `O3`.
- `dev` builds allow `Debug.log`, `Debug.toString`, and `Debug.todo`, with their
  usual behavior, while still using static output without a debugger.
- Builds reject `mode: 'debug'` and `advancedDebugger: true` with an error.
- `useDevServer` only affects development; builds always use the CLI.

Each `.elm` import defaults to that module's initialization API:

```js
import Main from './src/Main.elm';
import Widget from './src/Admin/Widget.elm';

Main.init({ node: document.getElementById('app') });
Widget.init({ node: document.getElementById('widget') });
```

Multiple entry points (including nested module names and dynamic imports) are
compiled **independently**, with private export scopes. This preserves independent
loading but duplicates shared Elm runtime/dependency code. Joint compilation and
runtime deduplication are deferred.

Missing entries, compiler failures, missing executables, empty/invalid JavaScript,
and missing expected exports fail the build. Export validation inspects the
compiler's syntax tree without executing the application or rewriting its output.

## Production tests

From `apps/elm-dev`, run `bun run test:production`. These tests require `elm-dev`
on PATH and Playwright Chromium (`bunx playwright install chromium`). They cover
real optimized builds, CSP, ports, CSS custom-property updates/removal, nested and
dynamic entries, minification, compiler modes, and diagnostic failures. They do
not start or restart the development daemon.

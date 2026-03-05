# Elm Dev Vite HMR Harness

This harness starts a real Vite dev server with the local Elm plugin and mutates files on disk to verify that updates are reflected in served modules.

It also includes a real browser test (Playwright + Chromium) to verify that Elm updates do not force navigation and that in-page state survives an Elm edit.

## Run

From `apps/elm-dev2`:

```bash
npm run test:hmr-harness
```

That script uses `tests/run-hmr-harness.mjs` to reset/start the daemon with bounded command timeouts before running the Node test file.

## What It Covers

- Main Elm module change propagates
- Elm dependency module change propagates
- Rapid consecutive Elm writes settle on latest output
- Elm syntax error surfaces, then recovers after fix
- TypeScript file updates are reflected
- Elm HMR update path does not emit `full-reload`
- Browser state is preserved across an Elm update
- Repeated Elm HMR updates do not hit duplicate `Elm.Main` runtime errors

## Notes

- The fixture is created in a temporary directory for each test run.
- Tests intentionally poll for eventual consistency because file watching and compile pipelines are asynchronous.
- Browser test requires Playwright's Chromium binary (`npx playwright install chromium`).

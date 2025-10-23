# Elm Dev — Release Guide

Practical steps to build, package, and publish:
- Haskell `elm-dev` (built in CI for all platforms)
- Rust `elm-dev-proxy` (built locally)
- NPM packages (per‑platform + root)
- VS Code extension

## Required Dependencies

- Node.js and npm (publish + scripts)
- jq (used by scripts)
- Zig and cargo-zigbuild (Rust cross‑compile for Linux/Windows)
- Rust toolchain via rustup (targets: aarch64-apple-darwin, x86_64-apple-darwin, x86_64-unknown-linux-musl, aarch64-unknown-linux-musl, x86_64-pc-windows-gnu)
- Xcode command line tools (macOS SDK for proxy mac builds)
- bun (to build the VS Code extension) or equivalent tooling to run its build
- vsce (to publish the VS Code extension) and a Marketplace token
- npm auth configured with publish rights for all packages

## CI: Haskell (elm-dev) cross‑platform builds

- Always built by CI. Trigger by pushing to the `distribute` branch.
- CI workflows live in `.github/workflows/` and call `distribution/build-*.sh`.
- They upload “next” artifacts needed by packaging.

Build Elm generator and trigger CI:

```bash
# 1) Run the build script (compile Elm assets used by Haskell build)
./scripts/build-generator-assets.sh

# 2) Commit outputs and push to main (run from repo root)
cd ../..
git switch main
git add \
  ext-generate/generator/dist/run.js \
  ext-generate/generator/dist/interactive-run.js \
  ext-generate/Gen/Javascript.hs \
  ext-generate/Gen/Templates.hs
git commit -m "chore: update generator outputs for release"
git push origin main

# 3) Rebase distribute on main and push to trigger CI
git switch distribute || git switch -c distribute
git fetch origin
git rebase origin/main
git push -f origin distribute
```

## Local: Rust proxy builds

- Always built locally. From repo root run:

```bash
./scripts/build-proxy-binaries.sh
```

This cross‑compiles and copies binaries into `installers-elm-dev/npm/packages/*`.

## Release Procedure

1) Verify versions (must match):
   - `installers-elm-dev/npm/package.json` (root and optionalDependencies)
   - `installers-elm-dev/npm/packages/*/package.json`
   - `distribution/common.sh` (`version=`)
   - `installers-elm-dev/npm/scripts/download-binaries.sh` (`VERSION=`)

2) Trigger CI for Haskell builds and wait for artifacts:
   - Run the build script, commit to main, rebase `distribute` on main, and push `distribute` (see commands above).
   - Confirm CI jobs finished.

3) Build local packages(Rust proxy, npm packages, and VS Code extension):
   - `./installers-elm-dev/build-release-local.sh`
   - Notes: requires `jq`, `node`, and `bun` for VS Code build (or build VS Code manually).

4) Publish per‑platform npm packages:
   - `./installers-elm-dev/publish-platforms.sh`
   - Note: needs npm auth; ensure binaries exist in each `npm/packages/<platform>/`.

5) Publish root npm package:
   - `./installers-elm-dev/publish-root.sh`
   - Note: runs `prepack` to include `ts-tool`.

6) Publish VS Code extension:
   - `./installers-elm-dev/publish-vscode.sh`
   - Notes: requires `vsce` and token; version bump in `apps/vscode/package.json`.

## Building the Rust Proxy Independently

If you need to build the proxy on its own (outside the orchestrator script):

```bash
./scripts/build-proxy-binaries.sh
```

This cross‑compiles and copies binaries into `installers-elm-dev/npm/packages/*`.

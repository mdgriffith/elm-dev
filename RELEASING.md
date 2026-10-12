# Releasing Elm Dev

This is the canonical release process for the `elm-dev` executable, platform
binary packages, and root npm package. Run commands from the repository root
unless a step says otherwise.

The VS Code extension is independently versioned and is not part of every
`elm-dev` release.

## How Releases Are Triggered

Two separate mechanisms are involved:

- Pushing a commit to `distribute` triggers CI builds for the five supported
  platforms. Those workflows upload mutable `elm-dev-next-*` artifacts to
  static hosting.
- A GitHub Release is attached to a Git tag such as `0.4.3`. CI does not create
  this release or tag. The `gh release create` command below creates both the
  tag and release when the tag does not already exist.

The `--target` argument determines which commit the new release tag points to.
Always use the exact commit that successfully built on `distribute`.

## Prerequisites

Local and distribution builds use the Stack version pinned in
`distribution/stack-version`. `stack.yaml` enforces that exact version; update
both files together when upgrading the toolchain. Install it locally with:

```bash
read -r STACK_VERSION < distribution/stack-version
ghcup install stack "$STACK_VERSION" --set
test "$(stack --numeric-version)" = "$STACK_VERSION"
```

After a Stack upgrade, verify Hackage access with a fresh cache as well as running
the normal build checks. An existing package cache can hide outdated Hackage
signing keys:

```bash
STACK_CHECK_ROOT="$(mktemp -d)"
STACK_ROOT="$STACK_CHECK_ROOT" stack update
rm -rf "$STACK_CHECK_ROOT"
```

- npm authentication with publish access to `elm-dev` and
  `@elm_dev_binaries/*`
- GitHub CLI authentication for `mdgriffith/elm-dev`
- Rust, rustup, Zig, and `cargo-zigbuild` for proxy cross-compilation
- Xcode command-line tools on macOS
- Node.js 18 or newer
- Bun only when publishing the independently versioned VS Code extension

Confirm authentication before starting:

```bash
npm whoami --registry=https://registry.npmjs.org/
gh auth status
```

## 1. Prepare The Release Commit

Update every release version and run the metadata check:

```bash
./scripts/check-release-version.sh
```

The check covers:

- `elm-dev.cabal`
- `cabal.project.freeze`
- `distribution/common.sh`
- root and platform npm package manifests
- `installers-elm-dev/npm/scripts/download-binaries.sh`
- the generated project template

Run release validation:

```bash
./scripts/check-linux-docker-sources.sh
stack test elm-dev:elm-dev-tests
npm run test-dependencies:network
```

If generator inputs changed, regenerate and commit their artifacts before
triggering distribution builds:

```bash
./scripts/build-generator-assets.sh
```

Commit and push the release commit to `main`, then trigger distribution CI with
that exact commit:

```bash
git push origin main
git push origin HEAD:distribute
```

Do not force-push `distribute`; advancing it to the release commit is enough to
trigger the workflows.

## 2. Verify Distribution Builds

Wait for all five `distribute` workflows to complete successfully:

```bash
git fetch origin distribute
test "$(git rev-parse HEAD)" = "$(git rev-parse origin/distribute)"
gh run list --repo mdgriffith/elm-dev --branch distribute
```

The expected targets are:

- macOS ARM64
- macOS x64
- Linux ARM64
- Linux x64
- Windows x64

Do not download until all five builds have completed. The download URLs use
mutable `next` names, so downloading while CI is still running can assemble a
mixed release.

## 3. Assemble Local Release Packages

Run the release orchestrator:

```bash
./installers-elm-dev/build-release-local.sh
```

It performs these operations in the required order:

1. Cross-compiles `elm-dev-proxy` for all platforms.
2. Downloads the CI-built `elm-dev` executables.
3. Creates platform package contents and versioned GitHub release archives.
4. Copies `ts-tool` into the root npm package.
5. Builds the VS Code extension when Bun is available.

The proxy build must happen before the download script because the download
script includes proxy executables in the release archives.

## 4. Inspect The Packages

Set the version from the root npm package:

```bash
VERSION="$(node -p "require('./installers-elm-dev/npm/package.json').version")"
```

Validate release archives and record checksums:

```bash
gzip -t installers-elm-dev/npm/releases/"$VERSION"/*.gz
shasum -a 256 installers-elm-dev/npm/releases/"$VERSION"/*.gz
```

Inspect every npm tarball before publishing:

```bash
for package in darwin_arm64 darwin_x64 linux_arm64 linux_x64 win32_x64; do
  (cd "installers-elm-dev/npm/packages/$package" && npm pack --dry-run)
done

(cd installers-elm-dev/npm && npm pack --dry-run)
```

Verify the version has not already been published. npm versions are immutable:

```bash
npm view "elm-dev@$VERSION" version

for package in darwin_arm64 darwin_x64 linux_arm64 linux_x64 win32_x64; do
  npm view "@elm_dev_binaries/$package@$VERSION" version
done
```

An npm `E404` is expected for a new version. If a command prints the requested
version, that package has already been published and cannot be republished.

## 5. Create The GitHub Release

Create the version tag and GitHub Release from the commit that built on
`distribute`, then upload all executable and proxy archives:

```bash
gh release create "$VERSION" \
  installers-elm-dev/npm/releases/"$VERSION"/*.gz \
  --repo mdgriffith/elm-dev \
  --target "$(git rev-parse HEAD)" \
  --title "elm-dev $VERSION" \
  --generate-notes
```

`gh release create` creates the Git tag when it does not already exist. If the
tag and release already exist but need assets, use:

```bash
gh release upload "$VERSION" \
  installers-elm-dev/npm/releases/"$VERSION"/*.gz \
  --repo mdgriffith/elm-dev
```

Create the GitHub Release before npm publication. Unsupported-platform install
errors direct users to the corresponding GitHub release page.

## 6. Publish npm Packages

Publish all platform packages first:

```bash
./installers-elm-dev/publish-platforms.sh
```

Confirm every exact platform version is visible before publishing the root
package:

```bash
for package in darwin_arm64 darwin_x64 linux_arm64 linux_x64 win32_x64; do
  npm view "@elm_dev_binaries/$package@$VERSION" version
done
```

Publish the root package:

```bash
./installers-elm-dev/publish-root.sh
```

Confirm the release and npm tag:

```bash
npm view "elm-dev@$VERSION" version
npm dist-tags ls elm-dev
```

The publish scripts use npm's default `latest` tag. Platform publication is
partially irreversible: if a later package fails, packages published earlier
in the sequence remain published.

## 7. Smoke Test The Published Package

Install into an empty temporary project rather than relying on local package
contents:

```bash
SMOKE_DIR="$(mktemp -d)"
cd "$SMOKE_DIR"
npm init -y
npm install "elm-dev@$VERSION"
./node_modules/.bin/elm-dev --version-full
```

## VS Code Extension

The extension version lives in `apps/vscode/package.json`. Publish it only when
that version has also been intentionally bumped:

```bash
./installers-elm-dev/publish-vscode.sh
```

## Supported architectures

Distributions for the following architectures are currently supported:

| Build                                 | Linking | Supported build hosts                   |
| ------------------------------------- | ------- | --------------------------------------- |
| `elm-dev-[verison]-macos-x86_64`      | Dynamic | `macos-x86_64`, `macos-arm64` (Rosetta) |
| `elm-dev-[verison]-macos-arm64`       | Dynamic | `macos-arm64`                           |
| `elm-dev-[verison]-linux-x86_64-musl` | Static  | `linux-x86_64`/`macos-x86_64` (Docker)  |
| `elm-dev-[verison]-linux-arm64-musl`  | Static  | `linux-arm64`/`macos-arm64` (Docker)    |
| `elm-dev-[verison]-win-x86_64`        | Dynamic | `win-x86_64`                            |

## Building an elm-dev compiler binary

### Pre-requisites

- [ghcup](https://www.haskell.org/ghcup/) (scripts will attempt to install Stack/GHC/Cabal)

How to install these varies depending on your build host. [GHCup](https://www.haskell.org/ghcup/) is a convenient option for trying out multiple versions.

Once you have these dependencies, running the relevant `build-<os>-<aarch>.sh` should result in an elm-dev binary.

## Macos cross-compiling

elm-dev should compile on both `x86_64` (Intel) and `arm64` (M-series) chipset macs.

If you have an `x86_64` mac, you can only build `x86_64` binaries.

If you have an `arm64` mac and [Rosetta 2](https://support.apple.com/en-gb/HT211861) (`softwareupdate --install-rosetta --agree-to-license`), you can also cross-compile the `x86_64` binary, however it will require the prerequisite toolchain in the respective CPU flavour, i.e.

- GHC-arm64 + cabal-arm64 => elm-dev-macos-arm64
- GHC-x86_64 + cabal-x86_64 => elm-dev-macos-x86-64

The `distribution/build-macos-*` scripts are setup to install the required toolchain seperately from any system ones.

## Linux builds with Docker

Docker provides us with the convenience of being able to run encapsulated Linux builds on both Linux and MacOS.

The Docker philosophy of immutable layers is great for reproducibility, but a pain for debugging. To get back some of the benefits of tool-level caching, break up expensive operations whenever possible.

I.e. where `cabal install` run directly will resume based on prior progress (i.e. expensive package compilation), to get similar behaviour via Docker we need to:

```bash
COPY elm-dev.cabal ./                   # only add elm-dev.cabal
RUN cabal build --only-dependencies # single layer for building deps based on elm-dev.cabal only
COPY ...                            # add all remaining project files afterward
RUN cabal build                     # run the actual elm build
```

Without this, even changing the readme would mean Docker decides all the packages need recompiling from scratch.

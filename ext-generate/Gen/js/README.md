JS assets for Gen.Javascript

These files are embedded into the Haskell binary by `Gen.Javascript` at compile time.

How to (re)generate

- Run the top-level script (from repo root or anywhere):

```bash
scripts/build-generator-assets.sh
```

- That script builds the Elm generator, bundles the JS, and copies the outputs here as:
  - `run.js`
  - `interactive-run.js`

Versioning and check-in policy

- These files are intended to be checked into the repository to keep Haskell builds simple and reproducible without requiring the JS toolchain.
- The build script also bumps an embedded `version` string in `ext-generate/Gen/Javascript.hs` to ensure recompilation when assets change.



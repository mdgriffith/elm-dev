# Developing Elm Dev

These are some instructions to get things set up for development.  Just went through this on an M4 Macbook.

1. Install XCode CLI tools: `xcode-select --install`
2. Install GHCup: https://www.haskell.org/ghcup/
3. Install homebrew: https://brew.sh/
   - Install llvm: `brew install llvm`
   - Install pkg-config: `brew install pkg-config`

Once this is done, run `stack build` in this directory.
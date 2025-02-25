
cd codegen;
rm -rf Gen;
elm-codegen install;
cd ..;


bun run typecheck

rm -rf dist
elm make main/Run.elm --output=dist/generate.js
bun build ./index.ts --outfile=dist/run.js

# To reduce confusion, remove the intermediate file.
rm ./dist/generate.js
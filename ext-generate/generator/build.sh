cd codegen;
rm -rf Gen;
elm-codegen install;
cd ..;


bun run typecheck

rm -rf dist
elm make main/Run.elm --output=dist/generate.js --optimize
bun build ./index.ts --outfile=dist/run.js

# To reduce confusion, remove the intermediate file.
rm ./dist/generate.js

# Generate random version string and update Javascript.hs
RANDOM_VERSION=$(openssl rand -hex 8)
sed -i '' "s/version = \".*\"/version = \"$RANDOM_VERSION\"/" ../Gen/Javascript.hs
sed -i '' "s/version = \".*\"/version = \"$RANDOM_VERSION\"/" ../Gen/Templates.hs

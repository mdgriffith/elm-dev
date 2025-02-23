
cd codegen;
rm -rf Gen;
elm-codegen install;
cd ..;

rm -rf dist
elm make main/Run.elm --output=dist/generate.js
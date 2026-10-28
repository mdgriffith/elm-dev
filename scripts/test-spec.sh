#!/usr/bin/env bash
set -euo pipefail

ROOT="$(git rev-parse --show-toplevel)"
stack build
BIN="$(stack path --local-install-root)/bin/elm-dev"
WORK="$(mktemp -d "${TMPDIR:-/tmp}/elm-dev-spec.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

pushd "$WORK" >/dev/null
"$BIN" init
"$BIN" add listener GeneratedBridge
cp -R "$ROOT/scripts/fixtures/spec-app/" .
cp elm.json manifest-before-setup.json
if "$BIN" test --glob=BehaviorSpec.tests >unprepared.log 2>&1; then
  printf 'Expected a missing-setup diagnostic.\n' >&2
  exit 1
fi
grep -q 'Behavioral spec setup is incomplete' unprepared.log
cmp manifest-before-setup.json elm.json
mv tests authored-tests
if "$BIN" test init >install-required.log 2>&1; then
  printf 'Dependency installation must be explicit.\n' >&2
  exit 1
fi
grep -q 'No files were changed' install-required.log
cmp manifest-before-setup.json elm.json
test ! -f elm.spec.json
test ! -d tests
"$BIN" test init --install
test -f elm-stuff/generated-tests/Spec.elm
test -f elm-stuff/generated-tests/Spec/Program.elm
test ! -f elm.spec.json
test ! -d tests
mv authored-tests tests
if "$BIN" test init --spec >removed-flag.log 2>&1; then
  printf 'The removed --spec flag must not be accepted.\n' >&2
  exit 1
fi
cp elm.json manifest-before-tests.json
"$BIN" make elm-stuff/generated/Main.elm --output=/dev/null
"$BIN" test --seed=12345
cmp manifest-before-tests.json elm.json

# Test execution repairs deleted generated support, including isolated/MCP runs
# which do not pass through the CLI's command handler.
rm elm-stuff/generated-tests/App/Spec.elm
rm elm-stuff/generated-tests/Spec.elm
rm elm-stuff/generated-tests/Spec/Program.elm
"$BIN" test --glob=BehaviorSpec.tests
test -f elm-stuff/generated-tests/App/Spec.elm
test -f elm-stuff/generated-tests/Spec.elm
test -f elm-stuff/generated-tests/Spec/Program.elm
cmp manifest-before-tests.json elm.json

# Customized test simulators must leave generated space and survive regeneration.
"$BIN" customize App.Spec.Effect
test -f tests/App/Spec/Effect.elm
test ! -f elm-stuff/generated-tests/App/Spec/Effect.elm
"$BIN" test
test ! -f elm-stuff/generated-tests/App/Spec/Effect.elm

# Setup prepares runtime support only; test layout and configuration are authored.
cp tests/Support/TestApp.elm test-config-before.elm
cp tests/BehaviorSpec.elm behavior-before.elm
"$BIN" test init
cmp test-config-before.elm tests/Support/TestApp.elm
cmp behavior-before.elm tests/BehaviorSpec.elm
test ! -f tests/HomeSpec.elm
test ! -f tests/Support/SpecConfig.elm
test ! -f tests/Example.elm

# A consumed port message must not satisfy another expectation; the first
# failure must retain its interaction number and cause a nonzero CLI exit.
cp "$ROOT/scripts/fixtures/spec-failures/FailureSpec.elm" tests/
if "$BIN" test >failure.log 2>&1; then
  printf 'Expected the duplicate-consumption spec to fail.\n' >&2
  exit 1
fi
grep -q 'Interaction 4' failure.log
grep -q 'No unconsumed matching message' failure.log
grep -q 'Random generation requires' failure.log
popd >/dev/null
printf 'Generated app behavioral specs, customization, and failure diagnostics passed.\n'

# Behavioral specs

The application generator exposes its real initialization/update/view functions
through `App.test`, without interpreting `Effect` or requiring a browser
navigation key. The test-only `App.Spec` bridge connects those functions to the
bundled `Spec` interaction API and elm-program-test. `Spec` and `Spec.Program`
are supplied by elm-dev itself; no elm-spec package or sibling checkout is needed.

This guide is also bundled in the MCP resource `file://guides/behavioral-specs`.
It can be read without selecting a project or having a repository checkout on disk.

## Setup

```sh
elm-dev test init --install
```

elm-dev embeds and generates the reusable spec modules into
`elm-stuff/generated-tests/Spec.elm` and `Spec/Program.elm`. Setup uses the
dependency manager to install compatible `avh4/elm-program-test` and
`elm-explorations/test` dependencies. Installation is explicit: `--install` authorizes those dependency
changes. Without it, missing/incompatible dependencies are reported before any
setup files are written. Existing production versions are preserved.

`test init` recognizes generated apps from `elm.dev.json`. It prepares the bundled
runtime and dependencies, but creates no starter tests or test-configuration
modules. Write your own tests as described below, then run `elm-dev test`.

CLI and MCP share the same setup implementation. Through MCP:

```json
{
  "dir": "/absolute/path/to/app",
  "additions": [
    { "kind": "spec", "installDependencies": true }
  ]
}
```

Pass this to the `add` tool. `installDependencies` defaults to false. No source
path is needed, and setup creates no local-library link or source configuration.

Applications can optionally declare additional test-helper sources in
`elm.spec.json`, independently of the bundled spec implementation:

```json
{
  "source-directories": ["test-support"]
}
```

These optional directories are resolved relative to the app root (absolute paths
are also accepted). The generated test project's manifest includes them
along with `tests/` and `elm-stuff/generated-tests/`. Production source directories
are unchanged. Normal `elm-test` does not read this configuration; use
`elm-dev test` for the generated app harness.

If an earlier development setup linked an elm-spec checkout through
`elm.spec.json`, remove that obsolete source entry. External copies of `Spec`
would conflict with the modules elm-dev now supplies.

## Manifest and readiness contract

- Running tests, checking readiness, and requesting docs do **not** edit the
  application's `elm.json`. Explicit dependency installation may update its
  test-dependencies through the dependency manager.
- `elm-stuff/elm-dev-test/elm.json` is a disposable, generated manifest. It merges
  the app's declared production/test dependencies and supplies test-only source
  directories. Do not add `generated-tests` to the app's source directories.
- Source directories are deduplicated by canonical filesystem identity, including
  relative aliases and symlinks. Configured local-source resolution is included
  in manifest freshness checks.
- Normal apps reuse pinned dependency versions without resolving replacements.
  When solving is necessary, failures are reported; no fallback versions are
  fabricated.
- Selected spec tests check bundled support and dependency version compatibility
  before launching the compiler. Harness generation alone is not readiness.

For a focused CLI run, use `elm-dev test --glob=HomeSpec.tests`. In MCP use
`check` with `tests: { kind: "only", runs: [{ glob: "HomeSpec.tests" }] }`.

MCP `docs` module/file/value queries resolve generated harness modules and local
test sources in the test compilation context. Use the owning app's absolute
`dir`, including for configured helper files outside the app root. For example:

```json
{
  "dir": "/absolute/path/to/app",
  "queries": [
    { "kind": "module", "module": "App.Spec" },
    { "kind": "value", "value": "Spec.Program.start" },
    { "kind": "module", "module": "ProgramTest" }
  ]
}
```

Compilation/setup errors are returned as such, rather than collapsed into
"module not found". Documentation requests never install dependencies.

## Ownership

| File | Owner |
| --- | --- |
| `elm-stuff/generated/App.elm` | Always generated; includes pure test entry point, callbacks, and active listeners |
| `elm-stuff/generated/Main.elm` | Generated, customizable; exposes shared `config` and `httpTarget` |
| `elm-stuff/generated/Effect.elm`, `Listen.elm`, helpers | Generated, customizable; retain inspectable effect/listener data |
| `elm-stuff/generated-tests/App/Spec.elm` | Always generated; application bridge |
| `elm-stuff/generated-tests/Spec.elm` | Always generated; reusable interaction API bundled in elm-dev |
| `elm-stuff/generated-tests/Spec/Program.elm` | Always generated; reusable virtual-clock runtime bundled in elm-dev |
| `elm-stuff/generated-tests/App/Spec/Effect.elm` | Generated, customizable; effect simulation |
| `elm-stuff/generated-tests/App/Spec/Listen.elm` | Generated, customizable; listener simulation |
| `tests/**/*.elm` | Authored and organized by the application; setup creates no test files |
| Test configuration and adapters, such as `tests/Support/PyreSpec.elm` | Authored by the application when needed |
| `elm.spec.json` | Optional application-owned additional test-helper sources; unnecessary for Spec itself |

Customize test interpreters with:

```sh
elm-dev customize App.Spec.Effect
elm-dev customize App.Spec.Listen
```

They move to `tests/App/Spec/Effect.elm` and `tests/App/Spec/Listen.elm`, respectively,
preserving module names under the existing `tests` source root. Regeneration
removes the hidden counterpart and preserves the customized file. Setup does not
create or rewrite application test/configuration modules.

## Writing a spec

Create an Elm module under `tests/` and expose a `Test` value. For example, write
`tests/HomeSpec.elm` yourself:

```elm
module HomeSpec exposing (tests)

import App.Spec
import Json.Encode
import Main
import Spec
import Test exposing (Test)
import Time


tests : Test
tests =
    Spec.onPage
        { start =
            \_ ->
                App.Spec.start
                    { app = Main.config
                    , httpTarget = Main.httpTarget
                    , url = "https://example.test/"
                    , flags = Json.Encode.object []
                    , time = Time.millisToPosix 0
                    , zone = Time.utc
                    , zoneName = Time.Name "UTC"
                    }
        , init = Spec.initStatic ()
        , tests =
            \_ ->
                [ Spec.test "opens the home page"
                    [ Spec.expect.textIsRendered "Home"
                    ]
                ]
        }
        |> Spec.run
```

Supply flags valid for your application; the empty object above suits the default
scaffold. Run the example with `elm-dev test --glob=HomeSpec.tests`.

### Organizing tests

There are no required suite names, helper names, or separate configuration files.
Keep startup options inline for a small spec. As suites grow, you can author
shared fixtures/startup helpers and data adapters under `tests/Support/`, and
group suites under `tests/Page/`, `tests/Store/`, or other application-specific
namespaces. For example:

```text
tests/
    Page/LoginSpec.elm
    Store/SessionSpec.elm
    Support/TestApp.elm
    Support/PyreSpec.elm
```

Each file's module name follows its path beneath `tests/`. Only exposed `Test`
values are run; helpers can expose ordinary values/functions. All of these files
are written and maintained by the application, not generated by elm-dev.

The core supplies extensible interaction lists, fixture-driven setup, HTTP
matching/responses, consumable outbound port messages, inbound port delivery,
and virtual time. GraphQL/Pyre adapters are external modules producing the same
`Interaction` type. Adapter examples follow below.

### Building an external adapter

Adapters return `Spec.Interaction model msg effect`, so they can be mixed with
built-in user interactions and assertions. `Spec.batch` composes interactions;
`Spec.fromProgramTest` and `Spec.custom`/`Spec.mapProgram` are lower-level extension
points. No GraphQL or Pyre dependency is required by the core.

For HTTP, use `Spec.httpPending`, `Spec.noHttpRequest`, and `Spec.httpResponse`
with a matcher:

```elm
{ method = "POST"
, url = "https://example.test/api"
, description = "save Ada"
, matches =
    \request ->
        Decode.decodeString (Decode.field "name" Decode.string) request.body
            == Ok "Ada"
}
```

Import `Json.Decode as Decode`. Matchers inspect real headers and body strings;
`Spec.httpResponse matcher response` supplies an `Http.Response String` to the
oldest matching pending request. `Test.Http.httpResponse` constructs responses.
Matching a request and delivering its response can be separate interactions to
assert loading states. A GraphQL adapter can derive the matcher and response
envelope from its client's serializer without changing the core.

For a port-based protocol, consume an outbound message and capture its identity:

```elm
Spec.outgoingPort
    { name = "bridgeOut"
    , description = "submit the expected edit"
    , decoder = outgoingDecoder
    , matches = matchesExpectedEdit
    , then_ =
        \submitted ->
            Spec.batch
                [ Spec.expect.textIsRendered "Saving"
                , Spec.incomingPort "bridgeIn" (encodeReceipt submitted)
                ]
    }
```

The decoder, matcher, and encoder are app-owned. Matching consumes the oldest
unconsumed message once per session; unrelated messages remain available. The
typed continuation can capture request IDs for subsequent response interactions.
Avoid mixing raw history-clearing elm-program-test port assertions with this
consumption API on the same port.

A Pyre adapter can inspect `pyreStoreOut` and deliver fixtures on
`pyre_receiveQueryDelta`, `pyre_receiveSyncState`, and
`pyre_receiveMutationResult` (or the app's configured names). Match database
identity alongside query/request identity, using generated public builders/codecs
where available. Mutation receipts and query publications should be independent
interactions; don't treat a receipt as another cache patch. This exercises the
Elm bridge boundary, not the TypeScript worker or database runtime.

For reusable setup, `Spec.initWith { cases, setup }` starts every test afresh for
each fixture, prepends `setup fixture` interactions, and passes the fixture to
`tests`. This supports external `initQuery` helpers and application-defined
personas. The runner has no built-in role or permission policy.

`Spec.advanceTime` explicitly runs delayed work. `Time.now`, delayed messages,
and periodic listeners share the virtual clock. Each test starts fresh. Errors
retain the first failing interaction's number/description. Failed specs and
compilation errors cause `elm-dev test` to exit unsuccessfully.

## Existing applications

Customized modules need these source changes before using the new bridge:

- Extract and expose `Main.config`, then define `main = App.app config`.
- Extract and expose the URL-base/header function as `Main.httpTarget`; use the
  same function in production `Effect.toCmd` and the test configuration.
- HTTP request bodies/headers are declarative: use `Effect.Http.jsonBody`,
  `emptyBody`, `stringBody`, etc., and `Effect.Http.header` (or string pairs),
  instead of opaque `elm/http` bodies and headers. Response error/decoder APIs
  remain based on `elm/http`.
- `Listen.OnFromJs` stores `portName`, a raw JSON `subscribe` function, and one
  `receive : Json.Decode.Value -> msg` callback. Remove the old `decoder` and
  constructed `subscription` fields. Production subscribes with
  `listener.subscribe identity |> Sub.map listener.receive`; simulation passes
  incoming JSON directly to `listener.receive`. Decode and handle errors in that
  one callback so both paths use the same application logic.
- In customized `Listen.map`, preserve `portName` and `subscribe`, and compose
  `receive = listener.receive >> f`.
- Generated decoder-based listener helpers now take `decoder` and
  `onError : { value : Json.Decode.Value, error : Json.Decode.Error } -> msg`.
  `Listen.LocalStorage.onUpdated` also requires `onError`. The callback receives
  the original payload and structured decoding error, allowing request correlation,
  recovery, or application-owned formatting with `.error >> Decode.errorToString`.
  To retain the previous ignore-on-invalid-input behavior, use
  `onError = \_ -> SubscriptionIgnored`, with an application message whose update
  leaves the model unchanged and returns `Effect.none`.
- Customized `Effect`/`Listen` modules must incorporate the corresponding new
  representations and helpers before the generated test interpreters can use
  them. App-specific variants can then be simulated in the customized test modules.

For example, an application listener that already delivers decoding results needs
only one receive pipeline:

```elm
listenSession toMsg =
    Listen.OnFromJs
        { portName = "authSessionReceived"
        , subscribe = authSessionReceived
        , receive =
            Decode.decodeValue sessionUserDecoder
                >> Result.mapError Decode.errorToString
                >> toMsg
        }
```

The polymorphic incoming port is specialized to raw JSON when stored as
`subscribe : (Decode.Value -> Decode.Value) -> Sub Decode.Value`. The callback
owns the error policy; there is no separate decoder in `OnFromJs`.

Unsupported effects are reported explicitly. Default focus/scroll simulations
complete successfully without modeling layout; measurement/file/random fixtures
and advanced HTTP behavior belong in customized simulators.

## Verification

```sh
bash scripts/build-generator-assets.sh
bash scripts/test-spec.sh
stack test elm-dev:elm-dev-tests
```

The integration script uses a temporary app, with no sibling checkout, and checks production
compilation, external exchanges, timer behavior, customization, repeatable setup,
and expected failures.

The isolated test worker uses a separate disk-based batch compiler, rather than
inheriting the editor's in-memory analysis mode. Compilation has a separate heap
budget from test execution; `ELM_DEV_TEST_COMPILER_GHCRTS` can override the default
compiler RTS settings when needed for a large app.

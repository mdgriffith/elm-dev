# Dependency Management Implementation

## Goal

Elm Dev should provide complete, transactional dependency management for both
production and test dependencies without adopting `elm-json` as a runtime
dependency or copying its solver. Elm Dev already contains Elm's registry,
package cache, constraint implementation, and dependency solver. The work is to
generalize those facilities and expose one consistent package-management layer
to the CLI and MCP server.

Interactive project creation is explicitly out of scope.

## Command Surface

```text
elm-dev install PACKAGE...
elm-dev dep tree [PACKAGE]
elm-dev uninstall PACKAGE...
elm-dev uninstall unused
elm-dev upgrade [PACKAGE...]

elm-dev test install PACKAGE...
elm-dev test dep tree [PACKAGE]
elm-dev test uninstall PACKAGE...
elm-dev test uninstall unused
elm-dev test upgrade [PACKAGE...]
```

Mutating commands support:

```text
--dry-run       Solve and report changes without writing.
--yes           Apply without asking for confirmation.
--format=json   Emit stable, machine-readable output and do not prompt.
```

Upgrade commands additionally support:

```text
--unsafe        Permit major-version upgrades.
--all-scopes    Upgrade production and test roots in one transaction.
```

`--all-scopes` means production and explicit test dependencies. Solving always
considers both scopes for compatibility, even when only one scope is allowed to
change.

Package arguments should eventually support:

```text
author/project          latest compatible release
author/project@2        latest compatible 2.x release
author/project@2.1.0    exactly 2.1.0
```

Exact requirements are supported for application projects. Package projects
reject them because Elm package dependency constraints must be non-empty ranges;
major requirements are persisted from the solved version to the next major.

All packages supplied to one command are solved and applied in one transaction.

## Scope Semantics

The project `elm.json` is always authoritative. The generated test application
at `elm-stuff/elm-dev-test/elm.json` is disposable derived state and must never
be edited as the source of truth.

Production commands mutate production direct dependencies. Test commands mutate
explicit test dependencies. They follow these rules:

- Production operations solve against explicit test roots so tests remain
  compatible.
- Test operations hold production direct versions fixed.
- `test install` does not duplicate a package already available as a production
  dependency.
- `test uninstall` never removes a production dependency.
- Successful mutations regenerate the derived test application and invalidate
  dependency-derived Watchtower state.
- `upgrade --all-scopes` is the explicit way to permit both sets of roots to
  change together.

For application projects, all four dependency maps are generated from the
solution: production direct, production indirect, test direct, and test
indirect. A package shared by production and tests belongs to the production
graph and is not duplicated in test maps.

For package projects, install, uninstall, tree, and unused detection are in
scope. Manifest-changing upgrades are initially unsupported because package
constraints describe a public compatibility contract. A future package-specific
feature should explicitly define whether it raises lower bounds, widens ranges,
or verifies compatibility without editing.

## Operation Semantics

### Install

- Accept one or more package requirements.
- Solve all requirements together.
- Promote an indirect dependency to direct without duplicating it.
- Moving an explicit test dependency to production removes it from the test
  roots.
- Preserve non-targeted direct versions when possible.
- Report additions, removals, version changes, and scope moves.

### Tree

- `dep tree` displays the production graph.
- `test dep tree` displays the combined test compilation graph.
- Use the application directory name or package name as the root.
- Use Unicode tree connectors and label only explicit test roots; production
  roots and indirect nodes are clear from their position and remain uncluttered.
- Mark repeated subtrees rather than expanding them again.
- An optional package filters output to paths that lead to that package, which
  answers why it is present.
- JSON output represents nodes and edges rather than rendered tree text.

### Uninstall

- Accept one or more explicit dependencies.
- Remove requested roots and solve once.
- Keep a removed root as indirect if another root still requires it.
- Prune transitive dependencies that are no longer reachable.
- A package absent from the requested scope is a reported no-op, not a partial
  mutation failure.
- Protect required dependencies such as `elm/core`, and preserve application
  validity requirements around `elm/json`.

### Uninstall Unused

Unused detection must be conservative. A direct package is removable only when
no relevant reachable source module imports a module owned by that package.

- Production analysis considers production and test sources. This prevents
  removal of a production dependency used only by tests.
- Test analysis considers test sources and only proposes explicit test roots.
- Package exposed modules are roots; application analysis uses configured build
  entrypoints when available.
- Generated source directories are included after generation is refreshed.
- Parse, compile, stale-generation, unknown-root, or ambiguous-ownership states
  must prevent automatic removal rather than guess.
- Implicit dependencies such as `elm/core` are protected.
- The command first lists unused direct packages and the transitive packages
  that would disappear, then asks for confirmation unless `--yes` is present.

### Upgrade

- Safe upgrades keep each targeted root within its current major version and do
  not intentionally downgrade it.
- `--unsafe` permits versions in later major lines.
- An optional package list targets only those roots.
- `upgrade` targets production roots and keeps test roots fixed.
- `test upgrade` targets test roots and keeps production roots fixed.
- `--all-scopes` allows both production and test roots to change.
- Indirect packages may change as required by the selected direct versions.
- The current Elm solver is newest-first but is not a formal global maximizer.
  User-facing wording should promise a compatible upgrade solution, not the
  mathematically maximal version vector.

## Shared Architecture

Do not add a fourth package-management implementation. Consolidate the current
logic in:

- `terminal/src/Install.hs`
- `ext-common/Ext/Install.hs`
- `ext-common/Ext/Test/Install.hs`

The shared layer should live under `ext-common`, for example
`Ext.DependencyManager`, with UI-independent concepts similar to:

```haskell
data Scope = Production | Test
data UpgradePolicy = Compatible | AllowMajor

data Operation
  = Install Scope [PackageRequirement]
  | Uninstall Scope [Pkg.Name]
  | Upgrade Scope UpgradePolicy UpgradeTargets

data Change a
  = Added a
  | Removed a
  | Changed a a
  | Moved Scope Scope a

data Plan
  = NoChanges NoChangeReason
  | PlannedChanges
      { oldOutline :: Outline.Outline
      , newOutline :: Outline.Outline
      , changes :: [DependencyChange]
      }
```

Planning, rendering, persistence, and project refresh must remain separate:

1. Parse requirements and operation options.
2. Build root constraints.
3. Solve and classify the complete graph.
4. Produce a deterministic change plan.
5. Render text or JSON.
6. Ask for confirmation when applicable.
7. Persist and verify transactionally.
8. Refresh test and Watchtower state.

## Solver Changes

`builder/src/Deps/Solver.hs` currently exposes `addToApp`, specialized to one
package. Add a generalized application solver that accepts independently
constrained production and test roots.

It must:

1. Solve the union of root constraints once.
2. Preserve the identity of production and test roots.
3. Compute the transitive closure of production roots.
4. Classify the remaining solved packages into test direct and test indirect.
5. Ensure each package appears in exactly one application dependency map.

Keep `addToApp` as a compatibility wrapper until all callers migrate.

## Persistence and Verification

Mutations must run under `Stuff.withRootLock` and be all-or-nothing:

1. Retain the original `elm.json` bytes and decoded outline.
2. Encode the proposed outline once with a trailing newline.
3. Write through an atomic temporary-file-and-rename operation.
4. Synchronize `Ext.FileCache` with the bytes written to disk.
5. Run project-details verification.
6. Restore the original bytes and cache state if verification fails.
7. Regenerate `elm-stuff/elm-dev-test/elm.json` after success.

Do not globally change `Elm.Outline.write`; it intentionally supports virtual
projects where writes may remain in memory.

## Output Contract

Human output should use one deterministic change plan with sections for added,
removed, upgraded, downgraded, and moved packages. Stable ordering is package
name order.

`--format=json` implies noninteractive behavior. It must never mix terminal
prose or ANSI control codes into stdout. Errors should also have structured JSON
representations and commands should use nonzero exit codes on failure.

The initial JSON schema should include:

```json
{
  "operation": "upgrade",
  "scope": "production",
  "status": "planned",
  "changes": [],
  "written": false
}
```

Tree JSON should use explicit package nodes and dependency edges.

## Test Strategy

Tests should prefer pure planner/classification functions and fixture registries
over the live package website. Add integration tests only where persistence,
locking, CLI dispatch, or compiler verification is the behavior under test.

### Application planning

- Multi-install solves one transaction.
- Exact and major requirements are honored for applications.
- Installing an indirect package promotes it.
- Production install promotes and removes an explicit test root.
- Test install of a production dependency is a no-op.
- Runtime and test maps never contain duplicates.
- Removing an unused direct dependency prunes its exclusive transitives.
- Removing a still-required direct dependency moves it to indirect.
- Test uninstall changes only the test roots.
- Safe upgrade stays within the major line.
- Unsafe upgrade may cross a major line.
- Targeted upgrades hold non-target roots fixed.
- All-scopes upgrade permits both root sets to change.
- No-solution and offline-no-solution remain distinguishable.

### Package planning

- Multi-install produces next-major constraints.
- Production install promotes a test constraint.
- Production and test maps never duplicate a package.
- Production and test uninstall affect only the requested scope.
- Removing `elm/core` is rejected where required.
- Upgrade reports unsupported without modifying the outline.

### Tree

- Production and test roots are labeled correctly.
- Shared subtrees are marked once.
- Filtering returns every path to the requested package.
- Missing-package filtering is a successful empty result.
- JSON nodes and edges are stable and complete.

### Unused analysis

- Imports mark their owning direct package used.
- Transitive local imports count.
- Dead local modules do not count when roots are known.
- Package exposed modules define reachability.
- Test-only use protects a production dependency.
- Test unused only proposes explicit test roots.
- Ambiguous ownership and failed analysis block mutation.
- Generated source directories participate.
- Protected compiler dependencies are never proposed.

### Transactions and interfaces

- `--dry-run` never writes.
- `--yes` does not prompt.
- JSON mode is valid JSON with no ANSI output.
- Failed verification restores the original bytes.
- Memory mode updates both disk and cache for real projects.
- Concurrent operations serialize under the root lock.
- Successful operations regenerate the test application.
- CLI rejects malformed requirements and unknown flags.
- MCP schemas and results match the shared operation model.

## Implementation Status

- [x] Product behavior and command semantics agreed.
- [x] Existing Elm JSON and Elm Dev architecture researched.
- [x] Generalized application solver.
- [x] Shared operation types and deterministic change plans.
- [x] Transactional persisted writer with verification rollback.
- [x] Multi-package install with latest, major-line, and application exact-version requirements.
- [x] Production and test dependency trees, including package filtering.
- [x] Multi-package uninstall.
- [x] Initial conservative unused-dependency analysis across production and test details.
- [x] Scoped safe and unsafe application upgrades.
- [x] Human confirmation, `--dry-run`, and `--yes`.
- [x] Initial stable `--format=json` output.
- [x] Derived test-project refresh after successful writes.
- [x] Composite MCP dependency operations with explicit project-state refresh.
- [x] Comprehensive unit and integration tests for parsing, constraint planning,
  graph classification, trees, unused candidates, diffs, persistence, and rollback.

## Relevant Existing Files

- `builder/src/Deps/Solver.hs`: registry-backed dependency solver and current
  `addToApp` classification.
- `builder/src/Elm/Outline.hs`: application/package manifest types and encoding.
- `compiler/src/Elm/Constraint.hs`: exact, next-minor, and next-major ranges.
- `terminal/src/Install.hs`: mature interactive plan display and rollback.
- `ext-common/Ext/Install.hs`: noninteractive production installer.
- `ext-common/Ext/Test/Install.hs`: noninteractive test installer.
- `ext-common/Ext/Test/Compile.hs`: derived test application generation.
- `ext-dev/CommandParser.hs`: active CLI parser.
- `ext-dev/MainDev.hs`: active CLI command registry.
- `ext-dev/Ext/Dev/Imports.hs`: existing package/import grouping.
- `ext-dev/Ext/Dev/Project.hs`: source reachability and unused-module analysis.
- `ext-watchtower/Watchtower/Server/MCP.hs`: MCP tool registry and handlers.
- `ext-watchtower/Watchtower/State/Project.hs`: filesystem-driven project refresh.
- `tests/TestMain.hs`: current Haskell test entrypoint.

## Design Risks

- The solver chooses newest versions first but does not optimize a global version
  vector. Tests must assert policy constraints and graph validity rather than an
  undocumented global optimum.
- `Elm.Outline.read` enforces required dependency invariants. Removal planning
  must reject invalid outlines before writing them.
- Current package-name parsing is duplicated and weaker than `Elm.Package`'s
  parser. New commands should share one validated requirement parser.
- The CLI parser currently permits unconsumed positional arguments in some
  command shapes. Package commands must reject leftovers.
- Current test installation can preserve production maps that disagree with its
  solver result and can fail to persist in memory mode. Migrating existing
  install callers is part of this feature, not optional cleanup.
- Filesystem notifications alone are insufficient for state refresh when the
  in-memory cache already contains the new manifest bytes.

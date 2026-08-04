# LSP Editor Restart Performance

## Summary

Restarting Zed currently causes substantially more work than one cold incremental Elm compile. The daemon discovers many nested Elm projects, creates one recursive filesystem watcher per discovered project, recompiles projects affected by reopened buffers, compiles their tests, and repeatedly rebuilds and publishes diagnostics across every project known to the daemon.

Automatic test compilation is useful and should remain. The avoidable costs are over-broad project discovery, overlapping watchers, repeated startup compile requests, and diagnostics that are not scoped to the LSP connection that requested them.

## Observed Situation

The following was observed on July 30, 2026 while restarting Zed with the `elm-ui` and `lore` worktrees open.

Before Zed connected, the daemon was idle at approximately:

- 0% CPU
- 34 threads
- 486 MB resident memory

Zed then started two expected LSP processes:

- One rooted at `/Users/griff/projects/elm-ui`
- One rooted at `/Users/griff/projects/lore`

After initialization, the daemon reached approximately:

- 22 registered Elm projects
- 76 threads
- 1.7 GB peak resident memory
- 1,559 VFS entries containing about 42 MB of source text

A process sample showed exactly 22 native `watchRunLoop` threads, matching the number of registered projects. Zed's log also showed repeated diagnostic batches at 07:06:42, 07:06:45, 07:06:47, and 07:06:48.

The registered projects included useful projects as well as derived or generated projects that should not have been discovered:

- `elm-ui/elm-stuff/elm-dev-test`
- `elm-ui/elm-stuff/generated-code/elm-community/elm-test/...`
- `lore/glyph/elm-stuff/elm-dev-test`
- `lore/glyph/elm-stuff/generated-code/elm-community/elm-test/...`
- `lore/dist/prod-artifact/pyre/packages/client`

The daemon also published `elm-ui` diagnostics to a Zed connection that did not own the corresponding worktree. Zed rejected these with `skipping diagnostics update, no worktree found` warnings.

## Current Startup Flow

### LSP initialization

Each LSP connection calls `Discover.discover` for its workspace root from `Watchtower/Server/LSP.hs`. Discovery recursively searches the entire root for `elm.json` files.

For each discovered project, initialization:

1. Reads and validates the outline.
2. Searches application source directories for Elm files exposing `main`.
3. Registers the project in shared daemon state.
4. Discovers test files.
5. Starts a recursive filesystem watcher if the project is new.

The shared daemon state is useful across LSP, MCP, and dev-server clients, but it also means projects from independent LSP roots are currently mixed together for later diagnostics work.

### Project discovery problems

`Ext/Dev/Project.hs` intends to skip `node_modules` and hidden directories. Its hidden-directory check examines whether the complete path starts with `.`, which does not work for an absolute path. Discovery also does not exclude `elm-stuff` or generated build output.

As a result, generated test projects become normal daemon projects. They receive project state, source discovery, test discovery, and their own recursive watchers.

Discovery is also repeated for each LSP initialization even when a canonical workspace root has already been scanned by the daemon.

### Reopened documents

Zed sends `textDocument/didOpen` for restored Elm buffers. `handleDidOpen` inserts the editor text into the VFS and, when that insert changes the cache, schedules a debounced compile for projects affected by that path.

The debounce state stores one pending thread per project, but not an accumulated set of changed paths. A later `didOpen` cancels the earlier request and replaces it with a request containing only the later path. A burst of reopened files can therefore cause repeated scheduling and can lose useful batching context.

### Test compilation

After a successful production compile, `compileRelevantProjectsUntraced` calls `compileTestsWithTrace` for the compiled project. This compiles all discovered tests for that project but does not execute them.

This behavior should remain. It gives immediate test compilation diagnostics and keeps test state ready for editor and MCP features. The relevant optimization is to ensure that a startup burst compiles each production project and its tests no more than once for the resulting filesystem/VFS version.

### Diagnostic publication

After owner and final compile phases, `publishDiagnosticsForWorkspaceSnapshotUntraced` reads every project in global daemon state and constructs diagnostic maps for all of them. It does not first restrict projects to the roots associated with the current LSP connection.

For every snapshot it:

1. Recomputes project and test diagnostics across all registered projects.
2. Computes warnings and open-file unused-module diagnostics.
3. Publishes every file with non-empty diagnostics, even if its diagnostics did not change.
4. Clears files that were present in the previous snapshot but not the current snapshot.
5. Requests a workspace-wide code-lens refresh.

Unused-module diagnostics can parse a project's complete source graph once per open file. Repeating that across several startup snapshots multiplies source-directory traversal and parsing work.

## Recommended Fixes

### 1. Make project discovery artifact-aware

Use path-segment-aware exclusion rules during recursive discovery.

Always exclude:

- `.git`
- `node_modules`
- `elm-stuff`

Treat conventional build-output directories such as `dist` as excluded by default only if the policy has an explicit override for intentionally checked-in or user-selected projects. A generated project should also be discoverable when directly supplied as a workspace/project root, without requiring broad recursive discovery to include all build artifacts.

The exclusion predicate should inspect each path segment or the current entry's basename, rather than searching the full path for substrings or testing whether an absolute path starts with `.`.

Keep nested project discovery for real monorepos, examples, benchmarks, and experiments. Finding an `elm.json` must not automatically stop recursion because valid Elm repositories can contain intentional child projects.

### 2. Cache workspace discovery

Track canonical workspace roots that have already completed discovery. A second LSP connection or reconnect for the same root should reuse the discovered project set rather than recursively scanning it again.

The cache needs an invalidation path for creation, deletion, or movement of `elm.json` and `elm.dev.json`. The workspace watcher can trigger a targeted rescan of the affected subtree rather than a full rescan on every initialization.

Concurrent initialization of the same canonical root should share one in-flight discovery operation.

### 3. Replace per-project recursive watchers with a watcher registry

Do not start an independent recursive FSNotify manager for every project.

Prefer one watcher per canonical LSP workspace root, with events dispatched to registered projects using `Ext.Dev.Project.affectsCompilation`. MCP or dev-server projects outside an LSP workspace can register the narrowest additional root necessary.

The registry should deduplicate covered roots. If `/repo` is already watched recursively, registering `/repo/example` must not create another recursive native watcher. It should only add project routing metadata.

Artifact exclusions still belong in both discovery and event filtering. Avoiding callbacks for `elm-stuff` is helpful, but preventing generated projects and overlapping watchers is the larger fix.

### 4. Batch initial `didOpen` compilation by project

Change the per-project debounce entry from only a `ThreadId` to pending work containing:

- The union of changed paths
- The callbacks/connections that need publication
- The scheduled worker

Every `didOpen` arriving during the debounce window should merge its path into the pending set. When the timer fires, compile each relevant owner/downstream project once using the complete set of reopened paths.

This batching should preserve owner-first diagnostics. It should also preserve the compile-version loop so edits arriving during compilation produce one subsequent clean-up compile rather than being lost.

### 5. Preserve test compilation, but deduplicate it

Continue compiling tests after a successful production compile.

Associate test compilation with the project's resulting compile/filesystem version. If several initial `didOpen` requests collapse into the same successful production compile, compile that project's tests once. If another request observes that tests are already compiled for the current project version, reuse the result.

Owner production diagnostics should remain publishable before downstream and test compilation finish. Final project/test diagnostics can follow when the complete batch is ready.

### 6. Scope diagnostics to each LSP connection

Use the `lspRoot` values stored in the connection's `LspSession` before constructing project diagnostic maps. Only projects belonging to that connection's workspace roots should participate in its snapshot.

This prevents:

- Cross-worktree diagnostics
- Work proportional to unrelated projects in the shared daemon
- Zed warnings for paths absent from the connection's worktree
- One busy workspace delaying diagnostics in another workspace

This should complement the ownership rules in `docs/lsp-diagnostic-ownership.md`: first scope by LSP session, then apply nearest-project ownership within that scope.

### 7. Publish diagnostic deltas

Store a stable fingerprint or result identifier for the diagnostics last published for each URI and connection. Send `textDocument/publishDiagnostics` only when:

- The diagnostic payload changed, or
- Previously published diagnostics need to be cleared.

Do not resend every non-empty diagnostic on every owner/final callback. Coalesce code-lens refresh requests once per completed startup batch.

### 8. Cache source graphs per project version

Build the parsed source graph once per project filesystem/VFS version and reuse it for:

- Entrypoint dependency selection
- Unused-module diagnostics
- Multiple open files in the same project

Invalidate the graph only when a relevant Elm source or project configuration changes. Diagnostic publication should not recursively list and parse the same source tree once per open file.

### 9. Add startup-specific tracing and counters

Tracing is currently opt-in through `ELM_DEV_TRACE_DB`, so a daemon started by another client may not capture the editor restart that needs investigation. Add lightweight always-available counters to the service/memory endpoint and detailed spans when tracing is enabled.

Useful fields include:

- LSP connection ID and canonical workspace roots
- Discovery duration, visited-directory count, and discovered-project count
- Excluded artifact-directory counts by reason
- Active native watcher roots
- Reopened-file count and debounce batch size
- Production projects compiled per batch
- Test projects compiled per batch
- Diagnostic projects/files considered, changed, published, and cleared
- Source-graph cache hits and rebuilds

## Suggested Implementation Order

1. Fix discovery exclusions, especially `elm-stuff` and hidden directories.
2. Scope diagnostic snapshots to the initiating LSP connection.
3. Deduplicate recursive watchers by workspace root.
4. Merge pending `didOpen` paths per project.
5. Publish diagnostic deltas and coalesce code-lens refreshes.
6. Cache source graphs and test compilation by project version.
7. Add discovery/watcher/startup counters and tracing.

The first three changes should remove the largest accidental work without changing the useful behavior of automatic test compilation.

## Acceptance Criteria

For a Zed restart with `elm-ui` and `lore` open:

- No project under `elm-stuff`, `.git`, or `node_modules` is recursively discovered.
- Build artifacts such as `lore/dist/prod-artifact/...` are excluded unless explicitly selected.
- Two LSP roots produce at most two recursive workspace watchers, plus narrowly scoped watchers for explicitly registered external projects.
- Reopened files are compiled in one merged batch per affected project/version.
- Tests are compiled automatically once after each successful resulting production compile.
- The `elm-ui` LSP connection receives only `elm-ui` diagnostics, and the `lore` connection receives only `lore` diagnostics.
- Unchanged diagnostic payloads are not republished.
- No `skipping diagnostics update, no worktree found` warnings are produced by Zed for Elm Dev diagnostics.
- CPU returns to the normal idle range shortly after startup work completes.
- Thread count is bounded by active services and workspace roots rather than discovered project count.

# LSP Diagnostic Ownership

Diagnostics should be owned by the nearest Elm project root for the file being edited, and that owner project should be compiled first and published as soon as its primary compile completes. Downstream projects that include the file through `source-directories` are still affected and should be recompiled, but they should not overwrite or duplicate diagnostics for a file owned by a nearer project. This mirrors the test-diagnostic policy: diagnostics are source-scoped, and slower secondary compiles should not replace fast owner diagnostics for the active file.

- For a changed file, determine the nearest owning Elm project root by physical project hierarchy, not only by affected source directories.
- Compile the owning project first and publish its diagnostics immediately after the primary compile loop is clean.
- If the owning project is an Elm package and the changed file is not reachable from exposed modules, still compile/check the changed file as an owner diagnostic target so editor feedback does not depend on tests.
- Loosen affected-project detection so an Elm file inside any configured `source-directories` affects that project, even when the source dir is outside the project's `elm.json` directory.
- Recompile downstream affected projects after/alongside the owner compile, but do not publish their diagnostics for files owned by another nearer project unless an explicit merging policy is added.
- Test diagnostics remain scoped to files under that project's `tests/` directory unless the ownership policy is expanded later.
- Keep tracing around publish events with diagnostic counts and source/owner metadata so we can verify fast owner diagnostics and detect duplicate downstream publishes.

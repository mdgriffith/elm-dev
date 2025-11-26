### Standard Elm Application Architecture

This project is a standard Elm application using The Elm Architecture (TEA).

- Purpose: Build a user-facing UI application compiled to JavaScript.
- Structure:
  - `elm.json`: Declares `type: "application"`, sources, dependencies, and entrypoints.
  - `src/`: Contains your Elm modules (e.g., `Main.elm`).
  - `index.html` or bundler setup (outside Elm) bootstraps your Elm program.
- The Elm Architecture (TEA):
  - `Model`: The immutable state of your app.
  - `Msg`: Messages describing events that change the model.
  - `update`: Pure function handling messages and producing a new model and effects (`Cmd`).
  - `view`: Pure function rendering HTML from the model.
  - Optional `subscriptions` for external events.
- Guidance:
  - Keep `update` pure and deterministic; push side effects to `Cmd` and `subscriptions`.
  - Organize code by feature or domain, and avoid large “god modules”.
  - Prefer custom types over bools and strings to model state precisely.

Key reading:
- Well‑formed Elm code guidance: `file://guides/well-formed-elm-code`



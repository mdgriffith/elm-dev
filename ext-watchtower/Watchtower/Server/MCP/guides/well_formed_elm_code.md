## Well‑Formed Elm Code

### General notes

- **Prefer unaliased, non‑exposing imports**
  - Keep call sites explicit and searchable. Avoid `as` and `exposing` unless there’s a strong reason.
  - Rare exceptions: infix ops from `Parser`.

### Msg naming conventions

`Msg`s should be named with a `{Domain Noun}{Domain Action}{Past tense Event}` format.
Examples:
- `PostArchiveClicked` - `Post` is a domain concept noun.  `Archive` is the action we're attempting to perform.  `Clicked` is the event, What happened.
- `PostArchiveConfirmed` - We can visually scan that this is part of the same flow as the above Msg.
- `PostsReceived` - Same `Post` domain noun.  No action, just `Received`, which is what happened.

Tha main advantage for this format is that Msgs for related flows can be grouped visually in the Msg definition and in the `update` function, allowing for easy scanning and understanding.  It maps nicely to the information architecture we need to develop.

Of note, some naming methodologies use `User` or `Server` in the naming convention, though in practice this information isn't very useful.

The `Event` portion of the `Msg` can also be standardized on a per-project level. Some common `Event` names
- `Clicked`
- `Submitted`
- `Received` (sometimes `Fetched` or `Got`).
- `Confirmed`


### Use Modularity

- **Design clear module interfaces**
  - Expose only what callers need; keep implementation details private.
  - Every module should have a brief module doc with at least 1–3 sentences and (ideally) example usage.
  - All exposed values/types should have succinct doc comments.


- **Anchor modules around a core type**
  - Most modules center on a primary type and the functions that operate on it (`User`, `Session`, `Decoder User`, etc.).

- **Make undesirable states unrepresentable**
  - Model state with types so invalid combinations can’t be constructed.
  - Example:
    ```elm
    type Connection
        = Disconnected
        | Connecting
        | Connected { token : Token }
    ```

- **Parse, don’t validate**
  - Parse untrusted/loose inputs at the boundary into well‑typed values; downstream code can lead on the well-defined types and stays simple.
  - Example:
    ```elm
    type NonEmptyString = NonEmptyString String

    fromString : String -> Result String NonEmptyString
    fromString s =
        if String.trim s == "" then
            Err "Must not be empty"
        else
            Ok (NonEmptyString s)
    ```
  - Further reading: Alexis King, “Parse, don’t validate”; Elm Patterns: Parse, don’t validate.

### UI component architectures

When designing a reusable component, choose the architecture with the least power that will serve your needs.  **Configurable components** are the most common.  **Stateful components** are the most rare.


1) **Just view functions** - Least power.
   - Pure renderers. Everything is passed in via a record and transformed directly into HTML.
     Example:
     ```elm
     Button.view { label = "Save", onClick = SaveClicked }
     ```

2) **Configurable components**
   - Still stateless, but accept configuration and handlers from the parent.
   - For larger configs, use sensible defaults and the builder pattern:
     - Start with the component’s base function named after the component, e.g. `Button.button`, `Dropdown.dropdown`.
     - Have the starting function take all required values in a single record (prevents half‑configured components). For example: `Button.button : { label : String, onClick : msg } -> Button`.
     - Add options with `with*` helpers (e.g. `withLabel`, `withDisabled`, `withSelected`).
     - Register event handlers with `on{Event} : {EventPayload} -> msg` (e.g. `onClick : msg`, `onInput : String -> msg`); include mandatory handlers in the required record.
     - Finish by producing `Html msg` (either via a final `view` step or by the last builder returning `Html msg`).

     Example:
     ```elm
     Button.button { label = "Save", onClick = SaveClicked }
         |> Button.withDisabled isSaving
         |> Button.view
     ```

3) **Stateful components** - Most powerful, most complex
   - Full TEA bundle when local state/side‑effects are warranted.
   - Shape:
     ```elm
     type alias Model = ...
     type Msg = ...
     init : ( Model, Cmd Msg )
     update : Msg -> Model -> ( Model, Cmd Msg )
     view : Model -> Html Msg
     ```
   - Prefer lifting state up unless encapsulation clearly improves clarity/reuse.

References

- Elm Land — Components: state and composition (read for deeper context)
- Richard Feldman — Making Impossible States Impossible (talk)
- Alexis King — Parse, don’t validate (essay)
- Elm Patterns — Parse, don’t validate (Elm‑focused overview)
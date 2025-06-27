# Overview

Ok, I keep having to remind myself how this works, so let's write it down.

Here's the procedure.
1. `Generate.elm` - We start with a `docs.json`
2. We call `Exemplar.interactiveAll` on each `Elm.Docs.Module` in the docs json.
    2.a - This produces an `Interactive.Module`
    2.b - We then call `Interacive.generate` using these modules.



`Exemplar.interactiveAll`:
    - Fold through all the mod.values.
        - If it's a starting point, use `Example.Build.getValueNamed`
        - Then use that to call `Example.Interactive.build`


`Example.Interactive.build`:
    - `buildExampleCallStack` -> `Example.CallStack.find`
        -> `Example.Interactive.Build.build` -> Renders the callstack in an executable form
        -> `Example.Interactive.Rendered.build` -> Renders the callstack as a code string



## So, high level.

1. We build a CallStack
2. And use that to both render a string code example and generate Elm code.





## Interactive.elm

```


type alias Module =
    { name : String
    , examples : List Interactive
    }


{-| All the information needed for an interactive example.
-}
type alias Interactive =
    { name : String
    , fields : List Field
    , view :
        ViewReferences
        -> Elm.Expression
    }


type alias ViewReferences =
    { model : Elm.Expression
    , codeOrOutput : Elm.Expression
    , onChange : Elm.Expression
    }


type Field
    = Field
        String
        { init : Elm.Expression
        , input : Input
        }

type Input
    = InputString
    | InputBool
    | InputInt
    | InputFloat
    | InputMaybe Input

```
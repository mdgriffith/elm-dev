module App.Page.Error exposing (Error(..))

{-| You may want to protect a page with a certain error when it is first requested.

  - `NotFound` is built in to `elm-prefab`, so you don't need to capture that here.

Common errors are

    - Unauthenticated — When you require someone to be signed in in order to see a page.
    - Permission denied — When you require taht someone is both signed in and has certain permissions.

@docs Error

-}


type Error
    = Unauthenticated

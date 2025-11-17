module Docs.Ref exposing
    ( Id(..)
    , Location(..)
    , Ref
    )

import Elm.Docs
import Elm.Module
import Elm.Package


type alias Ref =
    { id : Id
    , source : Source
    , block : Elm.Docs.Block
    }


type Id
    = Id String


type alias Source =
    { moduleName : Elm.Module.Name
    , location : Location
    }


type Location
    = Package Elm.Package.Name
    | LocalFile String

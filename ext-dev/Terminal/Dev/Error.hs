module Terminal.Dev.Error (Error(..), toString, toJson) where

import qualified Ext.CompileProxy
import qualified Json.Encode
import Json.Encode ((==>))

data Error 
      = CouldNotFindRoot  
      | CouldNotFindModule 
      | ValueProvidedIsntQualified 
      | ValueProvidedIsEmpty

      | CompilationError Ext.CompileProxy.CompilationError


toString :: Error -> String
toString err =
  case err of
    CouldNotFindRoot ->
      "Could not find elm.json file in this directory or any parent directories."

    CouldNotFindModule ->
      "Could not find module."

    ValueProvidedIsntQualified ->
      "Value provided isn't qualified."

    ValueProvidedIsEmpty ->
      "Value provided is empty."

    CompilationError err ->
      "Compilation error"

toJson :: Error -> Json.Encode.Value
toJson err =
  case err of
    CouldNotFindRoot ->
      Json.Encode.object
        [ "error" ==> Json.Encode.chars "Could not find elm.json file in this directory or any parent directories."
        ]

    CouldNotFindModule ->
      Json.Encode.object
        [ "error" ==> Json.Encode.chars "Could not find module."
        ]

    ValueProvidedIsntQualified ->
      Json.Encode.object
        [ "error" ==> Json.Encode.chars "Value provided isn't qualified."
        ]

    ValueProvidedIsEmpty ->
      Json.Encode.object
        [ "error" ==> Json.Encode.chars "Value provided is empty."
        ]

    CompilationError err ->
      Ext.CompileProxy.compilationErrorToJson err
      
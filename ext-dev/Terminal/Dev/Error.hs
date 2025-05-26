module Terminal.Dev.Error (Error(..), toString, toJson) where

import qualified Ext.CompileProxy
import qualified Json.Encode
import Json.Encode ((==>))
import qualified Reporting.Exit as Exit
import qualified Elm.Package as Pkg


data Error 
      = CouldNotFindRoot  
      | CouldNotFindModule
      | NoEntrypoints
      | CouldNotFindCurrentVersionForPackage Pkg.Name
      | ValueProvidedIsntQualified 
      | ValueProvidedIsEmpty


      | ExitMake Exit.Make
      | ExitReactor Exit.Reactor
      | DocsProblem Exit.DocsProblem
      | CompilationError Ext.CompileProxy.CompilationError


toString :: Error -> String
toString err =
  case err of
    CouldNotFindRoot ->
      "Could not find elm.json file in this directory or any parent directories."

    CouldNotFindModule ->
      "Could not find module."

    NoEntrypoints ->
      "No entrypoints provided."

    CouldNotFindCurrentVersionForPackage packageName ->
      "Could not find current version for package " ++ Pkg.toChars packageName

    ValueProvidedIsntQualified ->
      "Value provided isn't qualified."

    ValueProvidedIsEmpty ->
      "Value provided is empty."


    ExitMake exit ->
      Exit.toString (Exit.makeToReport exit)

    ExitReactor exit ->
      Exit.toString (Exit.reactorToReport exit)

    DocsProblem problem ->
      Exit.toString (Exit.toDocsProblemReport problem "")

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
    
    CouldNotFindCurrentVersionForPackage packageName ->
      Json.Encode.object
        [ "error" ==> Json.Encode.chars ("Could not find current version for package " ++ Pkg.toChars packageName)
        ]

    NoEntrypoints ->
      Json.Encode.object
        [ "error" ==> Json.Encode.chars "No entrypoints provided."
        ]

    ValueProvidedIsntQualified ->
      Json.Encode.object
        [ "error" ==> Json.Encode.chars "Value provided isn't qualified."
        ]

    ValueProvidedIsEmpty ->
      Json.Encode.object
        [ "error" ==> Json.Encode.chars "Value provided is empty."
        ]

    ExitMake exit ->
      Exit.toJson (Exit.makeToReport exit)

    ExitReactor exit ->
      Exit.toJson (Exit.reactorToReport exit)
    
    DocsProblem problem ->
      Exit.toJson (Exit.toDocsProblemReport problem "")

    CompilationError err ->
      Ext.CompileProxy.compilationErrorToJson err
      
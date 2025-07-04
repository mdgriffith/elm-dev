module Generate exposing (..)

-- import Interactive

import Elm
import Elm.Docs
import Exemplar
import Extra.String
import Gen.CodeGen.Generate as Generate
import Interactive
import Json.Decode
import Options


main : Program Json.Decode.Value () ()
main =
    Generate.fromJson Options.decoder
        (\options ->
            List.concatMap renderExampleModule options.project
        )


renderExampleModule : Elm.Docs.Module -> List Elm.File
renderExampleModule mod =
    let
        interactiveMod =
            Exemplar.interactiveAll mod

        moduleName =
            "Dev" :: String.split "." interactiveMod.name
    in
    List.map
        (\exampleResult ->
            case exampleResult of
                Ok example ->
                    Interactive.generateSingle
                        (moduleName ++ [ Extra.String.capitalize example.name ])
                        example

                Err err ->
                    Elm.file (moduleName ++ [ Extra.String.capitalize err.name ])
                        [ Elm.declaration "error" (Elm.string err.description) ]
        )
        interactiveMod.examples

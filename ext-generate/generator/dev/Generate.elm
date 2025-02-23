module Generate exposing (..)

-- import Interactive

import Elm
import Exemplar
import Gen.CodeGen.Generate as Generate
import Generate.PageId
import Interactive
import Json.Decode
import Options


main : Program Json.Decode.Value () ()
main =
    Generate.fromJson Options.decoder
        (\options ->
            let
                _ =
                    Debug.log "options" options
            in
            Generate.PageId.generate options
                :: List.map renderExampleModule options.project
        )


renderExampleModule : Elm.Docs.Module -> Elm.File
renderExampleModule mod =
    case Exemplar.interactiveAll mod of
        Err err ->
            Elm.file [ "Live" ]
                [ Elm.declaration "error"
                    (Elm.string err)
                ]

        Ok interactives ->
            Interactive.generate ("Dev" :: String.split "." interactives.name)
                interactives

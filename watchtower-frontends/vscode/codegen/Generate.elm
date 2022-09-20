module Generate exposing (main)

{-| -}

import Elm
import Elm.Annotation as Type
import Elm.Docs
import Exemplar
import Gen.CodeGen.Generate as Generate
import Interactive
import Json.Decode as Decode
import Viewer


main : Program Decode.Value () ()
main =
    Generate.fromJson (Decode.list Elm.Docs.decoder) <|
        List.filterMap renderExampleModule


renderExampleModule mod =
    case Exemplar.interactiveAll mod of
        Err err ->
            Elm.file [ "Live" ]
                [ Elm.declaration "error"
                    (Elm.string err)
                ]
                |> Just

        Ok interactives ->
            Interactive.generate [ "Live" ]
                [ interactives ]

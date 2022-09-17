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
    -- if mod.name == "Ui.Button" then
    --     case Exemplar.interactive "primary" mod of
    --         Err err ->
    --             Just <|
    --                 Elm.file [ "Main" ]
    --                     [ Elm.declaration "error"
    --                         (Elm.string err)
    --                     ]
    --         Ok interactive ->
    --             let
    --                 exampleFile =
    --                     case Exemplar.example "primary" mod of
    --                         Err err ->
    --                             Elm.file
    --                                 [ "Example" ]
    --                                 [ Elm.declaration "error"
    --                                     (Elm.string err)
    --                                 ]
    --                         Ok expression ->
    --                             Elm.file
    --                                 [ "Example" ]
    --                                 [ Elm.declaration "example"
    --                                     expression
    --                                 ]
    --             in
    --             Interactive.generate ("Interactive" :: (mod.name |> String.split "."))
    --                 [ interactive
    --                 ]
    -- else
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

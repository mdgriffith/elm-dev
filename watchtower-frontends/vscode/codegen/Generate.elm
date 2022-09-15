module Generate exposing (main)

{-| -}

import Elm
import Elm.Annotation as Type
import Gen.CodeGen.Generate as Generate


main : Program {} () ()
main =
    Generate.run
        [ file
        ]


file : Elm.File
file =
    Elm.file [ "Live" ]
        [ Elm.declaration "main"
            (Elm.apply
                (Elm.value
                    { importFrom = [ "Html" ]
                    , name = "text"
                    , annotation = Nothing
                    }
                )
                [ Elm.string "World!"
                ]
            )
        ]

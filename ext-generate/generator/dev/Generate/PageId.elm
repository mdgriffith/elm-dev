module Generate.PageId exposing (generate)

{-| -}

import Elm
import Elm.Docs
import Options


generate : Options.Options -> Elm.File
generate options =
    Elm.file [ "Dev", "Page", "Id" ]
        [ Elm.customType "Id"
            (List.map
                (\mod ->
                    Elm.variant
                        (String.join "_"
                            (String.split "." mod.name)
                        )
                )
                options.project
            )
            |> Elm.expose
        ]

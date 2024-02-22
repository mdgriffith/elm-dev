module Flags exposing
    ( Flags
    , Platform(..)
    , decoder
    )

import Json.Decode as Decode


type alias Flags =
    { platform : Platform
    }


type Platform
    = Windows
    | Mac
    | Linux
    | VSCode


decoder : Decode.Decoder Flags
decoder =
    Decode.map Flags
        (Decode.field "platform" platformDecoder)


{-|

    List is taken from here: https://tauri.app/v1/api/js/os

        'linux', 'darwin', 'ios', 'freebsd', 'dragonfly', 'netbsd', 'openbsd', 'solaris', 'android', 'win32'

    But we also add in VSCode.

-}
platformDecoder : Decode.Decoder Platform
platformDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "win32" ->
                        Decode.succeed Windows

                    "darwin" ->
                        Decode.succeed Mac

                    "vscode" ->
                        Decode.succeed VSCode

                    _ ->
                        Decode.succeed Linux
            )

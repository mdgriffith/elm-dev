module Effect.File exposing (selectFile, selectMultipleFiles, toUrl, toString, toBytes)

{-|

@docs selectFile, selectMultipleFiles, toUrl, toString, toBytes

-}

import Bytes
import Effect
import File


{-| -}
selectFile : List String -> (File.File -> msg) -> Effect.Effect msg
selectFile =
    Effect.File


selectMultipleFiles : List String -> (File.File -> List File.File -> msg) -> Effect.Effect msg
selectMultipleFiles =
    Effect.Files


toUrl : File.File -> (String -> msg) -> Effect.Effect msg
toUrl fileData toMsg =
    Effect.FileToUrl fileData toMsg


{-| Read a file as text. Mirrors `File.toString`.
-}
toString : File.File -> (String -> msg) -> Effect.Effect msg
toString =
    Effect.FileToString


{-| Read a file as bytes. Mirrors `File.toBytes`.
-}
toBytes : File.File -> (Bytes.Bytes -> msg) -> Effect.Effect msg
toBytes =
    Effect.FileToBytes

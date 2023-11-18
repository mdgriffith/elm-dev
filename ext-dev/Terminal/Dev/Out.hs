module Terminal.Dev.Out (json, asJson) where

import qualified System.IO
import qualified Json.Encode
import qualified Terminal.Dev.Error
import Json.Encode ((==>))

json :: Maybe String -> Either Terminal.Dev.Error.Error Json.Encode.Value -> IO ()
json maybePath result =
    case result of
        Left err ->
            let 
                json = (Json.Encode.object ["error" ==> Json.Encode.chars (Terminal.Dev.Error.toString err) ])
            in
            case maybePath of
                Nothing ->
                    System.IO.hPutStrLn System.IO.stdout (show (Json.Encode.encode json))

                Just path ->
                    Json.Encode.write path json

        Right jsonValue ->
            case maybePath of
                Nothing ->
                    System.IO.hPutStrLn System.IO.stdout (show (Json.Encode.encode jsonValue))

                Just path ->
                    Json.Encode.write path jsonValue



asJson :: Maybe String -> (fail -> Json.Encode.Value) -> (success -> Json.Encode.Value) -> Either fail success -> IO ()
asJson maybePath encodeFail encoder result =
    case result of
        Left err ->
            let 
                json = encodeFail err
            in
            case maybePath of
                Nothing ->
                    System.IO.hPutStrLn System.IO.stdout (show (Json.Encode.encode json))

                Just path ->
                    Json.Encode.write path json

        Right success ->
            let jsonValue = encoder success
            in
            case maybePath of
                Nothing ->
                    System.IO.hPutStrLn System.IO.stdout (show (Json.Encode.encode jsonValue))

                Just path ->
                    Json.Encode.write path jsonValue
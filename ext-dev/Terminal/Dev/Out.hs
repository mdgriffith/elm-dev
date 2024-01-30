module Terminal.Dev.Out (json, asJson, asJsonUgly) where

import qualified System.IO
import qualified Json.Encode
import qualified Terminal.Dev.Error
import qualified Data.ByteString.Builder
import qualified Data.ByteString.Lazy
import Json.Encode ((==>))


printJson :: Json.Encode.Value -> IO ()
printJson jsonValue =
    Data.ByteString.Builder.hPutBuilder System.IO.stdout (Json.Encode.encode jsonValue)

asJsonUgly :: Either Terminal.Dev.Error.Error Json.Encode.Value ->  Data.ByteString.Builder.Builder
asJsonUgly result =
    case result of
        Left err ->
            Json.Encode.encodeUgly (Terminal.Dev.Error.toJson err)

        Right jsonValue ->
            Json.Encode.encodeUgly jsonValue

json :: Maybe String -> Either Terminal.Dev.Error.Error Json.Encode.Value -> IO ()
json maybePath result =
    case result of
        Left err ->
            let 
                json = Terminal.Dev.Error.toJson err
            in
            case maybePath of
                Nothing ->
                    printJson json

                Just path ->
                    Json.Encode.write path json

        Right jsonValue ->
            case maybePath of
                Nothing ->
                    printJson jsonValue

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
                    printJson json

                Just path ->
                    Json.Encode.write path json

        Right success ->
            let jsonValue = encoder success
            in
            case maybePath of
                Nothing ->
                    printJson jsonValue

                Just path ->
                    Json.Encode.write path jsonValue
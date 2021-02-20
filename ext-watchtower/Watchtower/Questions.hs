
{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Questions where


import Snap.Core hiding (path)

import Control.Monad.Trans (MonadIO(liftIO))
import Control.Applicative ((<$>), (<*>))
import qualified Data.Maybe as Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Builder

import qualified Json.Encode
import qualified Watchtower.Details
import qualified Watchtower.Annotate
import qualified Develop.Generate.Help
import qualified Reporting.Annotation
import qualified Data.Name as Name

import Data.Function ((&))

-- One off questions and answers you might have/want.
data Question
    = CallgraphPlease  FilePath Name.Name
    | ListMissingSignaturesPlease FilePath
    | SignaturePlease FilePath Name.Name
    | FindDefinitionPlease Watchtower.Details.Location
    | FindAllInstancesPlease Watchtower.Details.Location
    | ListAllRoots

-- data Answer
--     = CallgraphReturned Watchtower.Details.Callgraph
--     | ListMissingSignaturesReturned [ Watchtower.Details.Location ]
--     | SignatureReturned Watchtower.Details.Location String
--     | FindDefinitionReturned Watchtower.Details.Location
--     | FindAllInstancesReturned
--     | NOTDONE




serve :: Snap ()
serve =
  do  
    route 
        [ ("/:action", actionHandler)
        ]
    -- modifyResponse $ setResponseStatus 404 "Not Found"
    --   modifyResponse $ setContentType "text/html;charset=utf-8"
    --   writeBuilder $ Develop.Generate.Help.makePageHtml "NotFound" Nothing



questionHandler :: FilePath -> Question -> Snap ()
questionHandler root question =
    do
        answer <- liftIO (ask root question)
        writeBuilder answer


actionHandler :: Snap ()
actionHandler =
    do
        maybeAction <- getParam "action"
        case maybeAction of
            Just "list-missing-signatures" ->
                do
                    maybeFile   <- getQueryParam "file"
                    case maybeFile of
                        Nothing ->
                            writeBS "Needs location"
                        
                        Just file -> do
                            questionHandler "." (ListMissingSignaturesPlease (Data.ByteString.Char8.unpack file))
                           

            Just "signature" ->
                do
                    maybeFile   <- getQueryParam "file"
                    maybeName   <- getQueryParam "name"
                    case (maybeFile, maybeName) of
                        (Just file, Just name) ->
                            questionHandler "." 
                                (SignaturePlease 
                                    (Data.ByteString.Char8.unpack file)
                                    (Name.fromChars (Data.ByteString.Char8.unpack name))
                                )

                        _ ->
                            writeBS "Needs location"
                               
            Just "callgraph" ->
                do
                    maybeFile   <- getQueryParam "file"
                    maybeName   <- getQueryParam "name"
                    case (maybeFile, maybeName) of
                        (Just file, Just name) ->
                            questionHandler "." 
                                (CallgraphPlease 
                                    (Data.ByteString.Char8.unpack file)
                                    (Name.fromChars (Data.ByteString.Char8.unpack name))
                                )

                        _ ->
                            writeBS "Needs location"

            -- Just "definition" ->
            --     do
            --         maybeLocation   <- getLocation
            --         case maybeLocation of
            --             Nothing ->
            --                 writeBS "Needs location"
                        
            --             Just location ->
            --                 questionHandler "." (FindDefinitionPlease location)

            -- Just "instances" ->
            --     do
            --         maybeLocation   <- getLocation
            --         case maybeLocation of
            --             Nothing ->
            --                 writeBS "Needs location"
                        
            --             Just location ->
            --                 questionHandler "." (FindAllInstancesPlease location)

            _ ->
                writeBS "Wha?"

getLocation :: Snap (Maybe Watchtower.Details.Location)
getLocation =
    do
       maybeFilePath <- getQueryParam "file"
       maybePosition <- getPosition

       let maybeLocation =
            case (maybeFilePath, maybePosition) of
                (Just path, Just position) ->
                    Just 
                        (Watchtower.Details.Location
                            (Data.ByteString.Char8.unpack path) 
                            position
                        )
                    
                _ ->
                    Nothing
       
       pure maybeLocation


getPosition :: Snap (Maybe Reporting.Annotation.Position)
getPosition =
    do
        maybeRow <- getQueryParam "row"
        maybeCol <- getQueryParam "col"
        let position = 
                (\row col ->
                    Reporting.Annotation.Position 0 0
                )
                <$> maybeRow <*> maybeCol

        pure position


ask :: FilePath -> Question -> IO Data.ByteString.Builder.Builder
ask root question =
    case question of
        CallgraphPlease file name ->
            Watchtower.Annotate.callgraph root file name
                & fmap Json.Encode.encodeUgly

        ListMissingSignaturesPlease path ->
            Watchtower.Annotate.listMissingAnnotations root path
                & fmap Json.Encode.encodeUgly

        SignaturePlease file name ->
            Watchtower.Annotate.annotation root file name
                & fmap Json.Encode.encodeUgly

        FindDefinitionPlease location ->
            pure (Data.ByteString.Builder.byteString ("NOTDONE"))

        FindAllInstancesPlease location ->
            pure (Data.ByteString.Builder.byteString ("NOTDONE"))

        ListAllRoots ->
            -- Recursively list all instances of elm.json within the current directory.
            -- useufl for multi-elm app projects.
            pure (Data.ByteString.Builder.byteString ("NOTDONE"))





-- answerToJsonByteString :: Answer -> Data.ByteString.Builder.Builder
-- answerToJsonByteString answer =
--     Json.Encode.encodeUgly (answerToJson answer)
            


-- answerToJson :: Answer -> Json.Encode.Value
-- answerToJson answer =
--     case answer of
--         _ ->
--             Json.Encode.null
            
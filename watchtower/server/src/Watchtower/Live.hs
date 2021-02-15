{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Live where

import Control.Applicative ((<|>))
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import Snap.Core hiding (path)
import Snap.Http.Server
import Snap.Util.FileServe


import qualified Develop.Generate.Help
import qualified Json.Encode
import qualified Reporting.Annotation as Ann
import qualified Watchtower.StaticAssets




data State = State
        -- (TVar [Client], TVar (Maybe ClientId), BroadcastChan In Text, TVar Text)


init :: IO State
init =
    pure State


data Msg
    = EditorsVisible [ Json Location ]
    | EditorActive Location (Maybe Position)
    | ElmErrors [ Error ]
    | JumpTo Location
    -- 
    | CallgraphPlease
    | CallgraphReturned Callgraph
    -- 
    | ListMissingSignaturesPlease
    | ListMissingSignaturesReturned [ Location ]
    --
    | SignaturePlease Location
    | SignatureReturned Location String
    -- 
    | FindDefinitionPlease Location
    | FindDefinitionReturned Location
    --
    | FindAllInstancesPlease Location
    | FindAllInstancesReturned 



newtype Json guard =
    Json Json.Encode.Value


data Location =
    Location 
        { _path :: FilePath
        , _range :: Ann.Region
        }

data Position = Position
data Range = Range
-- data FilePath = FilePath
data Callgraph = CallGraph
data Error = Error
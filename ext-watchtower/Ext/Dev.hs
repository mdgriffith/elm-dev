{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev 
    ( docs
    )
    where


import qualified Elm.Docs

import qualified Ext.CompileProxy
import qualified Ext.Dev.Docs



-- warnings :: FilePath -> FilePath -> IO (Either () (Src.Module, [ Warning.Warning ]))
-- warnings root path =
--     pure (Left ())



docs :: FilePath -> FilePath -> IO (Maybe Elm.Docs.Module)
docs root path = do
    (Ext.CompileProxy.Single source warnings canonical compiled) <- Ext.CompileProxy.loadSingle root path
    case compiled of
        Just (Right artifacts) ->
            case  Ext.Dev.Docs.fromArtifacts artifacts of
                Left err ->
                    pure Nothing
                
                Right docs ->
                    pure (Just docs)

        _ ->
            pure Nothing


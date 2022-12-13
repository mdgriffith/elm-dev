{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Canonicalize
  ( type_
  )
where

import AST.Canonical (Type (..))
import qualified AST.Source as Src
import qualified AST.Canonical as Can
import qualified Reporting.Result
import qualified Canonicalize.Environment
import qualified Canonicalize.Type
import qualified Ext.CompileProxy


{-| For canonicalizing pieces of a Src module instead of the whole thing.

-}
type_ :: String -> String -> Src.Module -> Src.Type -> IO (Maybe Can.Type)
type_ root path srcMod tipe = do
    canonicalizationEnvResult <- Ext.CompileProxy.loadCanonicalizeEnv root path srcMod
    case canonicalizationEnvResult of
        Nothing ->
            pure Nothing

        Just env -> do
            let (warnings, eitherCanType) = Reporting.Result.run $ Canonicalize.Type.canonicalize env tipe

            case eitherCanType of
                Left err ->
                    pure Nothing

                Right canType ->
                    pure (Just canType)



             
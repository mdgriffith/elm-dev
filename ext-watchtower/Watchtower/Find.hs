{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Find
  ( printFindDefinition,
  )
where

import AST.Canonical (Type (..))
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified Compile
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Utf8
import Data.Word (Word16)
import qualified Elm.ModuleName
import qualified Elm.String
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String
import qualified Llamadera
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit
import StandaloneInstances
import qualified Stuff as PerUserCache
import qualified System.Directory as Dir
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)
import Terminal (Parser (..))
import qualified Text.Show.Unicode
import qualified Watchtower.Details

{- Find Definition -}

-- |
--  Given file name and coords (line and char)
--  1. Find which name is at the coordinates
--
--  2. Resolve definition source
--
--    2.a Qualified call
--        -> resolve Qualification to full module name
--        -> resolve full module name to file path
--        -> read file, get coordinates of definition
--
--    2.b Unqualified call
--        -> Check variables in local scope
--        -> Else, check top level values in file
--        -> Else, check interface for modules with exposed values
printFindDefinition :: FilePath -> FilePath -> Integer -> Integer -> IO ()
printFindDefinition root path row col = do
  mSource <- Llamadera.loadFileSource root path
  case mSource of
    Left err ->
      putStrLn $ show err
    Right (source, modul) -> do
      -- Llamadera.formatHaskellValue "Source" source
      let point = A.Position (fromIntegral row) (fromIntegral col)
          maybeValue =
            modul
              & Src._values
              & findFirst
                ( \(A.At region (Src.Value (A.At _ name_) params expr typeM)) ->
                    withinRegion point region
                )
              >>= (getExpressionNameAt point)

      Llamadera.formatHaskellValue "Source" maybeValue
      do
        -- interfaces <- Llamadera.allInterfaces [path]
        eitherArtifacts <- Llamadera.loadSingleArtifacts root path
        case eitherArtifacts of
          Right (Compile.Artifacts mod annotations (Opt.LocalGraph main graph fields)) -> do
            -- Llamadera.formatHaskellValue ("Interfaces") interfaces
            Llamadera.formatHaskellValue ("Interfaces") annotations
          Left err -> putStrLn $ show err

        pure ()

      pure ()

getExpressionNameAt ::
  A.Position ->
  A.Located Src.Value ->
  Maybe (Src.VarType, Maybe Name.Name, Name.Name)
getExpressionNameAt point (v@(A.At region (Src.Value (A.At _ name_) params expr typeM))) =
  getExpressionNameAtHelper point expr

getExpressionNameAtHelper :: A.Position -> Src.Expr -> Maybe (Src.VarType, Maybe Name.Name, Name.Name)
getExpressionNameAtHelper point expr =
  if not (withinRegion point (A.toRegion expr))
    then Nothing
    else case A.toValue expr of
      Src.Chr str ->
        Nothing
      Src.Str str ->
        Nothing
      Src.Int i ->
        Nothing
      Src.Float f ->
        Nothing
      Src.Var vType name ->
        Just (vType, Nothing, name)
      Src.VarQual vType qual name ->
        Just (vType, Just qual, name)
      Src.List exps ->
        Nothing
      Src.Op name ->
        Nothing
      Src.Negate exp ->
        Nothing
      Src.Binops ops expr ->
        Nothing
      Src.Lambda patterns expr ->
        Nothing
      Src.Call call with ->
        Nothing
      Src.If conditions elseExpr ->
        Nothing
      Src.Let defs expr ->
        Nothing
      Src.Case caseExpr patterns ->
        Nothing
      Src.Accessor name ->
        Nothing
      Src.Access expr locatedName ->
        Nothing
      Src.Update name fields ->
        Nothing
      Src.Record fields ->
        Nothing
      Src.Unit ->
        Nothing
      Src.Tuple one two listThree ->
        Nothing
      Src.Shader src types ->
        Nothing

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst fn vals =
  case vals of
    [] ->
      Nothing
    top : remain ->
      if fn top
        then Just top
        else findFirst fn remain

findFirstJust :: (t -> Maybe a) -> [t] -> Maybe a
findFirstJust fn vals =
  case vals of
    [] ->
      Nothing
    top : remain ->
      case fn top of
        Nothing ->
          findFirstJust fn remain
        otherwise ->
          otherwise

withinRegion :: A.Position -> A.Region -> Bool
withinRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  row >= startRow && row <= endRow

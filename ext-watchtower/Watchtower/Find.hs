{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Find
  ( definition,
  )
where

import AST.Canonical (Type (..))
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified Compile
import qualified Data.Binary.Get as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Name (Name)
import qualified Data.Name as Name
import qualified Data.Set as Set
import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Utf8
import Data.Word (Word16)
import qualified Elm.Details
import qualified Elm.ModuleName
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String
import Ext.Common (debug)
import qualified Ext.CompileProxy
import Json.Encode ((==>))
import qualified Json.Encode
import qualified Json.String
import qualified Llamadera
import qualified Reporting
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import Reporting.Error.Docs (SyntaxProblem (Name))
import qualified Reporting.Exit as Exit
import StandaloneInstances
import qualified Stuff as PerUserCache
import qualified System.Directory as Dir
import qualified Text.Show.Unicode
import qualified Util
import qualified Watchtower.Details

{- Find Definition -}

definition :: FilePath -> Watchtower.Details.PointLocation -> IO Json.Encode.Value
definition root (Watchtower.Details.PointLocation path point) = do
  Right (Compile.Artifacts modul typeMap localGraph) <-
    Ext.CompileProxy.loadSingleArtifacts root path

  let found = modul & Can._decls & findAtPoint point

  case found of
    FoundNothing -> do
      pure Json.Encode.null
    FoundExpr expr patterns -> do
      case getLocatedDetails expr of
        Nothing -> do
          fail ("Could not locate expression: " ++ show expr)
        Just (Local localName) -> do
          findFirstPatternIntroducing localName patterns
            & encodeResult path
            & pure
        Just (External canMod name) ->
          findExternalWith findFirstValueNamed name Src._values canMod
        Just (Ctor canMod name) ->
          findExternalWith findFirstCtorNamed name Src._unions canMod
      where
        encodeResult path result =
          case result of
            Nothing ->
              Json.Encode.null
            Just (A.At region _) ->
              Json.Encode.object
                [ ( "definition",
                    Json.Encode.object
                      [ ("region", Watchtower.Details.encodeRegion region),
                        ("path", Json.Encode.string (Json.String.fromChars path))
                      ]
                  )
                ]

        findExternalWith findFn name listAccess canMod = do
          details <- Ext.CompileProxy.loadProject

          case lookupModulePath details canMod of
            Nothing ->
              fail "could not find path"
            Just targetPath -> do
              Right (stringSource, sourceMod) <- Ext.CompileProxy.loadFileSource root targetPath

              listAccess sourceMod
                & findFn name
                & encodeResult targetPath
                & pure
    FoundPattern _ -> do
      fail "finding patterns, not implemented"

lookupModulePath :: Elm.Details.Details -> ModuleName.Canonical -> Maybe FilePath
lookupModulePath details canModuleName =
  details
    & Elm.Details._locals
    & Map.lookup (ModuleName._module canModuleName)
    & fmap Elm.Details._path

-- Match the AST node at the specified position

data SearchResult
  = FoundNothing
  | FoundExpr Can.Expr [Can.Pattern]
  | FoundPattern Can.Pattern
  deriving (Show)

findAtPoint :: A.Position -> Can.Decls -> SearchResult
findAtPoint point decls =
  case decls of
    Can.SaveTheEnvironment ->
      FoundNothing
    Can.Declare def next ->
      case findDef point [] def of
        FoundNothing ->
          findAtPoint point next
        found ->
          found
    Can.DeclareRec def defs next ->
      -- TODO: check defs too!
      case findDef point [] def of
        FoundNothing ->
          findFirstInList (findDef point []) defs
            & orFind (findAtPoint point) next
        found ->
          found

findDef :: A.Position -> [Can.Pattern] -> Can.Def -> SearchResult
findDef point foundPatterns def =
  case def of
    Can.Def locatedName patterns expr ->
      findExpr point (patterns ++ foundPatterns) expr
    Can.TypedDef locatedName freeVars patternsWithTypes expr type_ ->
      findExpr point (map fst patternsWithTypes ++ foundPatterns) expr

findExpr :: A.Position -> [Can.Pattern] -> Can.Expr -> SearchResult
findExpr point foundPatterns expr@(A.At region expr_) =
  if withinRegion point region
    then case refineMatch point foundPatterns expr_ of
      FoundNothing ->
        FoundExpr expr foundPatterns
      refined ->
        refined
    else FoundNothing

withinRegion :: A.Position -> A.Region -> Bool
withinRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  ((row == startRow && col >= startCol) || row > startRow) && ((row == endRow && col <= endCol) || row < endRow)

refineMatch :: A.Position -> [Can.Pattern] -> Can.Expr_ -> SearchResult
refineMatch point foundPatterns expr_ =
  case expr_ of
    Can.List exprs ->
      findFirstInList dive exprs
    Can.Negate expr ->
      dive expr
    Can.Binop name canName otherName annotation exprOne exprTwo ->
      dive exprOne
        & orFind dive exprTwo
    Can.Lambda patterns expr ->
      -- findFirstInList (findPattern point) patterns
      -- & orFind
      extendAndDive patterns expr
    Can.Call expr exprs ->
      findFirstInList dive exprs
        & orFind dive expr
    Can.If listTupleExprs expr ->
      findFirstInList
        ( \(one, two) ->
            dive one
              & orFind dive two
        )
        listTupleExprs
        & orFind dive expr
    Can.Let def expr ->
      findDef point foundPatterns def
        & orFind (extendAndDive [defNamePattern def]) expr
    Can.LetRec defs expr ->
      findFirstInList (findDef point foundPatterns) defs
        & orFind (extendAndDive (map defNamePattern defs)) expr
    Can.LetDestruct pattern one two ->
      dive one
        & orFind (extendAndDive [pattern]) two
    Can.Case expr branches ->
      dive expr
        & orFind
          ( findFirstInList $
              \(Can.CaseBranch pattern expr) ->
                extendAndDive [pattern] expr
          )
          branches
    Can.Access expr locatedName ->
      dive expr
    Can.Update name expr fields ->
      fields
        & Map.toAscList
        & findFirstInList
          (\(fieldName, Can.FieldUpdate region fieldExpr) -> dive fieldExpr)
        & orFind dive expr
    Can.Record fields ->
      fields
        & Map.toAscList
        & findFirstInList
          (\(fieldName, fieldExpr) -> dive fieldExpr)
    Can.Tuple one two maybeThree ->
      dive one
        & orFind dive two
        & orFind (maybe FoundNothing dive) maybeThree
    _ -> FoundNothing
  where
    dive = findExpr point foundPatterns

    extendAndDive newPatterns = findExpr point (newPatterns ++ foundPatterns)

defNamePattern :: Can.Def -> Can.Pattern
defNamePattern def =
  case def of
    Can.Def (A.At region name) _ _ ->
      A.At region $ Can.PVar name
    Can.TypedDef (A.At region name) _ _ _ _ ->
      A.At region $ Can.PVar name

findPattern :: A.Position -> Can.Pattern -> SearchResult
findPattern point pattern@(A.At region patt) =
  if withinRegion point region
    then FoundPattern pattern
    else FoundNothing

orFind :: (t -> SearchResult) -> t -> SearchResult -> SearchResult
orFind toNewResult val existing =
  case existing of
    FoundNothing ->
      toNewResult val
    _ ->
      existing

findFirstInList :: (t -> SearchResult) -> [t] -> SearchResult
findFirstInList toNewResult vals =
  case vals of
    [] ->
      FoundNothing
    (top : remain) ->
      case toNewResult top of
        FoundNothing ->
          findFirstInList toNewResult remain
        searchResult ->
          searchResult

-- Classify matched expression so we know where to search

data LocatedValue
  = Local Name
  | External ModuleName.Canonical Name
  | Ctor ModuleName.Canonical Name

getLocatedDetails :: Can.Expr -> Maybe LocatedValue
getLocatedDetails (A.At region expr) =
  case expr of
    Can.VarLocal name ->
      Just (Local name)
    Can.VarTopLevel mod name ->
      Just (External mod name)
    Can.VarKernel oneName twoName ->
      Nothing
    Can.VarForeign mod name ann ->
      Just (External mod name)
    Can.VarCtor _ mod name idx ann ->
      Just (Ctor mod name)
    Can.VarDebug mod name ann ->
      Just (External mod name)
    Can.VarOperator firstName mod name ann ->
      Just (External mod name)
    Can.Chr _ ->
      Nothing
    Can.Str _ ->
      Nothing
    Can.Int i ->
      Nothing
    Can.Float f ->
      Nothing
    Can.List listExprs ->
      Nothing
    Can.Negate expr ->
      Nothing
    Can.Binop otherName mod name ann exprOne exprTwo ->
      Nothing
    Can.Lambda patterns expr ->
      Nothing
    Can.Call expr exprs ->
      Nothing
    Can.If branches return ->
      Nothing
    Can.Let def expr ->
      Nothing
    Can.LetRec defs expr ->
      Nothing
    Can.LetDestruct pattern one two ->
      Nothing
    Can.Case expr caseBranch ->
      Nothing
    Can.Accessor name ->
      Nothing
    Can.Access expr locatedName ->
      Nothing
    Can.Update name expr fieldUpdates ->
      Nothing
    Can.Record fields ->
      Nothing
    Can.Unit ->
      Nothing
    Can.Tuple one two three ->
      Nothing
    Can.Shader shader types ->
      Nothing

-- Find the definition

findFirstValueNamed :: Name.Name -> [A.Located Src.Value] -> Maybe (A.Located Src.Value)
findFirstValueNamed name list =
  case list of
    [] ->
      Nothing
    (top@(A.At _ ((Src.Value (A.At _ valName) _ _ _))) : remain) ->
      if name == valName
        then Just top
        else findFirstValueNamed name remain

findFirstCtorNamed :: Name.Name -> [A.Located Src.Union] -> Maybe (A.Located Name.Name)
findFirstCtorNamed name =
  findFirstJust findUnion
  where
    findUnion (A.At _ ((Src.Union _ _ ctors))) =
      findFirstJust findCtor ctors

    findCtor (nameAt@(A.At _ ctorName), _) =
      if ctorName == name
        then Just nameAt
        else Nothing

findFirstPatternIntroducing :: Name.Name -> [Can.Pattern] -> Maybe Can.Pattern
findFirstPatternIntroducing name =
  findFirstJust (findPatternIntroducing name)

findPatternIntroducing :: Name.Name -> Can.Pattern -> Maybe Can.Pattern
findPatternIntroducing name pattern@(A.At _ pattern_) =
  case pattern_ of
    Can.PVar pname ->
      if pname == name
        then Just pattern
        else Nothing
    Can.PRecord names ->
      if name `elem` names
        then Just pattern
        else Nothing
    Can.PAlias subPattern aliasName ->
      if aliasName == name
        then Just pattern
        else findPatternIntroducing name subPattern
    Can.PTuple a b c ->
      inList ([a, b] ++ Maybe.maybeToList c)
    Can.PList subPatterns ->
      inList subPatterns
    Can.PCons a b ->
      inList [a, b]
    Can.PCtor _ _ _ _ _ args ->
      args
        & map Can._arg
        & inList
    _ ->
      Nothing
  where
    inList =
      findFirstJust (findPatternIntroducing name)

-- Helpers

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
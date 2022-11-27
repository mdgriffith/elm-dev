{-# LANGUAGE OverloadedStrings #-}

module Ext.Dev.Find
  ( definition
  , usedModules
  , instances
  )
where

import AST.Canonical (Type (..))
import qualified AST.Canonical as Can
import qualified AST.Optimized as Opt
import qualified AST.Source as Src
import qualified Canonicalize.Environment
import qualified Canonicalize.Type
import qualified Compile
import Control.Applicative ((<|>))

import qualified Data.Set as Set
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
import qualified Elm.ModuleName as ModuleName
import qualified Elm.String
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
import qualified Reporting.Result
import StandaloneInstances
import qualified Stuff as PerUserCache
import qualified System.Directory as Dir
import qualified Text.Show.Unicode
import qualified Util
import qualified Watchtower.Editor
import qualified Ext.Dev.Find.Canonical
import Watchtower.Editor (PointRegion(..))
import qualified Data.Maybe

{- Find Definition -}


definition :: FilePath -> Watchtower.Editor.PointLocation -> IO (Maybe PointRegion)
definition root (Watchtower.Editor.PointLocation path point) = do
  (Ext.CompileProxy.Single source warnings canonical compiled) <- Ext.CompileProxy.loadSingle root path
  case source of
    Left _ ->
       pure Nothing
    
    Right srcMod -> do
      let foundType = findTypeAtPoint point srcMod

      found <-
        case foundType of
          FoundNothing -> do
            case canonical of
              Just canMod ->
                pure $ findDeclAtPoint point (Can._decls canMod)
              
              Nothing ->
                pure foundType

          existing ->
            pure existing

      case found of
        FoundNothing -> do
          pure Nothing

        FoundExpr expr patterns -> do
          case getLocatedDetails expr of
            Nothing ->
                pure Nothing

            Just (Local localName) ->
              findFirstPatternIntroducing localName patterns
                & defResult path
                & pure

            Just (External extCanMod name) ->
              findExternalWith findFirstValueNamed name Src._values extCanMod

            Just (Ctor extCanMod name) ->
              findExternalWith findFirstCtorNamed name Src._unions extCanMod

        FoundPattern (A.At _ (Can.PCtor extCanMod _ _ ctorName _ _)) -> do
          findExternalWith findFirstCtorNamed ctorName Src._unions extCanMod

        FoundPattern _ ->
          pure Nothing

        FoundType tipe -> do
          canonicalizationEnvResult <- Ext.CompileProxy.loadCanonicalizeEnv root path srcMod
          case canonicalizationEnvResult of
            Nothing ->
              pure Nothing

            Just env -> do
              let (_, eitherCanType) = Reporting.Result.run $ Canonicalize.Type.canonicalize env tipe

              case eitherCanType of
                Left err ->
                  pure Nothing

                Right (Can.TType extCanMod name _) ->
                  findExternalWith findFirstTypeNamed name id extCanMod

                Right _ ->
                  pure Nothing
      where
        findExternalWith findFn name listAccess canMod = do
          details <- Ext.CompileProxy.loadProject

          case lookupModulePath details canMod of
            Nothing ->
              pure Nothing

            Just targetPath -> do
              loadedFile <- Ext.CompileProxy.loadFileSource root targetPath
              case loadedFile of
                Right (_, sourceMod) -> do
                  listAccess sourceMod
                    & findFn name
                    & defResult targetPath
                    & pure
                
                Left _ ->
                  pure Nothing

defResult :: FilePath -> Maybe (A.Located a) -> Maybe PointRegion
defResult path =
  fmap
    ( \(A.At region _) ->
        PointRegion
          { _regionFile = path,
            _region = region
          }
    )

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
  | FoundType Src.Type
  deriving (Show)

findTypeAtPoint :: A.Position -> Src.Module -> SearchResult
findTypeAtPoint point srcMod =
  findAnnotation point (Src._values srcMod)
    & orFind (findUnion point) (Src._unions srcMod)
    & orFind (findAlias point) (Src._aliases srcMod)

findDeclAtPoint :: A.Position -> Can.Decls -> SearchResult
findDeclAtPoint point decls =
  case decls of
    Can.SaveTheEnvironment ->
      FoundNothing
    Can.Declare def next ->
      case findDef point [] def of
        FoundNothing ->
          findDeclAtPoint point next
        found ->
          found
    Can.DeclareRec def defs next ->
      -- TODO: check defs too!
      case findDef point [] def of
        FoundNothing ->
          findFirstInList (findDef point []) defs
            & orFind (findDeclAtPoint point) next
        found ->
          found

findDef :: A.Position -> [Can.Pattern] -> Can.Def -> SearchResult
findDef point foundPatterns def =
  case def of
    Can.Def locatedName patterns expr ->
      findFirstInList (findPattern point) patterns
        & orFind (findExpr point (patterns ++ foundPatterns)) expr
    Can.TypedDef locatedName freeVars patternsWithTypes expr type_ ->
      findFirstInList (findPattern point) patterns
        & orFind (findExpr point (patterns ++ foundPatterns)) expr
      where
        patterns = map fst patternsWithTypes

findExpr :: A.Position -> [Can.Pattern] -> Can.Expr -> SearchResult
findExpr point foundPatterns expr@(A.At region expr_) =
  if withinRegion point region
    then case refineExprMatch point foundPatterns expr_ of
      FoundNothing ->
        FoundExpr expr foundPatterns
      refined ->
        refined
    else FoundNothing

withinRegion :: A.Position -> A.Region -> Bool
withinRegion (A.Position row col) (A.Region (A.Position startRow startCol) (A.Position endRow endCol)) =
  (row == startRow && col >= startCol || row > startRow) && (row == endRow && col <= endCol || row < endRow)

refineExprMatch :: A.Position -> [Can.Pattern] -> Can.Expr_ -> SearchResult
refineExprMatch point foundPatterns expr_ =
  case expr_ of
    Can.List exprs ->
      findFirstInList dive exprs
    Can.Negate expr ->
      dive expr
    Can.Binop name canName otherName annotation exprOne exprTwo ->
      dive exprOne
        & orFind dive exprTwo
    Can.Lambda patterns expr ->
      findFirstInList (findPattern point) patterns
        & orFind (extendAndDive patterns) expr
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
      findPattern point pattern
        & orFind dive one
        & orFind (extendAndDive [pattern]) two
    Can.Case expr branches ->
      dive expr
        & orFind
          ( findFirstInList $
              \(Can.CaseBranch pattern expr) ->
                findPattern point pattern
                  & orFind (extendAndDive [pattern]) expr
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
findPattern point pattern@(A.At region pattern_) =
  if withinRegion point region
    then case refinePatternMatch point pattern_ of
      FoundNothing ->
        FoundPattern pattern
      refined ->
        refined
    else FoundNothing

refinePatternMatch :: A.Position -> Can.Pattern_ -> SearchResult
refinePatternMatch point pattern_ =
  case pattern_ of
    Can.PAlias subPattern _ ->
      dive subPattern
    Can.PTuple a b c ->
      diveList ([a, b] ++ Maybe.maybeToList c)
    Can.PList subPatterns ->
      diveList subPatterns
    Can.PCons a b ->
      diveList [a, b]
    Can.PCtor _ _ _ _ _ args ->
      args
        & map Can._arg
        & diveList
    _ ->
      FoundNothing
  where
    dive = findPattern point

    diveList = findFirstInList dive

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

findRegion :: A.Position -> (a -> SearchResult) -> [A.Located a] -> SearchResult
findRegion point findFn =
  findFirstInList
    ( \(A.At region node) ->
        if withinRegion point region
          then findFn node
          else FoundNothing
    )

findAnnotation :: A.Position -> [A.Located Src.Value] -> SearchResult
findAnnotation point =
  findFirstInList
    ( \(A.At _ (Src.Value _ _ _ typeAnn)) ->
        maybe FoundNothing (findType point) typeAnn
    )

findAlias :: A.Position -> [A.Located Src.Alias] -> SearchResult
findAlias point = findRegion point (\(Src.Alias _ _ type_) -> findType point type_)

findUnion :: A.Position -> [A.Located Src.Union] -> SearchResult
findUnion point =
  findRegion
    point
    ( \(Src.Union _ _ ctors) ->
        findFirstInList (findFirstInList (findType point) . snd) ctors
    )

findType :: A.Position -> Src.Type -> SearchResult
findType point tipe@(A.At region type_) =
  if withinRegion point region
    then case refineTypeMatch point type_ of
      FoundNothing ->
        FoundType tipe
      refined ->
        refined
    else FoundNothing

refineTypeMatch :: A.Position -> Src.Type_ -> SearchResult
refineTypeMatch point type_ =
  case type_ of
    Src.TLambda arg ret ->
      dive arg
        & orFind dive ret
    Src.TVar _ ->
      FoundNothing
    Src.TType _ _ tvars ->
      findFirstInList dive tvars
    Src.TTypeQual _ _ _ tvars ->
      findFirstInList dive tvars
    Src.TRecord fields extRecord ->
      -- TODO: Check extRecord
      findFirstInList (dive . snd) fields
    Src.TUnit ->
      FoundNothing
    Src.TTuple a b rest ->
      findFirstInList dive (a : b : rest)
  where
    dive = findType point

-- Classify matched expression or type so we know where to search

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

findFirstTypeNamed :: Name.Name -> Src.Module -> Maybe (A.Located ())
findFirstTypeNamed name mod =
  findFirstJust findAlias (Src._aliases mod)
    <|> findFirstJust findUnion (Src._unions mod)
  where
    findAlias (A.At region (Src.Alias (A.At _ aliasName) _ _)) =
      if name == aliasName
        then Just (A.At region ())
        else Nothing

    findUnion (A.At region (Src.Union (A.At _ unionName) _ _)) =
      if name == unionName
        then Just (A.At region ())
        else Nothing

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



{- GATHER USED MODULES -}


{-|

  Given a root and a target file, return a set of all modules that are used.

-}
usedModules :: FilePath -> FilePath -> IO (Maybe (Set.Set ModuleName.Canonical))
usedModules root path = do
    (Ext.CompileProxy.Single source warnings canonical compiled) <- Ext.CompileProxy.loadSingle root path
    pure (fmap Ext.Dev.Find.Canonical.used canonical)
    
    


{- Instances -}

instances :: FilePath -> Watchtower.Editor.PointLocation -> Bool -> IO [PointRegion]
instances path location includeDeclaration =
  if includeDeclaration then do
    def <- definition path location
    pure $ Data.Maybe.maybeToList def
  else
    pure []

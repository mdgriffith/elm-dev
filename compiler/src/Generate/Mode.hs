module Generate.Mode
  ( Mode(..)
  , isDebug
  , ShortFieldNames
  , RawFunctions
  , RawFunction(..)
  , UnwrappedFunctions
  , UnwrappedFunction(..)
  , shortenFieldNames
  , rawFunctions
  , unwrappedFunctions
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Name as Name

import qualified AST.Optimized as Opt
import qualified Elm.Compiler.Type.Extract as Extract
import qualified Ext.Optimization.Level as Optimization
import qualified Generate.JavaScript.Name as JsName



-- MODE


data Mode
  = Dev (Maybe Extract.Types)
  | Prod Optimization.Level ShortFieldNames RawFunctions
      UnwrappedFunctions


type RawFunctions =
  Map.Map Opt.Global RawFunction


data RawFunction =
  RawFunction
    { _rawGlobal :: Opt.Global
    , _rawArity :: Int
    }


type UnwrappedFunctions =
  Map.Map Opt.Global UnwrappedFunction


data UnwrappedFunction =
  UnwrappedFunction
    { _unwrappedArity :: Int
    , _unwrappedParamIndex :: Int
    , _unwrappedParamArity :: Int
    }


isDebug :: Mode -> Bool
isDebug mode =
  case mode of
    Dev mi -> Maybe.isJust mi
    Prod _ _ _ _ -> False



-- SHORTEN FIELD NAMES


type ShortFieldNames =
  Map.Map Name.Name JsName.Name


shortenFieldNames :: Opt.GlobalGraph -> ShortFieldNames
shortenFieldNames (Opt.GlobalGraph _ frequencies) =
  Map.foldr addToShortNames Map.empty $
    Map.foldrWithKey addToBuckets Map.empty frequencies


addToBuckets :: Name.Name -> Int -> Map.Map Int [Name.Name] -> Map.Map Int [Name.Name]
addToBuckets field frequency buckets =
  Map.insertWith (++) frequency [field] buckets


addToShortNames :: [Name.Name] -> ShortFieldNames -> ShortFieldNames
addToShortNames fields shortNames =
  List.foldl' addField shortNames fields


addField :: ShortFieldNames -> Name.Name -> ShortFieldNames
addField shortNames field =
  let rename = JsName.fromInt (Map.size shortNames) in
  Map.insert field rename shortNames



-- RAW FUNCTIONS


rawFunctions :: Opt.GlobalGraph -> RawFunctions
rawFunctions (Opt.GlobalGraph nodes _) =
  let
    direct = Map.mapMaybeWithKey rawFunctionFromNode nodes
  in
  Map.foldrWithKey (addLink direct) direct nodes


rawFunctionFromNode :: Opt.Global -> Opt.Node -> Maybe RawFunction
rawFunctionFromNode global node =
  case node of
    Opt.Define (Opt.Function args _) _ | isRawArity (length args) ->
      Just (RawFunction global (length args))

    Opt.DefineTailFunc args _ _ | isRawArity (length args) ->
      Just (RawFunction global (length args))

    _ ->
      Nothing


addLink :: RawFunctions -> Opt.Global -> Opt.Node -> RawFunctions -> RawFunctions
addLink direct global node rawMap =
  case node of
    Opt.Link linkedGlobal ->
      case Map.lookup linkedGlobal direct of
        Just rawFunction -> Map.insert global rawFunction rawMap
        Nothing -> rawMap

    _ ->
      rawMap


isRawArity :: Int -> Bool
isRawArity arity =
  2 <= arity && arity <= 9



-- UNWRAPPED FUNCTIONS


unwrappedFunctions :: Opt.GlobalGraph -> UnwrappedFunctions
unwrappedFunctions (Opt.GlobalGraph nodes _) =
  Map.mapMaybe unwrappedFunctionFromNode nodes


unwrappedFunctionFromNode :: Opt.Node -> Maybe UnwrappedFunction
unwrappedFunctionFromNode node =
  case node of
    Opt.Define (Opt.Function args body) _ ->
      findUnwrappedParam args body

    _ ->
      Nothing


findUnwrappedParam :: [Name.Name] -> Opt.Expr -> Maybe UnwrappedFunction
findUnwrappedParam args body =
  case foldr addCandidate [] (zip [0..] args) of
    [unwrapped] -> Just unwrapped
    _ -> Nothing
  where
    functionArity = length args

    addCandidate (index, arg) candidates =
      case localFunctionParamCallArity arg body of
        Just paramArity | isRawArity paramArity ->
          UnwrappedFunction functionArity index paramArity : candidates

        _ ->
          candidates


localFunctionParamCallArity :: Name.Name -> Opt.Expr -> Maybe Int
localFunctionParamCallArity target expr =
  let
    usages = collectLocalFunctionParamUsages target expr
  in
  case usages of
    [] -> Nothing
    arity : rest | all (arity ==) rest -> Just arity
    _ -> Nothing


collectLocalFunctionParamUsages :: Name.Name -> Opt.Expr -> [Int]
collectLocalFunctionParamUsages target expr =
  case expr of
    Opt.VarLocal name | name == target ->
      [-1]

    Opt.Call (Opt.VarLocal name) args | name == target ->
      length args : concatMap (collectLocalFunctionParamUsages target) args

    Opt.Call func args ->
      collectLocalFunctionParamUsages target func ++ concatMap (collectLocalFunctionParamUsages target) args

    Opt.Function args body | target `elem` args ->
      []

    Opt.Function _ body ->
      collectLocalFunctionParamUsages target body

    Opt.List entries ->
      concatMap (collectLocalFunctionParamUsages target) entries

    Opt.TailCall _ args ->
      concatMap (collectLocalFunctionParamUsages target . snd) args

    Opt.If branches final ->
      concatMap collectBranch branches ++ collectLocalFunctionParamUsages target final

    Opt.Let def body ->
      collectDef def ++ collectLocalFunctionParamUsages target body

    Opt.Destruct _ body ->
      collectLocalFunctionParamUsages target body

    Opt.Case _ _ _ jumps ->
      concatMap (collectLocalFunctionParamUsages target . snd) jumps

    Opt.Access record _ ->
      collectLocalFunctionParamUsages target record

    Opt.Update record fields ->
      collectLocalFunctionParamUsages target record ++ concatMap (collectLocalFunctionParamUsages target) (Map.elems fields)

    Opt.Record fields ->
      concatMap (collectLocalFunctionParamUsages target) (Map.elems fields)

    Opt.Tuple a b maybeC ->
      collectLocalFunctionParamUsages target a ++ collectLocalFunctionParamUsages target b ++ maybe [] (collectLocalFunctionParamUsages target) maybeC

    _ ->
      []
  where
    collectBranch (condition, branch) =
      collectLocalFunctionParamUsages target condition ++ collectLocalFunctionParamUsages target branch

    collectDef def =
      case def of
        Opt.Def name body | name == target -> []
        Opt.Def _ body -> collectLocalFunctionParamUsages target body
        Opt.TailDef name _ body | name == target -> []
        Opt.TailDef _ args body | target `elem` args -> []
        Opt.TailDef _ _ body -> collectLocalFunctionParamUsages target body

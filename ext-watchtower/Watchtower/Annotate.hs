{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Annotate
  ( run,
    listMissingAnnotations,
    printListMissingAnnotations,
    annotation,
    printAnnotation,
    callgraph,
    printCallGraph,
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
import qualified Data.String
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

{-

Lookup and print out the type annotation for the given file:expression.

@TODO this only works if we've already compiled successfully (no errors) before!

If we instead want to do a check compile first, we'll need to use Lamdera.Compile.make.

Note: if we do this, this annotation query will involve a full disk read of all
.elm file metadata for the entire project on every single query, as Elm tries to
figure out if there's any changes requiring an incremental compile.

@FUTURE @TRACK inotify-mode feature (https://trello.com/c/S1LmVKbK) would solve this!

-}
run :: Args -> () -> IO ()
run (Args file expressionName) () = do
  Llamadera.debug_ "Starting annotation..."

  elmHome <- PerUserCache.getElmHome
  root <- Llamadera.getProjectRoot
  printAnnotation root file expressionName

-- getTopLevelNameAt :: A.Position -> Src.Module -> Maybe Name.Name
-- getTopLevelNameAt position mod =
--   mod
--     & Src._values
--     & findFirstJust
--       ( \val ->
--           case A.toValue val of
--             Src.Value locatedName _ _ Nothing ->
--               if (withinRegion position (A.toRegion locatedName))
--                 then Just (A.toValue locatedName)
--                 else Nothing
--             _ ->
--               Nothing
--       )

{- For editors to add a button that prompts to insert a missing type annotation, they need to know which annotations are missing.

This will list those annotations as well as their coordinates

-}
printListMissingAnnotations :: FilePath -> FilePath -> IO ()
printListMissingAnnotations root file = do
  mSource <- Llamadera.loadFileSource root file
  case mSource of
    Left err ->
      putStrLn $ show err
    Right (source, modul) -> do
      let values =
            modul
              & Src._values
              & fmap
                ( \val ->
                    case A.toValue val of
                      Src.Value locatedName _ _ Nothing ->
                        Json.Encode.object
                          [ "name" ==> nameToJsonString (A.toValue locatedName),
                            "region" ==> Watchtower.Details.encodeRegion (A.toRegion locatedName)
                          ]
                          & Just
                      _ ->
                        Nothing
                )
              & Maybe.catMaybes
              & Json.Encode.list (\a -> a)
              & Json.Encode.encode

      B.hPutBuilder stderr values

      pure ()

{- For editors to add a button that prompts to insert a missing type annotation, they need to know which annotations are missing.

This will list those annotations as well as their coordinates

-}
listMissingAnnotations :: FilePath -> FilePath -> IO Json.Encode.Value
listMissingAnnotations root file = do
  mSource <- Llamadera.loadFileSource root file
  eitherArtifacts <- Llamadera.loadSingleArtifacts root file
  case mSource of
    Left err ->
      pure $ jsonError err
    Right (source, modul) ->
      case eitherArtifacts of
        Left err ->
          pure $ Json.Encode.string (Json.String.fromChars "Not compiling")
        Right artifacts ->
          do
            let values =
                  modul
                    & Src._values
                    & fmap
                      ( \val ->
                          case A.toValue val of
                            Src.Value locatedName _ _ Nothing ->
                              let maybeAnnotation = getAnnotation modul (A.toValue locatedName) artifacts
                               in case maybeAnnotation of
                                    -- Maybe this happens when we're not compiling yet?
                                    Nothing ->
                                      Nothing
                                    Just annotationJson ->
                                      Json.Encode.object
                                        [ "filepath" ==> Json.Encode.string (Json.String.fromChars file),
                                          "name" ==> nameToJsonString (A.toValue locatedName),
                                          "region" ==> Watchtower.Details.encodeRegion (A.toRegion locatedName),
                                          "signature" ==> annotationJson
                                        ]
                                        & Just
                            _ ->
                              Nothing
                      )
                    & Maybe.catMaybes
                    & Json.Encode.list (\a -> a)

            pure values

{-  Print a type signature!

-}
printAnnotation :: FilePath -> FilePath -> Name.Name -> IO ()
printAnnotation root file expressionName = do
  mSource <- Llamadera.loadFileSource root file
  case mSource of
    Left err ->
      putStrLn $ show err
    Right (source, modul) -> do
      Dir.withCurrentDirectory root $ do
        Llamadera.debug_ "Getting artifacts..."

        eitherArtifacts <- Llamadera.loadSingleArtifacts root file
        case eitherArtifacts of
          Right (Compile.Artifacts mod annotations (Opt.LocalGraph main graph fields)) -> do
            case annotations & Map.lookup expressionName of
              Just annotation -> do
                let imports = modul & Src._imports
                    selfName = modul & Src.getName

                -- Llamadera.formatHaskellValue ("debug AST") annotation
                putStrLn $ T.unpack $ canonicalAnnotationToString selfName imports annotation
              Nothing ->
                putStrLn "Oops! Something went wrong!"
          Left err ->
            putStrLn $ show err

      pure ()

{-  Return a type signature!
-}
annotation :: FilePath -> FilePath -> Name.Name -> IO Json.Encode.Value
annotation root file expressionName = do
  mSource <- Llamadera.loadFileSource root file
  case mSource of
    Left err ->
      pure $ jsonError err
    Right (source, modul) -> do
      do
        eitherArtifacts <- Llamadera.loadSingleArtifacts root file
        case eitherArtifacts of
          Right artifacts -> do
            case getAnnotation modul expressionName artifacts of
              Nothing ->
                pure (Json.Encode.string (Json.String.fromChars "Not found"))
              Just signature ->
                pure
                  ( Json.Encode.object
                      [ "signature" ==> signature
                      ]
                  )
          Left err ->
            -- @TODO can we use Elm's error JSON formatter here?
            pure $ jsonError err

getAnnotation ::
  Src.Module ->
  Name.Name ->
  Compile.Artifacts ->
  Maybe Json.Encode.Value
getAnnotation modul expressionName (Compile.Artifacts mod annotations (Opt.LocalGraph main graph fields)) =
  case annotations & Map.lookup expressionName of
    Just annotation ->
      let imports = modul & Src._imports
          selfName = modul & Src.getName
       in Just
            ( Json.Encode.string
                ( Json.String.fromChars
                    (T.unpack (canonicalAnnotationToString selfName imports annotation))
                )
            )
    Nothing ->
      Nothing

-- Args helpers

data Args = Args FilePath Name.Name

expressionName :: Parser Name.Name
expressionName =
  Parser
    { _singular = "expression name",
      _plural = "expression names",
      _parser = parseExpressionName,
      _suggest = suggestExpressionName,
      _examples = return . exampleExpressionNames
    }

parseExpressionName :: String -> Maybe Name.Name
parseExpressionName chars =
  Just $ Name.fromChars chars

suggestExpressionName :: String -> IO [String]
suggestExpressionName _ =
  return []

exampleExpressionNames :: String -> [String]
exampleExpressionNames chars =
  ["add", "addOneMore", "map3"]

{-

= TLambda Type Type
| TVar Name
| TType ModuleName.Canonical Name [Type]
| TRecord (Map.Map Name FieldType) (Maybe Name)
| TUnit
| TTuple Type Type (Maybe Type)
| TAlias ModuleName.Canonical Name [(Name, Type)] AliasType

For operations on Text
https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html

|> == &
<| == $

-}
canonicalAnnotationToString :: Name.Name -> [Src.Import] -> Can.Annotation -> Text
canonicalAnnotationToString selfName imports (Can.Forall freeVars tipe) =
  if canonicalTypeStringLength selfName imports tipe > 80
    then "\\n    " <> canonicalTypeToMultilineString selfName imports tipe
    else canonicalTypeToString selfName imports tipe

canonicalTypeStringLength :: Name.Name -> [Src.Import] -> Can.Type -> Int
canonicalTypeStringLength self imports tipe =
  case tipe of
    TLambda t1 t2 ->
      canonicalTypeStringLength self imports t1
        + canonicalTypeStringLength self imports t2
        + 3
    TVar name ->
      List.length (Name.toChars name)
    TType mod name paramTypes ->
      qualifierLength self imports mod name
        + Data.Utf8.size name
        + ( case paramTypes of
              [] -> 0
              _ ->
                1
                  + List.foldl
                    ( \acc pType ->
                        acc + canonicalTypeStringLength self imports pType
                    )
                    0
                    paramTypes
          )
    TRecord fieldTypes extensibleName ->
      2
        + ( case extensibleName of
              Nothing ->
                0
              Just ext ->
                Data.Utf8.size ext + 2
          )
        + ( List.foldl
              ( \acc (fieldName, fieldType) ->
                  acc
                    + Data.Utf8.size fieldName
                    + 3
                    + canonicalTypeStringLength self imports fieldType
              )
              0
              (Can.fieldsToList fieldTypes)
          )
        + 2
    TUnit ->
      2
    TTuple t1 t2 mt3 ->
      canonicalTypeStringLength self imports t1
        + 4 -- paren, space, comma, space
        + canonicalTypeStringLength self imports t2
        + 2
        + ( case mt3 of
              Nothing -> 0
              Just t3 ->
                canonicalTypeStringLength self imports t3 + 2
          )
    TAlias mod name namedParamTypes alias ->
      qualifierLength self imports mod name
        + Data.Utf8.size name

qualifierLength :: Name.Name -> [Src.Import] -> Elm.ModuleName.Canonical -> Name.Name -> Int
qualifierLength self imports (can@(Elm.ModuleName.Canonical pkg mdl)) value =
  if mdl == self
    then 0
    else
      if isBuiltIn can value
        then 0
        else case matchImport can imports value of
          Just (matched@(Src.Import _ (Just alias) _)) ->
            Data.Utf8.size alias + 1
          Just matched ->
            Data.Utf8.size (Src.getImportName matched) + 1
          Nothing ->
            0

singleName :: [Can.Type] -> Bool
singleName types =
  case types of
    [] ->
      True
    (Can.TVar _ : []) ->
      True
    (Can.TType _ _ [] : []) ->
      True
    (Can.TAlias _ _ _ _ : []) ->
      True
    _ ->
      False

isLambda :: Type -> Bool
isLambda tipe =
  case tipe of
    TLambda _ _ ->
      True
    _ ->
      False

parensIfLambda :: (Semigroup p, Data.String.IsString p) => Type -> p -> p
parensIfLambda tipe str =
  if isLambda tipe
    then "(" <> str <> ")"
    else str

canonicalTypeToString :: Name.Name -> [Src.Import] -> Can.Type -> Text
canonicalTypeToString self imports tipe =
  case tipe of
    TLambda t1 t2 ->
      parensIfLambda t1 (canonicalTypeToString self imports t1)
        <> " -> "
        <> canonicalTypeToString self imports t2
    TVar name ->
      T.pack
        (Name.toChars name)
    TType mod name paramTypes ->
      qualifier self imports mod name
        <> T.pack (Name.toChars name)
        <> T.pack "wut?"
        <> ( case paramTypes of
               [] -> ""
               _ ->
                 let typeString = T.intercalate " " (map (canonicalTypeToString self imports) paramTypes)
                  in if singleName paramTypes
                       then " " <> typeString
                       else
                         " ("
                           <> typeString
                           <> ")"
           )
    TRecord fieldTypes extensibleName ->
      "{ "
        <> ( case extensibleName of
               Nothing ->
                 ""
               Just ext ->
                 T.pack (Name.toChars ext) <> " |"
           )
        <> ( map
               ( \(fieldName, fieldType) ->
                   T.pack (Name.toChars fieldName)
                     <> " : "
                     <> canonicalTypeToString self imports fieldType
               )
               (Can.fieldsToList fieldTypes)
               & T.intercalate ", "
           )
        <> " }"
    TUnit ->
      "()"
    TTuple t1 t2 mt3 ->
      "( "
        <> canonicalTypeToString self imports t1
        <> ", "
        <> canonicalTypeToString self imports t1
        <> ( case mt3 of
               Nothing -> ""
               Just t3 ->
                 ", " <> canonicalTypeToString self imports t3
           )
        <> " )"
    TAlias mod name namedParamTypes alias ->
      qualifier self imports mod name
        <> T.pack (Name.toChars name)
        <> ( case namedParamTypes of
               [] -> ""
               _ ->
                 let paramTypes = map snd namedParamTypes
                     typeString = T.intercalate " " (map (canonicalTypeToString self imports) paramTypes)
                  in if singleName paramTypes
                       then " " <> typeString
                       else
                         " ("
                           <> typeString
                           <> ")"
           )

qualifier :: Name.Name -> [Src.Import] -> Elm.ModuleName.Canonical -> Name.Name -> Text
qualifier self imports (can@(Elm.ModuleName.Canonical pkg mdl)) value =
  if mdl == self
    then T.pack ""
    else
      if isBuiltIn can value
        then T.pack ""
        else case matchImport can imports value of
          Just (matched@(Src.Import _ (Just alias) _)) ->
            T.pack (Name.toChars alias) <> "."
          Just matched ->
            T.pack (Name.toChars (Src.getImportName matched)) <> "."
          Nothing ->
            T.pack ""

-- Turns out List isn't explicitly exported anywhere!
isBuiltIn :: Elm.ModuleName.Canonical -> Name.Name -> Bool
isBuiltIn (can@(Elm.ModuleName.Canonical pkg mdl)) value =
  let name = T.pack (Name.toChars mdl)
   in name == "List" && value == "List"

matchImport :: Elm.ModuleName.Canonical -> [Src.Import] -> Name.Name -> Maybe Src.Import
matchImport (can@(Elm.ModuleName.Canonical pkg mdl)) imports value =
  case imports of
    [] -> Nothing
    top : remain ->
      if Src.getImportName top == mdl
        then case Src._exposing top of
          Src.Open ->
            Nothing
          Src.Explicit exposedList ->
            if isExposed value exposedList
              then Nothing
              else Just top
        else matchImport can remain value

isExposed :: Name.Name -> [Src.Exposed] -> Bool
isExposed value expList =
  case expList of
    [] ->
      False
    Src.Lower (A.At _ name) : remain ->
      if name == value
        then True
        else isExposed value remain
    Src.Upper (A.At _ name) privacy : remain ->
      -- we're only dealing with type values, so privacy doesn't matter.
      if name == value
        then True
        else isExposed value remain
    Src.Operator region name : remain ->
      isExposed value remain

canonicalTypeToMultilineString :: Name.Name -> [Src.Import] -> Can.Type -> Text
canonicalTypeToMultilineString self imports tipe =
  case tipe of
    TLambda t1 t2 ->
      canonicalTypeToMultilineString self imports t1
        <> "\\n    -> "
        <> canonicalTypeToMultilineString self imports t2
    TVar name ->
      T.pack (Name.toChars name)
    TType mod name paramTypes ->
      qualifier self imports mod name
        <> T.pack (Name.toChars name)
        <> ( case paramTypes of
               [] -> ""
               _ ->
                 if singleName paramTypes
                   then
                     " "
                       <> T.intercalate
                         " "
                         (map (canonicalTypeToMultilineString self imports) paramTypes)
                   else
                     " ("
                       <> T.intercalate " " (map (canonicalTypeToString self imports) paramTypes)
                       <> ")"
           )
    TRecord fieldTypes extensibleName ->
      "{ "
        <> ( case extensibleName of
               Nothing ->
                 ""
               Just ext ->
                 T.pack (Name.toChars ext) <> " | "
           )
        <> ( map
               ( \(fieldName, fieldType) ->
                   T.pack (Name.toChars fieldName)
                     <> " : "
                     <> canonicalTypeToMultilineString self imports fieldType
               )
               (Can.fieldsToList fieldTypes)
               & T.intercalate "\\n       , "
           )
        <> "\\n       }"
    TUnit ->
      "()"
    TTuple t1 t2 mt3 ->
      "( "
        <> canonicalTypeToMultilineString self imports t1
        <> "\\n    , "
        <> canonicalTypeToMultilineString self imports t1
        <> ( case mt3 of
               Nothing -> ""
               Just t3 ->
                 "\\n    , " <> canonicalTypeToMultilineString self imports t3
           )
        <> "\\n    )"
    TAlias mod name namedParamTypes alias ->
      qualifier self imports mod name
        <> T.pack (Name.toChars name)
        <> ( case namedParamTypes of
               [] -> ""
               _ ->
                 let paramTypes = map snd namedParamTypes
                     typeString = T.intercalate " " (map (canonicalTypeToString self imports) paramTypes)
                  in if singleName paramTypes
                       then " " <> typeString
                       else
                         " ("
                           <> typeString
                           <> ")"
           )

-- |
callgraph :: FilePath -> FilePath -> Name.Name -> IO Json.Encode.Value
callgraph root file expressionName = do
  do
    Llamadera.debug_ "Getting artifacts..."

    eitherArtifacts <- Llamadera.loadSingleArtifacts root file
    case eitherArtifacts of
      Right (Compile.Artifacts mod annotations (Opt.LocalGraph main graph fields)) -> do
        let moduleName =
              case mod of
                Can.Module modName _ _ _ _ _ _ _ ->
                  modName

        case graph & Map.lookup (Opt.Global moduleName expressionName) of
          Just annotation -> do
            pure (graphToJson graph (Opt.Global moduleName expressionName))
          Nothing ->
            pure (Json.Encode.string (Json.String.fromChars "Not found"))
      Left err ->
        -- @TODO can we use Elm's error JSON formatter here?
        pure $ jsonError err

-- |
printCallGraph :: FilePath -> FilePath -> Name.Name -> IO ()
printCallGraph root file expressionName = do
  do
    Llamadera.debug_ "Getting artifacts..."

    eitherArtifacts <- Llamadera.loadSingleArtifacts root file
    case eitherArtifacts of
      Right (Compile.Artifacts mod annotations (Opt.LocalGraph main graph fields)) -> do
        let moduleName =
              case mod of
                Can.Module modName _ _ _ _ _ _ _ ->
                  modName

        let json = Json.Encode.encode (graphToJson graph (Opt.Global moduleName expressionName))
        case graph & Map.lookup (Opt.Global moduleName expressionName) of
          Just annotation -> do
            --  Llamadera.formatHaskellValue ("local GRAPH") graph
            B.hPutBuilder stderr json
          Nothing ->
            putStrLn "Not found!"
      Left err ->
        putStrLn $ show err

  pure ()

-- |
--
-- data Node
--   = Define Expr (Set.Set Global)
--   | DefineTailFunc [Name] Expr (Set.Set Global)
--   | Ctor Index.ZeroBased Int
--   | Enum Index.ZeroBased
--   | Box
--   | Link Global
--   | Cycle [Name] [(Name, Expr)] [Def] (Set.Set Global)
--   | Manager EffectsType
--   | Kernel [K.Chunk] (Set.Set Global)
--   | PortIncoming Expr (Set.Set Global)
--   | PortOutgoing Expr (Set.Set Global)
--
--
--
--
-- data Expr
--   = Bool Bool
--   | Chr ES.String
--   | Str ES.String
--   | Int Int
--   | Float EF.Float
--   | VarLocal Name
--   | VarGlobal Global
--   | VarEnum Global Index.ZeroBased
--   | VarBox Global
--   | VarCycle ModuleName.Canonical Name
--   | VarDebug Name ModuleName.Canonical A.Region (Maybe Name)
--   | VarKernel Name Name
--   | List [Expr]
--   | Function [Name] Expr
--   | Call Expr [Expr]
--   | TailCall Name [(Name, Expr)]
--   | If [(Expr, Expr)] Expr
--   | Let Def Expr
--   | Destruct Destructor Expr
--   | Case Name Name (Decider Choice) [(Int, Expr)]
--   | Accessor Name
--   | Access Expr Name
--   | Update Expr (Map.Map Name Expr)
--   | Record (Map.Map Name Expr)
--   | Unit
--   | Tuple Expr Expr (Maybe Expr)
--   | Shader Shader.Source (Set.Set Name) (Set.Set Name)
graphToJson :: Map.Map Opt.Global Opt.Node -> Opt.Global -> Json.Encode.Value
graphToJson graph entry =
  let (_, jsonListOfCalls) = getAllGraphCallsAsJson Set.empty graph entry
   in Json.Encode.list (\a -> a) jsonListOfCalls

tagged :: Opt.Global -> Json.Encode.Value -> Json.Encode.Value
tagged name val =
  Json.Encode.object
    [ "name" ==> globalNameToJson name,
      "body" ==> val
    ]

getAllGraphCallsAsJson :: Set.Set Opt.Global -> Map.Map Opt.Global Opt.Node -> Opt.Global -> (Set.Set Opt.Global, [Json.Encode.Value])
getAllGraphCallsAsJson alreadyCalled graph call =
  if Set.member call alreadyCalled
    then (alreadyCalled, [])
    else case graph & Map.lookup call of
      Just entryAnnotation -> do
        case entryAnnotation of
          Opt.Define expression _ ->
            let calls = getCalls expression

                (everyThingCalled, callsAsJson) =
                  List.foldl'
                    ( \(newAlreadyCalled, foundCalls) innerCall ->
                        let (subAlreadyCalled, innerCallJson) =
                              getAllGraphCallsAsJson newAlreadyCalled graph innerCall
                         in ( subAlreadyCalled,
                              foundCalls <> innerCallJson
                            )
                    )
                    ( Set.insert call alreadyCalled,
                      []
                    )
                    calls
             in ( everyThingCalled,
                  ( expressionToJson expression
                      & tagged call
                  ) :
                  callsAsJson
                )
          Opt.DefineTailFunc names expr _ ->
            ( alreadyCalled,
              [ Json.Encode.string (Json.String.fromChars "DEFINE TAIL CALL")
                  & tagged call
              ]
            )
          Opt.Enum index ->
            ( alreadyCalled,
              [ Json.Encode.string (Json.String.fromChars "Enum")
                  & tagged call
              ]
            )
          Opt.Ctor index i ->
            ( alreadyCalled,
              [ Json.Encode.string (Json.String.fromChars "Constructor")
                  & tagged call
              ]
            )
          Opt.Box ->
            ( alreadyCalled,
              [ Json.Encode.string (Json.String.fromChars "Box")
                  & tagged call
              ]
            )
          Opt.Link glob ->
            ( alreadyCalled,
              [ Json.Encode.string (Json.String.fromChars "Link")
                  & tagged call
              ]
            )
          Opt.Cycle names nameExprs defs _ ->
            ( alreadyCalled,
              [ Json.Encode.string (Json.String.fromChars "Cycle")
                  & tagged call
              ]
            )
          Opt.Manager effectsType ->
            ( alreadyCalled,
              [ Json.Encode.string (Json.String.fromChars "Effect Manager")
                  & tagged call
              ]
            )
          Opt.Kernel chunks _ ->
            ( alreadyCalled,
              [ Json.Encode.string (Json.String.fromChars "Kernel")
                  & tagged call
              ]
            )
          Opt.PortIncoming exp _ ->
            ( alreadyCalled,
              [ Json.Encode.string (Json.String.fromChars "Port Incoming")
                  & tagged call
              ]
            )
          Opt.PortOutgoing exp _ ->
            ( alreadyCalled,
              [ Json.Encode.string (Json.String.fromChars "Port Outgoing")
                  & tagged call
              ]
            )
      Nothing ->
        -- This is either:
        --  1. something that the user supplied and we couldn't find it (only top case)
        --  2. a value in a separate module
        -- For #1 we want to report to the user that we didn't find it.
        -- #2 is fine.  If they want that callgraph they can ask for it separately.
        --     Maybe at some point there can be a "noReallyGetMeEverything" version of the fn.
        --     Not sure it's necessary though.
        ( alreadyCalled,
          []
        )

-- Json.Encode.object
--     [ ( Json.String.fromChars "NOTFOUND"
--       , globalNameToJson call
--       )
--     ]

getCalls :: Opt.Expr -> [Opt.Global]
getCalls expr =
  case expr of
    Opt.Bool b ->
      []
    Opt.Chr str ->
      []
    Opt.Str str ->
      []
    Opt.Int i ->
      []
    Opt.Float elmFloat ->
      []
    Opt.VarLocal varName ->
      []
    Opt.VarGlobal global ->
      [global]
    Opt.VarEnum global index ->
      [global]
    Opt.VarBox global ->
      [global]
    Opt.VarCycle mod name ->
      []
    Opt.VarDebug name mod region maybeName ->
      []
    Opt.VarKernel name nameTwo ->
      []
    Opt.List expList ->
      List.concatMap (getCalls) expList
    Opt.Function nameList funcExpression ->
      getCalls funcExpression
    Opt.Call call listExps ->
      getCalls call <> List.concatMap getCalls listExps
    Opt.TailCall name listNameExpr ->
      List.concatMap
        ( \(name, expr) ->
            getCalls expr
        )
        listNameExpr
    Opt.If pairs elseExpr ->
      List.concatMap
        ( \(one, two) ->
            getCalls one <> getCalls two
        )
        pairs
        <> getCalls elseExpr
    Opt.Let def expr ->
      getCalls expr
    Opt.Destruct destructor exp ->
      getCalls exp
    Opt.Case one two decider listIntExprs ->
      getDeciderCalls decider
        <> List.concatMap
          ( \(_, expr) ->
              getCalls expr
          )
          listIntExprs
    Opt.Accessor name ->
      []
    Opt.Access accessExpr accessName ->
      getCalls accessExpr
    Opt.Update expression mapNameExpr ->
      getCalls expression
        <> Map.foldr
          ( \expr gathered ->
              getCalls expr <> gathered
          )
          []
          mapNameExpr
    Opt.Record mapNameExpr ->
      Map.foldr
        ( \expr gathered ->
            getCalls expr <> gathered
        )
        []
        mapNameExpr
    Opt.Unit ->
      []
    Opt.Tuple one two maybeThree ->
      getCalls one
        <> getCalls two
        <> ( case maybeThree of
               Nothing ->
                 []
               Just three ->
                 getCalls three
           )
    Opt.Shader _ _ _ ->
      []

getDeciderCalls :: Opt.Decider Opt.Choice -> [Opt.Global]
getDeciderCalls decider =
  case decider of
    Opt.Leaf (Opt.Inline expr) ->
      getCalls expr
    Opt.Leaf (Opt.Jump i) ->
      -- I assume we don't need to do anything here?
      []
    Opt.Chain test success failure ->
      getDeciderCalls success <> getDeciderCalls failure
    Opt.FanOut path tests fallback ->
      List.concatMap
        ( \(test, decide) ->
            getDeciderCalls decide
        )
        tests
        <> getDeciderCalls fallback

expressionToJson :: Opt.Expr -> Json.Encode.Value
expressionToJson expr =
  case expr of
    Opt.Bool b ->
      Json.Encode.bool b
    Opt.Chr str ->
      Json.Encode.object
        [ ( Json.String.fromChars "char",
            Json.Encode.string (Json.String.fromChars (Elm.String.toChars str))
          )
        ]
    Opt.Str str ->
      Json.Encode.object
        [ ( Json.String.fromChars "string",
            Json.Encode.string (Json.String.fromChars (Elm.String.toChars str))
          )
        ]
    Opt.Int i ->
      Json.Encode.object
        [ ( Json.String.fromChars "int",
            Json.Encode.int i
          )
        ]
    Opt.Float elmFloat ->
      Json.Encode.object
        [ ( Json.String.fromChars "float",
            Json.Encode.string (Json.String.fromChars ("AW GAWD HOW DO WE DO FLOATS"))
          )
        ]
    Opt.VarLocal varName ->
      Json.Encode.object
        [ ( Json.String.fromChars "var",
            nameToJsonString varName
          )
        ]
    Opt.VarGlobal global ->
      globalNameToJson global
    Opt.VarEnum global index ->
      globalNameToJson global
    Opt.VarBox global ->
      globalNameToJson global
    Opt.VarCycle mod name ->
      Json.Encode.object
        [ ( Json.String.fromChars "cycle",
            nameToJsonString name
          )
        ]
    Opt.VarDebug name mod region maybeName ->
      Json.Encode.string (Json.String.fromChars "Debug!")
    Opt.VarKernel one two ->
      Json.Encode.object
        [ ( Json.String.fromChars "kernelOne",
            nameToJsonString one
          ),
          ( Json.String.fromChars "kernelTwo",
            nameToJsonString two
          )
        ]
    Opt.List expList ->
      Json.Encode.list (expressionToJson) expList
    Opt.Function nameList funcExpression ->
      Json.Encode.object
        [ ( Json.String.fromChars "type",
            Json.Encode.string (Json.String.fromChars "fn")
          ),
          ( Json.String.fromChars "args",
            Json.Encode.list nameToJsonString nameList
          ),
          ( Json.String.fromChars "body",
            expressionToJson funcExpression
          )
        ]
    Opt.Call call listExps ->
      Json.Encode.object
        [ ( Json.String.fromChars "call",
            expressionToJson call
          ),
          ( Json.String.fromChars "with",
            Json.Encode.list (expressionToJson) listExps
          )
        ]
    Opt.TailCall name listNameExps ->
      Json.Encode.object
        [ ( Json.String.fromChars "tailcall",
            nameToJsonString name
          ),
          ( Json.String.fromChars "with",
            Json.Encode.list (\(name, exp) -> expressionToJson exp) listNameExps
          )
        ]
    Opt.If pairs elseExpr ->
      Json.Encode.object
        [ ( Json.String.fromChars "if",
            Json.Encode.list
              ( \(cond, result) ->
                  Json.Encode.object
                    [ ( Json.String.fromChars "condition",
                        expressionToJson cond
                      ),
                      ( Json.String.fromChars "then",
                        expressionToJson result
                      )
                    ]
              )
              pairs
          ),
          ( Json.String.fromChars "else",
            expressionToJson elseExpr
          )
        ]
    Opt.Let (Opt.Def name defExpr) expr ->
      Json.Encode.object
        [ ( Json.String.fromChars "let",
            expressionToJson defExpr
          ),
          ( Json.String.fromChars "in",
            expressionToJson expr
          )
        ]
    Opt.Let (Opt.TailDef name listName tailExpr) expr ->
      Json.Encode.object
        [ ( Json.String.fromChars "let",
            expressionToJson tailExpr
          ),
          ( Json.String.fromChars "names",
            Json.Encode.list (nameToJsonString) listName
          ),
          ( Json.String.fromChars "tail",
            (expressionToJson) expr
          )
        ]
    Opt.Destruct destruct expr ->
      Json.Encode.object
        [ ( Json.String.fromChars "destruct",
            (expressionToJson) expr
          )
        ]
    Opt.Case one two decider _ ->
      Json.Encode.object
        [ ( Json.String.fromChars "case",
            nameToJsonString one
          ),
          ( Json.String.fromChars "two",
            nameToJsonString two
          ),
          ( Json.String.fromChars "body",
            deciderToJson decider
          )
        ]
    Opt.Accessor name ->
      nameToJsonString name
    Opt.Access accessExpr accessName ->
      Json.Encode.object
        [ ( Json.String.fromChars "access",
            nameToJsonString accessName
          ),
          ( Json.String.fromChars "exp",
            expressionToJson accessExpr
          )
        ]
    Opt.Update exp fields ->
      Json.Encode.object
        [ ( Json.String.fromChars "record",
            expressionToJson exp
          ),
          ( Json.String.fromChars "fields",
            Map.foldlWithKey
              ( \gathered name val ->
                  Json.Encode.object
                    [ ( Json.String.fromChars "name",
                        nameToJsonString name
                      ),
                      ( Json.String.fromChars "value",
                        expressionToJson val
                      )
                    ] :
                  gathered
              )
              []
              fields
              & Json.Encode.list (\a -> a)
          )
        ]
    Opt.Record fields ->
      Json.Encode.object
        [ ( Json.String.fromChars "record",
            Map.foldlWithKey
              ( \gathered name val ->
                  Json.Encode.object
                    [ ( Json.String.fromChars "name",
                        nameToJsonString name
                      ),
                      ( Json.String.fromChars "value",
                        expressionToJson val
                      )
                    ] :
                  gathered
              )
              []
              fields
              & Json.Encode.list (\a -> a)
          )
        ]
    Opt.Unit ->
      Json.Encode.object []
    Opt.Tuple one two maybeThree ->
      Json.Encode.object
        ( Maybe.catMaybes
            [ Just
                ( Json.String.fromChars "0",
                  expressionToJson one
                ),
              Just
                ( Json.String.fromChars "1",
                  expressionToJson one
                ),
              fmap
                ( \three ->
                    ( Json.String.fromChars "2",
                      expressionToJson three
                    )
                )
                maybeThree
            ]
        )
    Opt.Shader source names names2 ->
      Json.Encode.string (Json.String.fromChars "Shader")

deciderToJson decider =
  case decider of
    Opt.Leaf (Opt.Inline expr) ->
      expressionToJson expr
    Opt.Leaf (Opt.Jump i) ->
      Json.Encode.string (Json.String.fromChars "Jump!")
    Opt.Chain test success failure ->
      Json.Encode.string (Json.String.fromChars "Chain")
    Opt.FanOut path tests fallback ->
      Json.Encode.string (Json.String.fromChars "Fanout")

nameToJsonString :: Name.Name -> Json.Encode.Value
nameToJsonString name =
  Json.Encode.string (Json.String.fromChars (Name.toChars name))

globalNameToJson :: Opt.Global -> Json.Encode.Value
globalNameToJson (Opt.Global (Elm.ModuleName.Canonical pkg mdl) name) =
  Json.Encode.string (Json.String.fromChars (Name.toChars mdl <> "." <> Name.toChars name))

jsonError :: Show a => a -> Json.Encode.Value
jsonError err =
  Json.Encode.object
    [ "error" ==> Json.Encode.string (Json.String.fromChars ("Error: " ++ show err))
    ]

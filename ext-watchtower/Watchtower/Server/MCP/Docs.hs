{-# LANGUAGE OverloadedStrings #-}

module Watchtower.Server.MCP.Docs
  ( renderModule
  , renderValue
  , renderPackage
  , toMarkdown
  , ModuleMeta(..)
  , ValueMeta(..)
  , PackageMeta(..)
  ) where

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)
import qualified Data.Name as Name
import qualified Elm.Docs as Docs
import qualified Elm.Compiler.Type as ElmType
import qualified Reporting.Doc as RDoc
import qualified Reporting.Render.Type as RenderType
import qualified Reporting.Render.Type.Localizer as Localizer
import qualified Json.String

-- Render a simple YAML doc from key/value pairs (strings only)
renderYamlLines :: [(Text, Text)] -> Text
renderYamlLines pairs =
  let quoteIfNeeded t =
        if Text.any (\c -> c == ':' || c == '"' || c == '\n') t || Text.null t
          then Text.concat ["\"", Text.replace "\"" "\\\"" t, "\""]
          else t
      render (k,v) = Text.concat [k, ": ", quoteIfNeeded v]
  in Text.intercalate "\n" (map render pairs)

-- | Render a single markdown document with YAML frontmatter.
markdownDoc :: Text -> Text -> Text
markdownDoc metaYaml body =
  Text.concat [
    "---\n",
    metaYaml,
    "\n---\n\n",
    body,
    if Text.isSuffixOf "\n" body then "" else "\n"
  ]

markdownDocWithToc :: Text -> Text -> Text
markdownDocWithToc metaYaml body =
  let tocEntries = extractTocEntries body
      tocYaml = renderTocYaml tocEntries
      fullMeta =
        if Text.null tocYaml
          then metaYaml
          else Text.concat [metaYaml, "\n", tocYaml]
  in markdownDoc fullMeta body

data ModuleMeta = ModuleMeta
  { moduleName :: Text
  , moduleProjectRoot :: Maybe FilePath
  , moduleFilePath :: Maybe FilePath
  , moduleFromPackage :: Maybe Text
  }

data ValueMeta = ValueMeta
  { valueModuleName :: Text
  , valueName :: Text
  , valueProjectRoot :: Maybe FilePath
  , valueFilePath :: Maybe FilePath
  , valueFromPackage :: Maybe Text
  }

data PackageMeta = PackageMeta
  { packageFromPackage :: Text
  , packageProjectRoot :: Maybe FilePath
  }

-- Public meta renderers (no inference from Docs.Module)
renderModuleMeta :: ModuleMeta -> Text
renderModuleMeta meta =
  let pairs =
        maybe [] (\p -> [("package", p)]) (moduleFromPackage meta)
  in renderYamlLines pairs

renderValueMeta :: ValueMeta -> Text
renderValueMeta meta =
  let pairs =
        [ ("module_name", valueModuleName meta)
        , ("value_name", valueName meta)
        ]
        ++ maybe [] (\p -> [("project_root", Text.pack p)]) (valueProjectRoot meta)
        ++ maybe [] (\p -> [("path", Text.pack p)]) (valueFilePath meta)
        ++ maybe [] (\p -> [("package", p)]) (valueFromPackage meta)
  in renderYamlLines pairs

renderPackageMeta :: PackageMeta -> Text
renderPackageMeta meta =
  let pairs =
        [ ("package", packageFromPackage meta)
        ]
        ++ maybe [] (\p -> [("project_root", Text.pack p)]) (packageProjectRoot meta)
  in renderYamlLines pairs


renderModule :: ModuleMeta -> Docs.Module -> Text
renderModule meta modu =
  let body = normalizeSpacing (toMarkdown modu)
  in markdownDocWithToc (renderModuleMeta meta) body

-- public: render a single value from a module as a complete markdown doc
renderValue :: ValueMeta -> Docs.Module -> Name.Name -> Text
renderValue meta (Docs.Module _ _ unions aliases values binops) valueNm =
  let nm = Text.pack (Name.toChars valueNm)

      body = case Map.lookup valueNm values of
        Just v  -> elmBlock (renderValueElmDecl nm v)
        Nothing -> case Map.lookup valueNm aliases of
          Just a  -> elmBlock (renderAliasElmDecl nm a)
          Nothing -> case Map.lookup valueNm unions of
            Just u  -> elmBlock (renderUnionElmDecl nm u)
            Nothing -> case Map.lookup valueNm binops of
              Just b  -> elmBlock (renderBinopElmDecl nm b)
              Nothing -> Text.concat ["## ", nm, "\n\n_(not found in docs)_\n"]
  in markdownDoc (renderValueMeta meta) body

-- public: render a package (readme + modules) as a single markdown doc
renderPackage :: PackageMeta -> Maybe Text -> [Docs.Module] -> Text
renderPackage pkgMeta mReadme modules =
  let modulesSection =
        if null modules then ""
        else
          let items = map (\(Docs.Module modName _ _ _ _ _) ->
                          let modTxt = Text.pack (Name.toChars modName)
                          in Text.concat ["- [", modTxt, "](elm://docs/module/", modTxt, ")"]
                        ) modules
          in Text.intercalate "\n" ("## Modules" : "" : items)

      readmeSection = maybe "" (\t -> Text.concat ["# Readme\n\n", t]) mReadme
      body = Text.intercalate "\n\n" (filter (not . Text.null) [readmeSection, modulesSection])
  in markdownDoc (renderPackageMeta pkgMeta) body



-- Convert an Elm Docs.Module into a markdown body by expanding @docs references
toMarkdown :: Docs.Module -> Text
toMarkdown (Docs.Module modName modComment unions aliases values binops) =
  let commentText = Text.pack (Json.String.toChars modComment)
      -- Json.String is already JSON-escaped, so newlines are represented as the two characters \ and n
      -- Split on the literal "\\n" sequence rather than actual newline characters
      lines0 = Text.splitOn "\\n" commentText
      moduleHeader = Text.concat ["# ", Text.pack (Name.toChars modName)]

      renderNamed nm =
        let nmTxt = Text.pack (Name.toChars nm) in
        case Map.lookup nm values of
          Just v  -> Just (renderValueElmDecl nmTxt v)
          Nothing -> case Map.lookup nm aliases of
            Just a  -> Just (renderAliasElmDecl nmTxt a)
            Nothing -> case Map.lookup nm unions of
              Just u  -> Just (renderUnionElmDecl nmTxt u)
              Nothing -> case Map.lookup nm binops of
                Just b  -> Just (renderBinopElmDecl nmTxt b)
                Nothing -> Nothing

      step (accText, seen) line =
        let stripped = Text.stripStart line in
        if Text.isPrefixOf "@docs" stripped then
          let names = parseDocsRefs stripped
              renderedDecls = mapMaybe renderNamed names
              newlySeen = foldl (\s n -> Set.insert n s) seen names
              chunk =
                if null renderedDecls
                  then ""
                  else elmBlock (Text.intercalate "\n\n" renderedDecls)
              acc' = if Text.null accText then chunk else Text.intercalate "\n" (filter (not . Text.null) [accText, chunk])
          in (acc', newlySeen)
        else
          let lineAdjusted =
                if Text.isPrefixOf "#" stripped
                  then Text.concat ["#", stripped]
                  else line
              acc' = if Text.null accText then lineAdjusted else Text.concat [accText, "\n", lineAdjusted]
          in (acc', seen)

      (bodyPrefix, seenRefs) = foldl step (Text.empty, Set.empty) lines0

      trailingUnions =
        Map.foldrWithKey
          (\n u acc -> if Set.member n seenRefs then acc else renderUnionElmDecl (Text.pack (Name.toChars n)) u : acc)
          []
          unions

      trailingAliases =
        Map.foldrWithKey
          (\n a acc -> if Set.member n seenRefs then acc else renderAliasElmDecl (Text.pack (Name.toChars n)) a : acc)
          []
          aliases

      trailingValues =
        Map.foldrWithKey
          (\n v acc -> if Set.member n seenRefs then acc else renderValueElmDecl (Text.pack (Name.toChars n)) v : acc)
          []
          values

      trailingBinops =
        Map.foldrWithKey
          (\n b acc -> if Set.member n seenRefs then acc else renderBinopElmDecl (Text.pack (Name.toChars n)) b : acc)
          []
          binops

      trailingDecls = trailingUnions ++ trailingAliases ++ trailingValues ++ trailingBinops
      trailingBlock =
        if null trailingDecls
          then ""
          else elmBlock (Text.intercalate "\n\n" trailingDecls)

      body =
        if Text.null trailingBlock
          then Text.intercalate "\n\n" (filter (not . Text.null) [moduleHeader, bodyPrefix])
          else Text.intercalate "\n\n" (filter (not . Text.null) [moduleHeader, bodyPrefix, trailingBlock])
       
  in body

-- Parse an @docs line to a list of names
parseDocsRefs :: Text -> [Name.Name]
parseDocsRefs line =
  let rest = Text.drop 5 line -- drop "@docs"
      parts = Text.splitOn "," rest
      trim = Text.strip
  in [ Name.fromChars (Text.unpack t) | t <- map trim parts, not (Text.null t) ]

-- Render helpers
typeToText :: ElmType.Type -> Text
typeToText tipe =
  let normalized = normalizeType tipe
  in Text.pack (RDoc.toLine (ElmType.toDoc defaultLocalizer RenderType.None normalized))

-- Remove default module qualifiers like "Basics.Int" -> "Int", "String.String" -> "String"
normalizeType :: ElmType.Type -> ElmType.Type
normalizeType tipe =
  case tipe of
    ElmType.Lambda a b -> ElmType.Lambda (normalizeType a) (normalizeType b)
    ElmType.Var n -> ElmType.Var n
    ElmType.Type n args -> ElmType.Type (stripDefaultPrefix n) (map normalizeType args)
    ElmType.Record fields ext -> ElmType.Record (map (\(n,t) -> (n, normalizeType t)) fields) ext
    ElmType.Unit -> ElmType.Unit
    ElmType.Tuple a b cs -> ElmType.Tuple (normalizeType a) (normalizeType b) (map normalizeType cs)

stripDefaultPrefix :: Name.Name -> Name.Name
stripDefaultPrefix n =
  let s = Name.toChars n
      (modPart, rest) = break (== '.') s
  in case rest of
      [] -> n
      (_:typeName) ->
        if modPart `elem` defaultModules then Name.fromChars typeName else n

defaultModules :: [String]
defaultModules = ["Basics","String","List","Maybe","Result","Char","Tuple"]

-- Use a localizer that treats Elm's default imports as unqualified
defaultLocalizer :: Localizer.Localizer
defaultLocalizer =
  Localizer.fromNames $
    Map.fromList
      [ (Name.fromChars "Basics", ())
      , (Name.fromChars "String", ())
      , (Name.fromChars "List", ())
      , (Name.fromChars "Maybe", ())
      , (Name.fromChars "Result", ())
      , (Name.fromChars "Char", ())
      , (Name.fromChars "Tuple", ())
      ]

-- When a type appears as a constructor argument, certain forms need parentheses
needsParens :: ElmType.Type -> Bool
needsParens tipe =
  case tipe of
    ElmType.Lambda _ _ -> True
    _ -> False

parentTypeToText :: ElmType.Type -> Text
parentTypeToText tipe =
  let inner = typeToText tipe
  in if needsParens tipe then Text.concat ["(", inner, ")"] else inner

-- Convert a JSON-escaped comment to displayable Text
-- 1) Trim leading/trailing whitespace
-- 2) Replace literal "\\n" sequences with actual newlines
commentToText :: Json.String.String -> Text
commentToText s =
  let raw = Text.pack (Json.String.toChars s)
  in Text.replace "\\n" "\n" (Text.strip raw)

fenceTicks :: Text
fenceTicks = "````"

elmBlock :: Text -> Text
elmBlock body =
  Text.intercalate "\n"
    [ Text.concat [fenceTicks, "elm"]
    , body
    , fenceTicks
    ]

renderCommentInElm :: Json.String.String -> Text
renderCommentInElm c =
  let doc = commentToText c
  in if Text.null doc then
       ""
     else
       Text.intercalate "\n"
         [ "{-|"
         , doc
         , "-}"
         ]

renderValueElmDecl :: Text -> Docs.Value -> Text
renderValueElmDecl nm (Docs.Value comment tipe) =
  let declaration = Text.concat [ nm, " : ", typeToText tipe]
      commentBlock = renderCommentInElm comment
  in if Text.null commentBlock then declaration else Text.concat [commentBlock, "\n", declaration]

renderAliasElmDecl :: Text -> Docs.Alias -> Text
renderAliasElmDecl nm (Docs.Alias comment args tipe) =
  let argsText = if null args then Text.empty else Text.concat [" ", Text.intercalate " " (map (Text.pack . Name.toChars) args)]
      declaration = Text.concat ["type alias ", nm, argsText, " = ", typeToText tipe]
      commentBlock = renderCommentInElm comment
  in if Text.null commentBlock then declaration else Text.concat [commentBlock, "\n", declaration]
  

renderUnionElmDecl :: Text -> Docs.Union -> Text
renderUnionElmDecl nm (Docs.Union comment args ctors) =
  let argsText = if null args then Text.empty else Text.concat [" ", Text.intercalate " " (map (Text.pack . Name.toChars) args)]
      ctorFirstLine (ctorName, ctorArgs) =
        let ctorNm = Text.pack (Name.toChars ctorName)
            argsDoc = case ctorArgs of
              [] -> Text.empty
              _  -> Text.concat [" ", Text.intercalate " " (map parentTypeToText ctorArgs)]
        in Text.concat ["    = ", ctorNm, argsDoc]
      ctorRestLine (ctorName, ctorArgs) =
        let ctorNm = Text.pack (Name.toChars ctorName)
            argsDoc = case ctorArgs of
              [] -> Text.empty
              _  -> Text.concat [" ", Text.intercalate " " (map parentTypeToText ctorArgs)]
        in Text.concat ["    | ", ctorNm, argsDoc]
      ctorsBlock = case ctors of
        [] -> "    = -- no constructors"
        (first:rest) ->
          let firstLine = ctorFirstLine first
          in Text.intercalate "\n" (firstLine : map ctorRestLine rest)
      declaration = Text.intercalate "\n"
        [ Text.concat ["type ", nm, argsText]
        , ctorsBlock
        ]
      commentBlock = renderCommentInElm comment
  in if Text.null commentBlock then declaration else Text.concat [commentBlock, "\n", declaration]
    

renderBinopElmDecl :: Text -> Docs.Binop -> Text
renderBinopElmDecl nm (Docs.Binop comment tipe _ _) =
  let declaration = Text.concat [ nm, " : ", typeToText tipe]
      commentBlock = renderCommentInElm comment
  in if Text.null commentBlock then declaration else Text.concat [commentBlock, "\n", declaration]


extractTocEntries :: Text -> [Text]
extractTocEntries body =
  let step (inFence, acc) line =
        let stripped = Text.strip line
            isFence = Text.isPrefixOf "```" stripped
        in if isFence then
             (not inFence, acc)
           else if inFence then
             (inFence, acc)
           else if Text.isPrefixOf "#" stripped then
             let noHashes = Text.dropWhile (== '#') stripped
                 cleaned = Text.strip noHashes
             in if Text.null cleaned
                  then (inFence, acc)
                  else (inFence, cleaned : acc)
           else
             (inFence, acc)
      (_, entriesReversed) = foldl step (False, []) (Text.lines body)
      allEntries = List.nub (reverse entriesReversed)
  in case allEntries of
       [] -> []
       _moduleTitle : rest -> rest


renderTocYaml :: [Text] -> Text
renderTocYaml entries =
  case entries of
    [] -> ""
    _ ->
      Text.intercalate "\n" ("toc:" : map (\entry -> Text.concat ["  - ", yamlQuote entry]) entries)


yamlQuote :: Text -> Text
yamlQuote t =
  Text.concat ["\"", Text.replace "\"" "\\\"" t, "\""]


normalizeSpacing :: Text -> Text
normalizeSpacing txt =
  let lines0 = Text.lines txt
      step (acc, blankCount) line =
        let isBlank = Text.null (Text.strip line)
        in if isBlank then
             if blankCount >= 1 then (acc, blankCount + 1) else (acc ++ [""], 1)
           else
             (acc ++ [line], 0)
      (collapsed, _) = foldl step ([], 0) lines0
  in Text.stripEnd (Text.unlines collapsed)
  

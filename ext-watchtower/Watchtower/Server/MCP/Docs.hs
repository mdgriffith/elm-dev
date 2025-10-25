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
import qualified Data.Map as Map
import qualified Data.Set as Set
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
        [ ("module_name", moduleName meta)
        ]
        ++ maybe [] (\p -> [("project_root", Text.pack p)]) (moduleProjectRoot meta)
        ++ maybe [] (\p -> [("file_path", Text.pack p)]) (moduleFilePath meta)
        ++ maybe [] (\p -> [("from_package", p)]) (moduleFromPackage meta)
  in renderYamlLines pairs

renderValueMeta :: ValueMeta -> Text
renderValueMeta meta =
  let pairs =
        [ ("module_name", valueModuleName meta)
        , ("value_name", valueName meta)
        ]
        ++ maybe [] (\p -> [("project_root", Text.pack p)]) (valueProjectRoot meta)
        ++ maybe [] (\p -> [("file_path", Text.pack p)]) (valueFilePath meta)
        ++ maybe [] (\p -> [("from_package", p)]) (valueFromPackage meta)
  in renderYamlLines pairs

renderPackageMeta :: PackageMeta -> Text
renderPackageMeta meta =
  let pairs =
        [ ("from_package", packageFromPackage meta)
        ]
        ++ maybe [] (\p -> [("project_root", Text.pack p)]) (packageProjectRoot meta)
  in renderYamlLines pairs


renderModule :: ModuleMeta -> Docs.Module -> Text
renderModule meta modu =
  let body = toMarkdown modu
  in markdownDoc (renderModuleMeta meta) body

-- public: render a single value from a module as a complete markdown doc
renderValue :: ValueMeta -> Docs.Module -> Name.Name -> Text
renderValue meta (Docs.Module _ _ unions aliases values binops) valueNm =
  let nm = Text.pack (Name.toChars valueNm)

      body = case Map.lookup valueNm values of
        Just v  -> renderValueBlock nm v
        Nothing -> case Map.lookup valueNm aliases of
          Just a  -> renderAliasBlock nm a
          Nothing -> case Map.lookup valueNm unions of
            Just u  -> renderUnionBlock nm u
            Nothing -> case Map.lookup valueNm binops of
              Just b  -> renderBinopBlock nm b
              Nothing -> Text.concat ["## ", nm, "\n\n_(not found in docs)_\n"]
  in markdownDoc (renderValueMeta meta) body

-- public: render a package (readme + modules) as a single markdown doc
renderPackage :: PackageMeta -> Maybe Text -> [Docs.Module] -> Text
renderPackage pkgMeta mReadme modules =
  let modulesSection =
        Text.intercalate "\n\n" (map (\m@(Docs.Module modName _ _ _ _ _) ->
          Text.concat ["## Module ", Text.pack (Name.toChars modName), "\n\n", toMarkdown m]
        ) modules)

      readmeSection = maybe "" (\t -> Text.concat ["# Readme\n\n", t]) mReadme
      body = Text.intercalate "\n\n" (filter (not . Text.null) [readmeSection, modulesSection])
  in markdownDoc (renderPackageMeta pkgMeta) body



-- Convert an Elm Docs.Module into a markdown body by expanding @docs references
toMarkdown :: Docs.Module -> Text
toMarkdown (Docs.Module modName modComment unions aliases values _binops) =
  let commentText = Text.pack (Json.String.toChars modComment)
      -- Json.String is already JSON-escaped, so newlines are represented as the two characters \ and n
      -- Split on the literal "\\n" sequence rather than actual newline characters
      lines0 = Text.splitOn "\\n" commentText

      step (accText, seen) line =
        if Text.isPrefixOf "@docs" line then
          let names = parseDocsRefs line
              (renderedBlocks, newlySeen) =
                foldr (\nm (bs, s) ->
                  let nmTxt = Text.pack (Name.toChars nm) in
                  case Map.lookup nm values of
                    Just v  -> (renderValueBlock nmTxt v : bs, Set.insert nm s)
                    Nothing -> case Map.lookup nm aliases of
                      Just a  -> (renderAliasBlock nmTxt a : bs, Set.insert nm s)
                      Nothing -> case Map.lookup nm unions of
                        Just u  -> (renderUnionBlock nmTxt u : bs, Set.insert nm s)
                        Nothing -> (bs, s)
                ) ([], seen) names
              chunk = Text.intercalate "\n" renderedBlocks
              acc' = if Text.null accText then chunk else Text.intercalate "\n" (filter (not . Text.null) [accText, chunk])
          in (acc', newlySeen)
        else
          let acc' = if Text.null accText then line else Text.concat [accText, "\n", line]
          in (acc', seen)

      (bodyPrefix, seenRefs) = foldl step (Text.empty, Set.empty) lines0

      trailingUnions =
        Map.foldrWithKey
          (\n u acc -> if Set.member n seenRefs then acc else renderUnionBlock (Text.pack (Name.toChars n)) u : acc)
          []
          unions

      trailingAliases =
        Map.foldrWithKey
          (\n a acc -> if Set.member n seenRefs then acc else renderAliasBlock (Text.pack (Name.toChars n)) a : acc)
          []
          aliases

      trailingValues =
        Map.foldrWithKey
          (\n v acc -> if Set.member n seenRefs then acc else renderValueBlock (Text.pack (Name.toChars n)) v : acc)
          []
          values

      trailingBlocks = trailingUnions ++ trailingAliases ++ trailingValues

      body =
        case trailingBlocks of
          [] -> bodyPrefix
          _  -> Text.intercalate "\n" (filter (not . Text.null) [bodyPrefix, Text.intercalate "\n" trailingBlocks])
      header = Text.concat ["# ", Text.pack (Name.toChars modName), "\n\n"]
  in Text.append header body

-- Parse an @docs line to a list of names
parseDocsRefs :: Text -> [Name.Name]
parseDocsRefs line =
  let rest = Text.drop 5 line -- drop "@docs"
      parts = Text.splitOn "," rest
      trim = Text.strip
  in [ Name.fromChars (Text.unpack t) | t <- map trim parts, not (Text.null t) ]

-- Render helpers
typeToText :: ElmType.Type -> Text
typeToText tipe = Text.pack (RDoc.toLine (ElmType.toDoc Localizer.empty RenderType.None tipe))

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

-- Render an Elm code block with an optional doc comment.
-- If the rendered comment text is empty, omit the doc comment.
elmBlock :: Json.String.String -> Text -> Text
elmBlock c body =
  let doc = commentToText c
  in if Text.null doc then
    Text.intercalate "\n"
      [ "```elm"
      , body
      , "```"
      ]
  else
    Text.intercalate "\n"
      [ "```elm"
      , Text.concat ["{-| ", doc, "-}"]
      , body
      , "```"
      ]

renderValueBlock :: Text -> Docs.Value -> Text
renderValueBlock nm (Docs.Value comment tipe) =
  elmBlock comment (Text.concat [ nm, " : ", typeToText tipe])

renderAliasBlock :: Text -> Docs.Alias -> Text
renderAliasBlock nm (Docs.Alias comment args tipe) =
  let argsText = if null args then Text.empty else Text.concat [" ", Text.intercalate " " (map (Text.pack . Name.toChars) args)]
      body = Text.concat ["type alias ", nm, argsText, " = ", typeToText tipe]
  in
  elmBlock comment body
  

renderUnionBlock :: Text -> Docs.Union -> Text
renderUnionBlock nm (Docs.Union comment args ctors) =
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
      body = Text.intercalate "\n"
        [ Text.concat ["type ", nm, argsText]
        , ctorsBlock
        ]
  in elmBlock comment body
    

renderBinopBlock :: Text -> Docs.Binop -> Text
renderBinopBlock nm (Docs.Binop comment tipe _ _) =
  elmBlock comment (Text.concat [ nm, " : ", typeToText tipe])
  

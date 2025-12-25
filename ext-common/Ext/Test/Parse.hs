{-# LANGUAGE OverloadedStrings #-}
module Ext.Test.Parse
  ( Exposed(..)
  , ParsedModule(..)
  , parseModule
  , potentialTestValues
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Name as Name
import qualified Data.Set as Set
import qualified Elm.ModuleName as ModuleName
import StandaloneInstances


data Exposed
  = ExposedEverything
  | ExposedExact [Name.Name]
  deriving (Show)


data ParsedModule = ParsedModule
  { pmModuleName :: ModuleName.Raw
  , pmExposed :: Exposed  -- Exposed values (from exposing clause)
  , pmTopLevelDecls :: Set.Set Name.Name  -- All lowercase top-level declarations
  }
  deriving (Show)


-- | Minimal parsing: extract module name, exposing clause, and all lowercase top-level declarations.
-- This is much faster than full compilation and doesn't require type checking.
parseModule :: FilePath -> BS.ByteString -> Either String ParsedModule
parseModule filePath content = do
  let src = UTF8.toString content
  moduleName <- parseModuleName src filePath
  exposed <- parseExposing src
  topLevelDecls <- parseTopLevelDeclarations src
  pure (ParsedModule moduleName exposed topLevelDecls)


-- | Extract module name from "module Module.Name exposing (...)"
-- Module names start with a capital letter and contain [A-Za-z0-9_.]
parseModuleName :: String -> FilePath -> Either String ModuleName.Raw
parseModuleName src filePath =
  case List.stripPrefix "module " (dropWhile Char.isSpace src) of
    Nothing -> Left ("Could not find module declaration in " ++ filePath)
    Just rest ->
      let -- Extract module name: starts with capital, then [A-Za-z0-9_.]
          isValidModuleChar c = Char.isAlphaNum c || c == '_' || c == '.'
          (namePart, _) = span isValidModuleChar rest
      in if null namePart || not (Char.isUpper (head namePart))
           then Left ("Invalid module declaration in " ++ filePath)
           else Right (Name.fromChars namePart)


-- | Extract exposed values from exposing clause.
-- Handles: exposing (..), exposing (a, b, c), or no exposing clause (defaults to all)
parseExposing :: String -> Either String Exposed
parseExposing src =
  let lowerSrc = map Char.toLower src
      -- Find "exposing" keyword
      findExposing = List.findIndex (List.isPrefixOf "exposing") (List.tails lowerSrc)
  in case findExposing of
    Nothing -> Right ExposedEverything -- No exposing clause means we'll check all top-level decls
    Just idx ->
      let afterExposing = drop (idx + 8) lowerSrc  -- "exposing" is 8 chars
          trimmed = dropWhile Char.isSpace afterExposing
      in case trimmed of
        '(':rest ->
          if take 2 rest == ".." then
            Right ExposedEverything -- exposing (..) means all
          else
            ExposedExact <$> parseExposedList rest
        _ -> Right ExposedEverything -- Malformed, default to checking all


-- | Parse a list of exposed names: "a, b, c)"
parseExposedList :: String -> Either String [Name.Name]
parseExposedList src =
  let names = List.takeWhile (/= ')') src
      splitNames = splitOn ',' names
      cleanNames = map (filter (\c -> not (Char.isSpace c || c == '('))) splitNames
      validNames = filter (not . null) cleanNames
  in Right (map Name.fromChars validNames)


-- | Split a string on a delimiter character
splitOn :: Char -> String -> [String]
splitOn delimiter = go []
  where
    go acc [] = reverse acc
    go acc (c:cs)
      | c == delimiter = go ([] : acc) cs
      | otherwise = case acc of
          [] -> go [[c]] cs
          (lastPart:rest) -> go ((lastPart ++ [c]):rest) cs


-- | Extract all lowercase top-level declarations.
-- Finds identifiers that start with a lowercase character at the beginning of a line
-- (after whitespace) and end with whitespace.
-- Tracks multiline comment state to skip `{- ... -}` blocks.
parseTopLevelDeclarations :: String -> Either String (Set.Set Name.Name)
parseTopLevelDeclarations src =
  let lines_ = lines src
      -- Extract declarations from each line, tracking multiline comment state
      -- State is (inMultilineComment, declarationsSet)
      (_, decls) = List.foldl processLine (False, Set.empty) lines_
  in Right decls
  where
    processLine :: (Bool, Set.Set Name.Name) -> String -> (Bool, Set.Set Name.Name)
    processLine (inComment, acc) line =
      let trimmed = dropWhile Char.isSpace line
          hasOpenComment = List.isInfixOf "{-" trimmed
          hasCloseComment = List.isInfixOf "-}" trimmed
          -- Determine if this line is a comment:
          -- - If we see `{-`, this line is a comment and we enter comment mode
          -- - If we're already in comment mode, this line is a comment
          -- - If we see `-}`, this line is a comment but we exit comment mode
          isCommentLine = hasOpenComment || inComment || hasCloseComment
          -- Update comment state: enter on `{-`, exit on `-}`
          newCommentState = if hasOpenComment then True
                           else if hasCloseComment then False
                           else inComment
          -- Only process the line if it's not a comment
          newAcc = if isCommentLine then acc
                  else extractTopLevelDeclarations acc line
      in (newCommentState, newAcc)


-- | Reserved words that should never be captured as top-level declarations
reservedWords :: Set.Set String
reservedWords = Set.fromList ["type", "import", "port", "module"]


-- | Extract top-level declaration names from a line.
-- Finds lowercase identifiers that start at the beginning of a line (after whitespace).
-- Identifiers must start with lowercase and contain only [a-zA-Z0-9].
-- Never captures reserved words.
-- Accumulates results into the provided Set.
extractTopLevelDeclarations :: Set.Set Name.Name -> String -> Set.Set Name.Name
extractTopLevelDeclarations acc line =
    let -- Find identifier: starts with lowercase, then [a-zA-Z0-9]*
        isValidIdentStart c = Char.isLower c
        isValidIdentChar c = Char.isAlphaNum c
        ident = takeWhile isValidIdentChar line
    in if null ident || not (isValidIdentStart (head ident))
        then acc -- Not a lowercase identifier
        else
            -- Check that it's not a reserved word
            if Set.member ident reservedWords
            then acc
            else Set.insert (Name.fromChars ident) acc


-- | Extract potential test values from a parsed module.
-- Returns a list of (moduleName, valueName) pairs for values that could be tests.
-- Uses exposed values if available, otherwise falls back to all top-level declarations
-- (which happens when exposing (..) or no exposing clause is present).
potentialTestValues :: ParsedModule -> [(ModuleName.Raw, Name.Name)]
potentialTestValues pm =
  let moduleName = pmModuleName pm
      exposed = pmExposed pm
      topLevelDecls = pmTopLevelDecls pm
      -- If ExposedEverything, use all top-level decls; otherwise use the exact list
      valuesToCheck = case exposed of
        ExposedEverything -> Set.toList topLevelDecls
        ExposedExact names -> names
  in map (\valName -> (moduleName, valName)) valuesToCheck


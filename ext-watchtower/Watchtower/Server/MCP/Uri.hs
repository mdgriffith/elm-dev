{-# LANGUAGE OverloadedStrings #-}
module Watchtower.Server.MCP.Uri (Pattern, pattern, var, s, PatternMatch(..), match, renderPattern, hasVariables) where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Text (Text)


-- Pattern types

data PathToken
  = Literal Text
  | Var Text
  deriving (Show, Eq)

data Pattern = Pattern
  { patternProtocol :: Text
  , patternTokens :: [PathToken]
  , patternAllowedQueryParams :: [Text]
  }
  deriving (Show, Eq)

data PatternMatch = PatternMatch
  { matchPathValues :: Map.Map Text Text
  , matchQueryParams :: Map.Map Text Text
  }
  deriving (Show, Eq)


-- Builders and renderers

pattern :: Text -> [PathToken] -> [Text] -> Pattern
pattern protocol tokens allowedParams =
  Pattern protocol tokens allowedParams

var :: Text -> PathToken
var = Var

s :: Text -> PathToken
s = Literal

renderPattern :: Pattern -> Text
renderPattern (Pattern protocol tokens _allowed) =
  let renderToken tk = case tk of
        Literal t -> t
        Var name -> Text.concat ["{", name, "}"]
  in Text.concat (protocol : "://" : fmap renderToken tokens)


-- URI helpers

splitUri :: Text -> (Text, [(Text, Text)])
splitUri uri =
  let (beforeQ, afterQ) = Text.breakOn "?" uri
  in if Text.null afterQ then (beforeQ, []) else (beforeQ, parseQueryParams (Text.drop 1 afterQ))

parseQueryParams :: Text -> [(Text, Text)]
parseQueryParams q =
  let pairs = if Text.null q then [] else Text.splitOn "&" q
  in map (\p -> case Text.breakOn "=" p of (k, v) -> (k, Text.drop 1 v)) pairs



-- Matching

match :: Pattern -> Text -> Maybe PatternMatch
match pat concrete =
  let (uriNoQuery, queryPairs) = splitUri concrete
      proto = patternProtocol pat
      prefix = Text.concat [proto, "://"]
  in if not (Text.isPrefixOf prefix uriNoQuery)
     then Nothing
     else let pathPortion = Text.drop (Text.length prefix) uriNoQuery
          in case matchPath (patternTokens pat) pathPortion Map.empty of
       Nothing -> Nothing
       Just pathValues ->
         let allowed = patternAllowedQueryParams pat
             queryMap = Map.fromList [ (k, v) | (k, v) <- queryPairs, k `elem` allowed ]
         in Just (PatternMatch pathValues queryMap)

matchPath :: [PathToken] -> Text -> Map.Map Text Text -> Maybe (Map.Map Text Text)
matchPath tokens input accum =
  case tokens of
    [] -> if Text.null input then Just accum else Nothing
    (Literal lit : rest) ->
      if Text.isPrefixOf lit input
        then matchPath rest (Text.drop (Text.length lit) input) accum
        else Nothing
    (Var name : rest) ->
      case nextLiteral rest of
        Nothing -> Just (Map.insert name input accum)
        Just lit ->
          case breakOnText lit input of
            Nothing -> Nothing
            Just (value, remain) -> matchPath rest remain (Map.insert name value accum)

nextLiteral :: [PathToken] -> Maybe Text
nextLiteral [] = Nothing
nextLiteral (Literal l : _) = Just l
nextLiteral (_ : xs) = nextLiteral xs

breakOnText :: Text -> Text -> Maybe (Text, Text)
breakOnText needle haystack =
  let (before, after) = Text.breakOn needle haystack
  in if Text.null after then Nothing else Just (before, Text.drop (Text.length needle) after)


-- | Determine if a Pattern contains any variable path tokens
hasVariables :: Pattern -> Bool
hasVariables (Pattern _ tokens _allowed) = any isVar tokens
  where
    isVar tk = case tk of
      Var _ -> True
      Literal _ -> False
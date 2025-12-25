{-# LANGUAGE OverloadedStrings #-}
module Ext.Test.Generate
  ( writeAggregator
  , aggregatorModuleName
  , generatedDir
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import qualified Data.Name as Name
import qualified Elm.ModuleName as ModuleName
import qualified System.Directory as Dir
import qualified System.FilePath as FP


aggregatorModuleName :: String
aggregatorModuleName = "Everything"


generatedDir :: FilePath -> FilePath
generatedDir root = root `FP.combine` "elm-stuff" `FP.combine` "elm-dev-test"


writeAggregator :: FilePath -> [(ModuleName.Raw, Name.Name)] -> IO FilePath
writeAggregator root testValues = do
  let outDir = generatedDir root
  Dir.createDirectoryIfMissing True outDir
  let outPath = outDir `FP.combine` (aggregatorModuleName ++ ".elm")
  let content = renderAggregator testValues
  BS.writeFile outPath content
  pure outPath


renderAggregator :: [(ModuleName.Raw, Name.Name)] -> BS.ByteString
renderAggregator pairs =
  let imports = unique (map (ModuleName.toChars . fst) pairs)
      importLines = map (\m -> "import " <> m) imports
      refs = map (\(moduleName,value) -> 
                    let name = ModuleName.toChars moduleName ++ "." ++ Name.toChars value in
                    "(\"" ++ name ++ "\", check " ++ name ++ ")"
                  ) pairs
      body = case refs of
        [] -> "[]"
        _  -> "\n    [ " ++ List.intercalate ",\n    " refs ++ "\n    ]"
      lines_ =
        [ "module " ++ aggregatorModuleName ++ " exposing (tests)"
        , ""
        , "import Test exposing (Test)"
        ]
        ++ importLines
        ++ [ ""
           , ""
           , "{-| The implementation of this function will be replaced in the generated JS"
           , "with a version that returns `Just value` if `value` is a `Test`, otherwise `Nothing`."
           , ""
           , "If you rename or change this function you also need to update the regex that looks for it."
           , ""
           , "-}"
           , "check : a -> Maybe Test"
           , "check ="
           , "    Debug.todo \"This function needs to be replaced at runtime so that it can detect test values\""
           , ""
           , ""
           , "tests : List (String, Maybe Test)"
           , "tests = " ++ body
           , ""
           ]
  in BL.toStrict (BB.toLazyByteString (mconcat (map (\l -> BB.string8 l <> BB.string8 "\n") lines_)))


unique :: Ord a => [a] -> [a]
unique = go []
  where
    go seen xs = case xs of
      [] -> reverse seen
      (y:ys) -> if y `elem` seen then go seen ys else go (y:seen) ys



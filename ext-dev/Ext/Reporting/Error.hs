{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Ext.Reporting.Error
  ( toDoc
  , toJson
  
  -- elm dev exported
  , toReports
  )
  where


import qualified Data.NonEmptyList as NE
import qualified Data.OneOrMore as OneOrMore
import qualified System.FilePath as FP

import qualified Elm.ModuleName as ModuleName
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import qualified Ext.Reporting.Error.Canonicalize as Canonicalize
import qualified Ext.Reporting.Error.Docs as Docs
import qualified Ext.Reporting.Error.Import as Import
import qualified Ext.Reporting.Error.Main as Main
import qualified Ext.Reporting.Error.Pattern as Pattern
import qualified Ext.Reporting.Error.Syntax as Syntax
import qualified Ext.Reporting.Error.Type as Type
import qualified Reporting.Render.Code as Code
import qualified Reporting.Report as Report
import Reporting.Error hiding (toDoc, toJson, toReports)



-- MODULE


-- TO REPORT


toReports :: Code.Source -> Error -> NE.List Report.Report
toReports source err =
  case err of
    BadSyntax syntaxError ->
      NE.List (Syntax.toReport source syntaxError) []

    BadImports errs ->
      fmap (Import.toReport source) errs

    BadNames errs ->
      fmap (Canonicalize.toReport source) (OneOrMore.destruct NE.List errs)

    BadTypes localizer errs ->
      fmap (Type.toReport source localizer) errs

    BadMains localizer errs ->
      fmap (Main.toReport localizer source) (OneOrMore.destruct NE.List errs)

    BadPatterns errs ->
      fmap (Pattern.toReport source) errs

    BadDocs docsErr ->
      Docs.toReports source docsErr



-- TO DOC


toDoc :: FilePath -> Module -> [Module] -> D.Doc
toDoc root err errs =
  let
    (NE.List m ms) = NE.sortBy _modificationTime (NE.List err errs)
  in
  D.vcat (toDocHelp root m ms)


toDocHelp :: FilePath -> Module -> [Module] -> [D.Doc]
toDocHelp root module1 modules =
  case modules of
    [] ->
      [moduleToDoc root module1
      ,""
      ]

    module2 : otherModules ->
      moduleToDoc root module1
      : toSeparator module1 module2
      : toDocHelp root module2 otherModules


toSeparator :: Module -> Module -> D.Doc
toSeparator beforeModule afterModule =
  let
    before = ModuleName.toChars (_name beforeModule) ++ "  ↑    "
    after  = "    ↓  " ++  ModuleName.toChars (_name afterModule)
  in
    D.dullred $ D.vcat $
      [ D.indent (80 - length before) (D.fromChars before)
      , "====o======================================================================o===="
      , D.fromChars after
      , ""
      , ""
      ]



-- MODULE TO DOC


moduleToDoc :: FilePath -> Module -> D.Doc
moduleToDoc root (Module _ absolutePath _ source err) =
  let
    reports =
      toReports (Code.toSource source) err

    relativePath =
      FP.makeRelative root absolutePath
  in
  D.vcat $ map (reportToDoc relativePath) (NE.toList reports)


reportToDoc :: FilePath -> Report.Report -> D.Doc
reportToDoc relativePath (Report.Report title _ _ message) =
  D.vcat
    [ toMessageBar title relativePath
    , ""
    , message
    , ""
    ]


toMessageBar :: String -> FilePath -> D.Doc
toMessageBar title filePath =
  let
    usedSpace =
      4 + length title + 1 + length filePath
  in
    D.dullcyan $ D.fromChars $
      "-- " ++ title
      ++ " " ++ replicate (max 1 (80 - usedSpace)) '-'
      ++ " " ++ filePath



-- TO JSON


toJson :: Module -> E.Value
toJson (Module name path _ source err) =
  let
    reports =
      toReports (Code.toSource source) err
  in
  E.object
    [ "path" ==> E.chars path
    , "name" ==> E.name name
    , "problems" ==> E.array (map reportToJson (NE.toList reports))
    ]


reportToJson :: Report.Report -> E.Value
reportToJson (Report.Report title region _sgstns message) =
  E.object
    [ "title" ==> E.chars title
    , "region" ==> encodeRegion region
    , "message" ==> D.encode message
    ]


encodeRegion :: A.Region -> E.Value
encodeRegion (A.Region (A.Position sr sc) (A.Position er ec)) =
  E.object
    [ "start" ==>
          E.object
            [ "line" ==> E.int (fromIntegral sr)
            , "column" ==> E.int (fromIntegral sc)
            ]
    , "end" ==>
          E.object
            [ "line" ==> E.int (fromIntegral er)
            , "column" ==> E.int (fromIntegral ec)
            ]
    ]

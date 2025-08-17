{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Ext.Reporting.Render.Code
  ( toSnippet
 
  )
  where


import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8_BS
import qualified Data.Char as Char
import qualified Data.IntSet as IntSet
import qualified Data.List as List
import qualified Data.Name as Name
import qualified Data.Set as Set
import Data.Word (Word16)

import qualified Reporting.Annotation as A
import qualified Reporting.Doc as D
import Reporting.Doc (Doc)
import Parse.Primitives (Row, Col)
import Parse.Symbol (binopCharSet)
import Parse.Variable (reservedWords)
import Reporting.Render.Code hiding (toSnippet)



-- CODE FORMATTING


toSnippet :: Source -> A.Region -> Maybe A.Region -> (D.Doc, D.Doc) -> D.Doc
toSnippet source region highlight (preHint, postHint) =
  D.vcat
    [ preHint
    -- , ""
    -- , render source region highlight
    , postHint
    ]


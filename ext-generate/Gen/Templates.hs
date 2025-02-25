{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Gen.Templates (templates) where


import qualified Gen.Templates.Loader
import qualified Language.Haskell.TH.Syntax as TH


-- The actual templates list, populated at compile time
templates :: [Gen.Templates.Loader.Template]
templates = $(TH.runIO 
                (Gen.Templates.Loader.read
                    "ext-generate/generator" 
                    [ "app"
                    , "assets"
                    ]
                ) >>= TH.lift
             )
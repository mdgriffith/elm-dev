{-# LANGUAGE ScopedTypeVariables #-}

module Modify (update, Modifications(..)) where

import qualified Elm.ModuleName as Module
import qualified AST.Canonical as Can
import qualified Modify.Ui

data Modifications =
  Modifications
    { uiSourceMap :: Bool
    }

{-|

Do any AST modifications we want for elm-dev


-}
update :: Modifications -> Can.Module -> Can.Module
update modifications canonical =
  if uiSourceMap modifications then
    let
      moduleName :: Module.Canonical = (Can._name canonical)
      decls :: Can.Decls = (Can._decls canonical)
      newDecls :: Can.Decls =  Modify.Ui.updateDecls moduleName decls
    in
    canonical { Can._decls = newDecls }
  else
    canonical

{-# LANGUAGE ScopedTypeVariables #-}

module Modify (update) where

import qualified Elm.ModuleName as Module
import qualified AST.Canonical as Can
import qualified Modify.Ui

{-|

Do any AST modifications we want for elm-dev



-}
update :: Can.Module -> Can.Module
update canonical =
  let
    moduleName :: Module.Canonical = (Can._name canonical)
    decls :: Can.Decls = (Can._decls canonical)
    newDecls :: Can.Decls =  Modify.Ui.updateDecls moduleName decls
  in
  canonical { Can._decls = newDecls }

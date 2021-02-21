module Watchtower.Duplicates where

-- Placeholder file for functions that are known duplicates of Lamdera but just
-- haven't been extracted into ext-* yet.

import Control.Monad (unless)


-- Inversion of `unless` that runs IO only when condition is True
onlyWhen :: Monad f => Bool -> f () -> f ()
onlyWhen condition io =
  unless (not condition) io


withDefault :: a -> Maybe a -> a
withDefault default_ m =
  case m of
    Just v -> v
    Nothing -> default_

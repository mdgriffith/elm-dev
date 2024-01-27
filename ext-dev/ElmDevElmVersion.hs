module ElmDevElmVersion (elmVersion) where


import Data.Version (Version(..))

{-|
This is the version of Elm that Elm Dev is meant to work with.


-}
elmVersion :: Version
elmVersion = Version [0,19,1] []
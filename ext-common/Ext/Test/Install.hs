module Ext.Test.Install
  ( installTestDependency
  , InstallResult(..)
  ) where


import qualified Elm.Package as Pkg
import qualified Ext.DependencyManager as DependencyManager
import qualified Stuff


data InstallResult
  = SuccessfullyInstalled
  | AlreadyInstalled
  deriving (Eq, Show)


installTestDependency :: Pkg.Name -> IO (Either DependencyManager.Error InstallResult)
installTestDependency package =
  do  maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing -> return (Left DependencyManager.NoOutline)
        Just root ->
          do  planned <- DependencyManager.planInstall root DependencyManager.Test [package]
              case planned of
                Left problem -> return (Left problem)
                Right plan ->
                  if not (DependencyManager.planChanged plan)
                  then return (Right AlreadyInstalled)
                  else do
                    applied <- DependencyManager.applyPlan root plan
                    return (fmap (const SuccessfullyInstalled) applied)

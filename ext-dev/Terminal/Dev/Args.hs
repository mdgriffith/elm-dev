module Terminal.Dev.Args
    ( moduleList
    , ModuleList(..)
    , ModuleInfo(..)
    , Module(..)
    , modul
    , Value(..)
    , value
    )
    where

import System.FilePath ((</>), (<.>))
import qualified System.FilePath as Path

import qualified Stuff

import qualified Ext.Dev.Project
import qualified Elm.Details
import qualified Data.Name as Name
import qualified Data.Utf8 as Utf8
import qualified System.Directory as Dir
import qualified Ext.CompileProxy
import qualified Terminal.Dev.Error as Error (Error(..))
import qualified Control.Monad as Monad
import qualified Data.NonEmptyList as NE


data ModuleList =
    ModuleList
        { _modListRoot :: FilePath
        , _modListInfo :: NE.List ModuleInfo
        , _modListDetails :: Elm.Details.Details
        }

data ModuleInfo =
    ModuleInfo
        { _modName :: Name.Name
        , _modFilePath :: FilePath
        }


moduleList :: (FilePath, [FilePath])  -> IO (Either Error.Error ModuleList)
moduleList (basePath, allOtherPaths) = do
    maybeRoot <- Dir.withCurrentDirectory (Path.takeDirectory basePath) Stuff.findRoot
    case maybeRoot of
        Nothing -> pure (Left Error.CouldNotFindRoot)
        Just root -> do
            details <- Ext.CompileProxy.loadProject root
            topModuleInfo <- readModuleInfo basePath details
            moduleInfos <- Monad.mapM (\path -> readModuleInfo path details) allOtherPaths
            case (topModuleInfo, sequence moduleInfos) of
                (Right topInfo, Right infos)->
                    pure (Right (ModuleList root (NE.List topInfo infos) details))
                  
                (Left err, _) -> pure (Left err)

                (_, Left err) -> pure (Left err)
            


{--}

data Module =
    Module
        { _modRoot :: FilePath
        , _moduleInfo :: ModuleInfo
        , _modDetails :: Elm.Details.Details
        }

toAbsoluteFilePath :: FilePath -> IO FilePath
toAbsoluteFilePath path = do 
  cwd <- Dir.getCurrentDirectory
  return $ cwd </> path
  


modul :: FilePath  -> IO (Either Error.Error Module)
modul base = do
    maybeRoot <- Dir.withCurrentDirectory (Path.takeDirectory base) Stuff.findRoot
    case maybeRoot of
        Nothing -> pure (Left Error.CouldNotFindRoot)
        Just root -> do
            details <- Ext.CompileProxy.loadProject root
            moduleInfo <- readModuleInfo base details
            case moduleInfo of
                (Right info)->
                    pure (Right (Module root info details))
                  
                (Left err) -> pure (Left err)
                


readModuleInfo :: String -> Elm.Details.Details -> IO (Either Error.Error ModuleInfo)
readModuleInfo string details = do
   if Path.takeExtension string == ".elm" then do
      absolutePath <- toAbsoluteFilePath string
      case Ext.Dev.Project.lookupModuleName details absolutePath of
        Nothing -> pure (Left Error.CouldNotFindModule)
        Just name -> pure (Right (ModuleInfo name absolutePath))
   else 
      case Ext.Dev.Project.lookupModulePath details (Name.fromChars string) of
          Nothing -> pure (Left Error.CouldNotFindModule)
          Just path -> pure (Right (ModuleInfo (Name.fromChars string) path))


readModulePath :: String -> Elm.Details.Details -> IO (Either Error.Error FilePath)
readModulePath string details = do
   if Path.takeExtension string == ".elm" then do
      absolutePath <- toAbsoluteFilePath string
      pure (Right absolutePath)
   else 
      case Ext.Dev.Project.lookupModulePath details (Name.fromChars string) of
          Nothing -> pure (Left Error.CouldNotFindModule)
          Just path -> pure (Right path)



readModuleName :: String -> Elm.Details.Details -> IO (Either Error.Error Name.Name)
readModuleName string details = do
   if Path.takeExtension string == ".elm" then do
      absolutePath <- toAbsoluteFilePath string
      case Ext.Dev.Project.lookupModuleName details absolutePath of
        Nothing -> pure (Left Error.CouldNotFindModule)
        Just name -> pure (Right name)
   else 
      pure (Right (Name.fromChars string))



data Value =
    Value
        { _valRoot :: String
        , _valueModuleName :: Name.Name
        , _value :: Name.Name
        }


value :: String -> IO (Either Error.Error Value)
value base = do
    maybeRoot <- Stuff.findRoot
    case maybeRoot of
        Nothing -> pure (Left Error.CouldNotFindRoot)
        Just root ->
            case readQualifiedValue base of
                Left err -> pure (Left err)
                Right (modName, valName) ->
                    pure (Right (Value root modName valName))


readQualifiedValue :: String -> Either Error.Error (Name.Name, Name.Name)
readQualifiedValue string =
  case reverse (Name.splitDots (Name.fromChars string)) of
    [] ->
      Left Error.ValueProvidedIsEmpty

    [_] ->
      Left Error.ValueProvidedIsntQualified

    name : path ->
      let 
          correctedPath = (reverse path)

          modName = Utf8.join 0x2E {- . -} correctedPath
      in
      Right
        ( modName
        , name
        )
module Terminal.Dev.Args
    ( Module(..)
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

data Module =
    Module
        { _modRoot :: FilePath
        , _modName :: Name.Name
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
            moduleNameResult <- readModuleName base details
            case moduleNameResult of
                Left err -> pure (Left err)
                Right name ->
                    pure (Right (Module root name details))

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
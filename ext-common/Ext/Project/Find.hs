module Ext.Project.Find (getAllElmModules, findPorts, ModuleListError(..), toElmModuleName) where

import qualified Data.List as List
import qualified Control.Monad
import qualified Elm.Outline
import qualified System.Directory as Dir
import qualified System.FilePath as Path

import qualified Data.Name as Name
import qualified Data.NonEmptyList as NE
import qualified Elm.ModuleName as ModuleName
import qualified Reporting.Exit as Exit
import qualified System.IO



data ModuleListError
    = OutlineError Exit.Outline
    | NoModulesFoundForApp
    | NoModulesFoundForPackage
    

getAllElmModules :: FilePath -> IO (Either ModuleListError (NE.List ModuleName.Raw))
getAllElmModules root = 
  let 
     toNonEmpty onFail list =
        case list of 
          [] -> Left onFail
          (top:remain) -> Right (NE.List top remain)
  in do
  elmJsonResult <- Elm.Outline.read root
  case elmJsonResult of
      Left outlineExit ->
        pure (Left (OutlineError outlineExit))
      
      Right (Elm.Outline.App appOutline) ->
        let 
            srcDirs = fmap (Elm.Outline.toAbsolute root) (Elm.Outline._app_source_dirs appOutline)

            findModuleNames srcDir =
                fmap 
                  (fmap (toElmModuleName srcDir))
                  (findAllElmFiles srcDir)

        in do
        allNestedFiles <- Control.Monad.mapM findModuleNames srcDirs
        pure (toNonEmpty NoModulesFoundForApp (List.concat allNestedFiles))
      
      Right (Elm.Outline.Pkg pkgOutline) ->
        let 
           exposed = Elm.Outline._pkg_exposed pkgOutline
        in
        case exposed of 
          Elm.Outline.ExposedList exposedList ->
            pure (toNonEmpty NoModulesFoundForPackage exposedList)

          Elm.Outline.ExposedDict keyValueList ->
            let 
               exposedList = List.concatMap snd keyValueList
            in
            pure (toNonEmpty NoModulesFoundForPackage exposedList)


findAllElmFiles :: FilePath -> IO [FilePath]
findAllElmFiles dir = do
    contents <- Dir.getDirectoryContents dir
    let paths = map (dir `Path.combine`) $ filter (`notElem` [".", ".."]) contents
    files <- Control.Monad.filterM Dir.doesFileExist paths
    dirs <- Control.Monad.filterM Dir.doesDirectoryExist paths
    let elmFiles = filter (\f -> Path.takeExtension f == ".elm") files
    elmFilesInDirs <- fmap concat $ mapM findAllElmFiles dirs
    return $ elmFiles ++ elmFilesInDirs
  

findPorts :: FilePath -> IO (Either Exit.Outline [FilePath])
findPorts root = do
  elmJsonResult <- Elm.Outline.read root
  case elmJsonResult of
      Left outlineExit ->
        pure (Left outlineExit)
      
      Right (Elm.Outline.App appOutline) ->
          let 
            srcDirs = fmap (Elm.Outline.toAbsolute root) (Elm.Outline._app_source_dirs appOutline)
        in do
        allNestedFiles <- Control.Monad.mapM findPortFilePaths srcDirs
        pure (Right (List.concat allNestedFiles))

      Right (Elm.Outline.Pkg pkgOutline) ->
        pure (Right [])



findPortFilePaths :: FilePath -> IO [FilePath]
findPortFilePaths dir = do
    contents <- Dir.getDirectoryContents dir
    let paths = map (dir `Path.combine`) $ filter (`notElem` [".", ".."]) contents
    files <- Control.Monad.filterM Dir.doesFileExist paths
    dirs <- Control.Monad.filterM Dir.doesDirectoryExist paths
    elmFiles <- Control.Monad.filterM
                    (\f ->
                        if Path.takeExtension f == ".elm" then 
                          isPortModule f
                        else 
                          pure False
                    ) files
    elmFilesInDirs <- fmap concat $ mapM findPortFilePaths dirs
    return $ elmFiles ++ elmFilesInDirs



isPortModule :: FilePath -> IO Bool
isPortModule path = do
    handle <- System.IO.openFile path System.IO.ReadMode
    firstFour <- readFirst handle 4 ""
    System.IO.hClose handle
    pure (firstFour == "port")


readFirst :: System.IO.Handle -> Int -> String -> IO String
readFirst handle n existing =
    if n <= 0 then
        pure existing
    else do
      char <- System.IO.hGetChar handle
      readFirst handle (n - 1) (existing ++ [char])



-- Convert a file path to an Elm module name based on a root directory
toElmModuleName :: FilePath -> FilePath -> ModuleName.Raw
toElmModuleName root file =
    let relativePath = Path.makeRelative root file
        withoutExtension = Path.dropExtension relativePath
    in 
    Name.fromChars (replaceDirectorySeparatorWithDot withoutExtension)



-- Convert a file path to an Elm module name based on a root directory
replaceDirectorySeparatorWithDot :: FilePath -> String
replaceDirectorySeparatorWithDot = 
    let 
        replaceSeparator c = if c == Path.pathSeparator then '.' else c
    in
    map replaceSeparator
  
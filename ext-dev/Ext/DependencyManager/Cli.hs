{-# LANGUAGE OverloadedStrings #-}
module Ext.DependencyManager.Cli
  ( commands
  , renderPlanForTests
  ) where


import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import qualified System.Exit as SystemExit
import qualified System.IO as IO

import qualified CommandParser
import qualified Elm.Constraint as Con
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Ext.DependencyManager as Manager
import Json.Encode ((==>))
import qualified Json.Encode as Encode
import qualified Json.String as Json
import qualified Stuff
import qualified Terminal.Colors as Colors


commands :: [CommandParser.Command]
commands =
  [ installCommand Manager.Production ["install"] "Install dependencies"
  , treeCommand Manager.Production ["dep", "tree"] "Show the dependency tree"
  , unusedCommand Manager.Production ["uninstall", "unused"] "Remove unused dependencies"
  , uninstallCommand Manager.Production ["uninstall"] "Remove dependencies"
  , upgradeCommand Manager.Production ["upgrade"] "Upgrade dependencies"
  , installCommand Manager.Test ["test", "install"] "Install test dependencies"
  , treeCommand Manager.Test ["test", "dep", "tree"] "Show the test dependency tree"
  , unusedCommand Manager.Test ["test", "uninstall", "unused"] "Remove unused test dependencies"
  , uninstallCommand Manager.Test ["test", "uninstall"] "Remove test dependencies"
  , upgradeCommand Manager.Test ["test", "upgrade"] "Upgrade test dependencies"
  ]


dependenciesGroup :: Maybe String
dependenciesGroup = Just "Dependencies"


data MutationOptions =
  MutationOptions
    { optionDryRun :: Bool
    , optionYes :: Bool
    , optionJson :: Bool
    }


installCommand :: Manager.Scope -> [String] -> String -> CommandParser.Command
installCommand scope name description =
  CommandParser.command name description dependenciesGroup (CommandParser.parseArgList installRequirementArg) mutationFlags $ \(package, packages) options ->
    runPlan "install" scope options (Manager.planInstallRequirementsWithScope scope (package : packages))


uninstallCommand :: Manager.Scope -> [String] -> String -> CommandParser.Command
uninstallCommand scope name description =
  CommandParser.command name description dependenciesGroup (CommandParser.parseArgList packageArg) mutationFlags $ \(package, packages) options ->
    runPlan "uninstall" scope options (Manager.planUninstallWithScope scope (package : packages))


unusedCommand :: Manager.Scope -> [String] -> String -> CommandParser.Command
unusedCommand scope name description =
  CommandParser.command name description dependenciesGroup CommandParser.noArg mutationFlags $ \_ options ->
    withRoot (optionJson options) $ \root ->
      do  result <- Manager.findUnused root scope
          case result of
            Left problem -> printError (optionJson options) problem
            Right packages ->
              if null packages
              then
                if optionJson options
                then printJson (Encode.object ["operation" ==> Encode.chars "uninstall-unused", "scope" ==> Encode.chars (scopeChars scope), "status" ==> Encode.chars "unchanged", "written" ==> Encode.bool False, "unused" ==> Encode.list Encode.chars []])
                else putStrLn "No unused direct dependencies were found."
              else
                runPlan "uninstall-unused" scope options (Manager.planUninstallWithScope scope packages)


upgradeCommand :: Manager.Scope -> [String] -> String -> CommandParser.Command
upgradeCommand scope name description =
  CommandParser.command name description dependenciesGroup (CommandParser.parseOptionalArgList packageArg) upgradeFlags $ \targets (dryRun, yes, format, unsafe, allScopes) ->
    let
      options = MutationOptions (present dryRun) (present yes) (format == Just JsonFormat)
      policy = if present unsafe then Manager.AllowMajor else Manager.Compatible
      requested = if null targets then Nothing else Just targets
    in
    runPlan "upgrade" scope options (Manager.planUpgradeWithScope scope policy requested (present allScopes))


treeCommand :: Manager.Scope -> [String] -> String -> CommandParser.Command
treeCommand scope name description =
  CommandParser.command name description dependenciesGroup (CommandParser.parseOptionalArg packageArg) formatFlags $ \target format ->
    withRoot (format == Just JsonFormat) $ \root ->
      do  result <- Manager.dependencyTree root scope
          case result of
            Left problem -> printError (format == Just JsonFormat) problem
            Right tree ->
              let filtered = maybe tree (`Manager.filterDependencyTree` tree) target
              in if format == Just JsonFormat
                 then printJson (encodeTree filtered)
                 else do
                   unusedResult <- Manager.findUnused root scope
                   case unusedResult of
                     Left problem -> printError False problem
                     Right unused -> mapM_ putStrLn (renderTree (Set.fromList unused) filtered)


packageArg :: CommandParser.Arg Pkg.Name
packageArg =
  CommandParser.argWith "package" parsePackage


installRequirementArg :: CommandParser.Arg Manager.PackageRequirement
installRequirementArg =
  CommandParser.argWith "package" Manager.parsePackageRequirement


parsePackage :: String -> Maybe Pkg.Name
parsePackage raw =
  case break (== '/') raw of
    (author, '/' : project)
      | validPart author && validPart project && '/' `notElem` project ->
          Just (Pkg.toName (Utf8.fromChars author) project)
    _ -> Nothing
  where
    validPart part = not (null part) && all validChar part
    validChar char =
      (char >= 'a' && char <= 'z') || (char >= '0' && char <= '9') || char == '-'


data OutputFormat = JsonFormat
  deriving (Eq)


dryRunFlag :: CommandParser.Flag Bool
dryRunFlag = CommandParser.flag "dry-run" "Show changes without writing"


yesFlag :: CommandParser.Flag Bool
yesFlag = CommandParser.flag "yes" "Apply changes without confirmation"


formatFlag :: CommandParser.Flag OutputFormat
formatFlag = CommandParser.flagWithArg "format" "Output format: json" parseFormat
  where
    parseFormat "json" = Just JsonFormat
    parseFormat _ = Nothing


unsafeFlag :: CommandParser.Flag Bool
unsafeFlag = CommandParser.flag "unsafe" "Allow major-version upgrades"


allScopesFlag :: CommandParser.Flag Bool
allScopesFlag = CommandParser.flag "all-scopes" "Upgrade production and test dependencies"


mutationFlags args =
  do  ((dryRun, yes, format), remaining) <- CommandParser.parseFlag3 dryRunFlag yesFlag formatFlag args
      return (MutationOptions (present dryRun) (present yes) (format == Just JsonFormat), remaining)


upgradeFlags =
  CommandParser.parseFlag5 dryRunFlag yesFlag formatFlag unsafeFlag allScopesFlag


formatFlags args =
  do  (format, remaining) <- CommandParser.parseFlag formatFlag args
      return (format, remaining)


present :: Maybe Bool -> Bool
present = maybe False id


runPlan :: String -> Manager.Scope -> MutationOptions -> (FilePath -> IO (Either Manager.Error Manager.Plan)) -> IO ()
runPlan operation scope options planner =
  withRoot (optionJson options) $ \root ->
    do  result <- planner root
        case result of
          Left problem -> printError (optionJson options) problem
          Right plan ->
            if not (Manager.planChanged plan)
            then reportPlan operation scope options "unchanged" False plan
            else if optionDryRun options
              then reportPlan operation scope options "planned" False plan
              else if optionJson options || optionYes options
                then apply operation scope root options True plan
                else do
                  mapM_ putStrLn (renderPlan operation scope plan)
                  approved <- confirm (confirmationPrompt operation plan)
                  if approved
                    then apply operation scope root options False plan
                    else putStrLn "\nNo changes were made."


apply :: String -> Manager.Scope -> FilePath -> MutationOptions -> Bool -> Manager.Plan -> IO ()
apply operation scope root options showPlan plan =
  do  result <- Manager.applyPlan root plan
      case result of
        Left problem -> printError (optionJson options) problem
        Right () -> reportAppliedPlan operation scope options showPlan plan


reportPlan :: String -> Manager.Scope -> MutationOptions -> String -> Bool -> Manager.Plan -> IO ()
reportPlan operation scope options status written plan =
  if optionJson options
  then printJson (encodePlan operation scope status written plan)
  else if not (Manager.planChanged plan)
    then putStrLn (unchangedMessage operation)
    else mapM_ putStrLn (renderPlan operation scope plan ++ ["", "Dry run; no files were changed."])


reportAppliedPlan :: String -> Manager.Scope -> MutationOptions -> Bool -> Manager.Plan -> IO ()
reportAppliedPlan operation scope options showPlan plan =
  if optionJson options
  then printJson (encodePlan operation scope "applied" True plan)
  else mapM_ putStrLn ((if showPlan then renderPlan operation scope plan ++ [""] else [""]) ++ ["Dependencies updated."])


confirm :: String -> IO Bool
confirm prompt =
  do  putStr (prompt ++ " [Y/n]: ")
      IO.hFlush IO.stdout
      answer <- getLine
      return (answer `elem` ["", "y", "Y", "yes", "YES"])


withRoot :: Bool -> (FilePath -> IO ()) -> IO ()
withRoot json callback =
  do  maybeRoot <- Stuff.findRoot
      case maybeRoot of
        Nothing ->
          do  if json
                then printJson (Encode.object ["status" ==> Encode.chars "error", "error" ==> Encode.chars "no-outline"])
                else IO.hPutStrLn IO.stderr "Could not find elm.json."
              SystemExit.exitFailure
        Just root -> callback root


renderPlanForTests :: String -> Manager.Scope -> Manager.Plan -> [String]
renderPlanForTests = renderPlan


renderPlan :: String -> Manager.Scope -> Manager.Plan -> [String]
renderPlan operation scope plan =
  let
    (moved, stationary) = List.partition isMoved (Manager.changes plan)
    (direct, indirect) = List.partition ((== Manager.DirectDependency) . changeKind operation plan) stationary
    primaryHeading = operationHeading operation scope (length direct)
    directSection = renderSectionChanges primaryHeading direct
    sections = filter (not . null) (directSection : indirectChangeSections operation indirect ++ movedChangeSections moved)
  in
  List.intercalate [""] sections


renderSectionChanges :: String -> [Manager.DependencyChange] -> [String]
renderSectionChanges heading changes =
  if null changes
  then []
  else heading : "" : renderChangeLines changes


renderChangeLines :: [Manager.DependencyChange] -> [String]
renderChangeLines changes =
  let width = maximum (map (length . changePackageChars) changes)
  in map (renderChangeLine width) changes


renderChangeLine :: Int -> Manager.DependencyChange -> String
renderChangeLine width change =
  let
    package = changePackageChars change
    padding = replicate (width - length package) ' '
  in
  "  " ++ package ++ padding ++ changeValueChars change


changePackageChars :: Manager.DependencyChange -> String
changePackageChars change = Pkg.toChars (changePackage change)


changePackage :: Manager.DependencyChange -> Pkg.Name
changePackage change =
  case change of
    Manager.DependencyAdded _ package _ -> package
    Manager.DependencyRemoved _ package _ -> package
    Manager.DependencyChanged _ package _ _ -> package
    Manager.DependencyMoved _ _ package _ _ -> package
    Manager.DependencyReclassified _ package _ _ _ -> package


changeValueChars :: Manager.DependencyChange -> String
changeValueChars change =
  case change of
    Manager.DependencyAdded _ _ value -> " @ " ++ valueChars value
    Manager.DependencyRemoved _ _ value -> " @ " ++ valueChars value
    Manager.DependencyChanged _ _ old new -> "  " ++ valueChars old ++ " → " ++ valueChars new
    Manager.DependencyMoved _ _ _ old new -> renderTransition old new
    Manager.DependencyReclassified _ _ _ _ value -> " @ " ++ valueChars value
  where
    renderTransition old new =
      if valueChars old == valueChars new
      then " @ " ++ valueChars new
      else "  " ++ valueChars old ++ " → " ++ valueChars new


changeKind :: String -> Manager.Plan -> Manager.DependencyChange -> Manager.DependencyKind
changeKind operation plan change =
  case change of
    Manager.DependencyAdded scope package _ -> kindInOutline scope package (Manager.newOutline plan)
    Manager.DependencyRemoved scope package _ -> kindInOutline scope package (Manager.oldOutline plan)
    Manager.DependencyChanged scope package _ _ -> kindInOutline scope package (Manager.newOutline plan)
    Manager.DependencyMoved _ newScope package _ _ -> kindInOutline newScope package (Manager.newOutline plan)
    Manager.DependencyReclassified _ _ oldKind newKind _ ->
      if operation `elem` ["uninstall", "uninstall-unused"] then oldKind else newKind


kindInOutline :: Manager.Scope -> Pkg.Name -> Outline.Outline -> Manager.DependencyKind
kindInOutline scope package outline =
  case outline of
    Outline.Pkg _ -> Manager.DirectDependency
    Outline.App app ->
      let
        direct = case scope of
          Manager.Production -> Outline._app_deps_direct app
          Manager.Test -> Outline._app_test_direct app
      in
      if Map.member package direct then Manager.DirectDependency else Manager.IndirectDependency


operationHeading :: String -> Manager.Scope -> Int -> String
operationHeading operation scope count =
  let test = if scope == Manager.Test then "test " else ""
  in case operation of
    "install" -> countHeading ("Install " ++ test ++ "dependency") ("Install " ++ test ++ "dependencies") count
    "uninstall" -> countHeading ("Remove " ++ test ++ "dependency") ("Remove " ++ test ++ "dependencies") count
    "uninstall-unused" -> countHeading ("Unused " ++ test ++ "direct dependency") ("Unused " ++ test ++ "direct dependencies") count
    "upgrade" -> countHeading ("Upgrade " ++ test ++ "dependency") ("Upgrade " ++ test ++ "dependencies") count
    _ -> "Dependency changes"


indirectChangeSections :: String -> [Manager.DependencyChange] -> [[String]]
indirectChangeSections operation changes =
  let
    added = filter isAdded changes
    removed = filter isRemoved changes
    updated = filter (\change -> not (isAdded change) && not (isRemoved change)) changes
    adding = renderSectionChanges ("Also adding " ++ countText "required indirect dependency" "required indirect dependencies" (length added)) added
    removing = renderSectionChanges ("Also removing " ++ countText "now-unneeded indirect dependency" "now-unneeded indirect dependencies" (length removed)) removed
    updating = renderSectionChanges ("Also updating " ++ countText "indirect dependency" "indirect dependencies" (length updated)) updated
  in
  case operation of
    "install" -> [adding, updating, removing]
    "upgrade" -> [updating, adding, removing]
    _ -> [removing, updating, adding]


isAdded :: Manager.DependencyChange -> Bool
isAdded change =
  case change of
    Manager.DependencyAdded _ _ _ -> True
    _ -> False


isRemoved :: Manager.DependencyChange -> Bool
isRemoved change =
  case change of
    Manager.DependencyRemoved _ _ _ -> True
    _ -> False


isMoved :: Manager.DependencyChange -> Bool
isMoved change =
  case change of
    Manager.DependencyMoved _ _ _ _ _ -> True
    _ -> False


movedChangeSections :: [Manager.DependencyChange] -> [[String]]
movedChangeSections changes =
  let
    (toTest, toProduction) = List.partition movedToTest changes
    testHeading = "Also retaining " ++ countText "dependency for tests" "dependencies for tests" (length toTest)
    productionHeading = "Also moving " ++ countText "dependency to production" "dependencies to production" (length toProduction)
  in
  [ renderSectionChanges testHeading toTest
  , renderSectionChanges productionHeading toProduction
  ]


movedToTest :: Manager.DependencyChange -> Bool
movedToTest change =
  case change of
    Manager.DependencyMoved _ Manager.Test _ _ _ -> True
    _ -> False


countHeading :: String -> String -> Int -> String
countHeading singular plural count = if count == 1 then singular else plural


countText :: String -> String -> Int -> String
countText singular plural count = show count ++ " " ++ if count == 1 then singular else plural


confirmationPrompt :: String -> Manager.Plan -> String
confirmationPrompt operation plan =
  let one = length (Manager.changes plan) == 1
  in case operation of
    "install" -> if one then "Install this dependency?" else "Install these dependencies?"
    "uninstall" -> if one then "Remove this dependency?" else "Remove these dependencies?"
    "uninstall-unused" -> if one then "Remove this dependency?" else "Remove these dependencies?"
    "upgrade" -> if one then "Apply this upgrade?" else "Apply these upgrades?"
    _ -> "Apply these changes?"


unchangedMessage :: String -> String
unchangedMessage operation =
  case operation of
    "install" -> "The requested dependencies are already installed."
    "upgrade" -> "Dependencies are already up to date."
    "uninstall-unused" -> "No unused direct dependencies were found."
    _ -> "No dependency changes are needed."


kindName :: Manager.DependencyKind -> String
kindName kind =
  case kind of
    Manager.DirectDependency -> "direct"
    Manager.IndirectDependency -> "indirect"


valueChars :: Manager.DependencyValue -> String
valueChars value =
  case value of
    Manager.AppVersion version -> V.toChars version
    Manager.PackageConstraint constraint -> Con.toChars constraint


scopeChars :: Manager.Scope -> String
scopeChars scope =
  case scope of
    Manager.Production -> "production"
    Manager.Test -> "test"


renderTree :: Set.Set Pkg.Name -> Manager.DependencyTree -> [String]
renderTree unused tree =
  let
    nodes = Map.fromList (map (\node -> (Manager.treePackage node, node)) (Manager.treeNodes tree))
    roots = filter (isRoot (Manager.treeScope tree)) (Manager.treeNodes tree)
    indirect = filter ((== Manager.Indirect) . Manager.treeKind) (Manager.treeNodes tree)
    indirectLines =
      if null indirect
      then []
      else Colors.grey "── indirect dependencies ──" : renderSection unused nodes indirect
  in
  Manager.treeRoot tree : renderSection unused nodes roots ++ indirectLines


isRoot :: Manager.Scope -> Manager.TreeNode -> Bool
isRoot scope node =
  Manager.treeKind node == Manager.ProductionRoot ||
    (scope == Manager.Test && Manager.treeKind node == Manager.TestRoot)


renderSection :: Set.Set Pkg.Name -> Map.Map Pkg.Name Manager.TreeNode -> [Manager.TreeNode] -> [String]
renderSection unused nodes forest =
  go forest
  where
    go remaining =
      case remaining of
        [] -> []
        node : rest ->
          let
            lastNode = null rest
            branch = if lastNode then "└── " else "├── "
            childPrefix = if lastNode then "    " else "│   "
            children = mapMaybeNode nodes (Manager.treeDependencies node)
            usage =
              if Manager.treeKind node == Manager.Indirect
              then [Colors.grey (childPrefix ++ "used by: " ++ List.intercalate ", " (usedBy nodes (Manager.treePackage node)))]
              else []
          in
          (Colors.grey branch ++ renderNode unused node) : usage ++ renderChildren childPrefix children ++ go rest


renderChildren :: String -> [Manager.TreeNode] -> [String]
renderChildren prefix children =
  case children of
    [] -> []
    node : rest ->
      let branch = if null rest then "└── " else "├── "
      in (Colors.grey (prefix ++ branch) ++ renderNode Set.empty node) : renderChildren prefix rest


renderNode :: Set.Set Pkg.Name -> Manager.TreeNode -> String
renderNode unused node =
  Pkg.toChars (Manager.treePackage node)
    ++ Colors.grey (" @ " ++ V.toChars (Manager.treeVersion node))
    ++ rootKindSuffix (Manager.treeKind node)
    ++ if Set.member (Manager.treePackage node) unused then Colors.yellow " [unused]" else ""


usedBy :: Map.Map Pkg.Name Manager.TreeNode -> Pkg.Name -> [String]
usedBy nodes package =
  [ Pkg.toChars (Manager.treePackage node)
  | node <- Map.elems nodes
  , package `elem` Manager.treeDependencies node
  ]


rootKindSuffix :: Manager.TreeKind -> String
rootKindSuffix kind =
  case kind of
    Manager.TestRoot -> Colors.yellow " [test]"
    Manager.ProductionRoot -> ""
    Manager.Indirect -> ""


kindChars :: Manager.TreeKind -> String
kindChars kind =
  case kind of
    Manager.ProductionRoot -> "production"
    Manager.TestRoot -> "test"
    Manager.Indirect -> "indirect"


mapMaybeNode :: Map.Map Pkg.Name Manager.TreeNode -> [Pkg.Name] -> [Manager.TreeNode]
mapMaybeNode nodes = foldr (\name found -> maybe found (: found) (Map.lookup name nodes)) []


encodePlan :: String -> Manager.Scope -> String -> Bool -> Manager.Plan -> Encode.Value
encodePlan operation scope status written plan =
  Encode.object
    [ "operation" ==> Encode.chars operation
    , "scope" ==> Encode.chars (scopeChars scope)
    , "status" ==> Encode.string (Json.fromChars status)
    , "written" ==> Encode.bool written
    , "changes" ==> Encode.list encodeChange (Manager.changes plan)
    ]


encodeChange :: Manager.DependencyChange -> Encode.Value
encodeChange change =
  case change of
    Manager.DependencyAdded scope package value ->
      Encode.object ["kind" ==> Encode.chars "added", "package" ==> Encode.chars (Pkg.toChars package), "scope" ==> Encode.chars (scopeChars scope), "to" ==> Encode.chars (valueChars value)]
    Manager.DependencyRemoved scope package value ->
      Encode.object ["kind" ==> Encode.chars "removed", "package" ==> Encode.chars (Pkg.toChars package), "scope" ==> Encode.chars (scopeChars scope), "from" ==> Encode.chars (valueChars value)]
    Manager.DependencyChanged scope package old new -> changeObject "changed" scope package (Just old) new
    Manager.DependencyMoved oldScope newScope package old new ->
      Encode.object
        [ "kind" ==> Encode.chars "moved"
        , "package" ==> Encode.chars (Pkg.toChars package)
        , "fromScope" ==> Encode.chars (scopeChars oldScope)
        , "scope" ==> Encode.chars (scopeChars newScope)
        , "from" ==> Encode.chars (valueChars old)
        , "to" ==> Encode.chars (valueChars new)
        ]
    Manager.DependencyReclassified scope package oldKind newKind value ->
      Encode.object
        [ "kind" ==> Encode.chars "reclassified"
        , "package" ==> Encode.chars (Pkg.toChars package)
        , "scope" ==> Encode.chars (scopeChars scope)
        , "from" ==> Encode.chars (kindName oldKind)
        , "to" ==> Encode.chars (kindName newKind)
        , "version" ==> Encode.chars (valueChars value)
        ]


changeObject :: String -> Manager.Scope -> Pkg.Name -> Maybe Manager.DependencyValue -> Manager.DependencyValue -> Encode.Value
changeObject kind scope package old new =
  Encode.object
    ([ "kind" ==> Encode.chars kind
     , "package" ==> Encode.chars (Pkg.toChars package)
     , "scope" ==> Encode.chars (scopeChars scope)
     , "to" ==> Encode.chars (valueChars new)
     ] ++ maybe [] (\value -> ["from" ==> Encode.chars (valueChars value)]) old)


encodeTree :: Manager.DependencyTree -> Encode.Value
encodeTree tree =
  Encode.object
    [ "scope" ==> Encode.chars (scopeChars (Manager.treeScope tree))
    , "nodes" ==> Encode.list encodeNode (Manager.treeNodes tree)
    ]
  where
    encodeNode node =
      Encode.object
        [ "package" ==> Encode.chars (Pkg.toChars (Manager.treePackage node))
        , "version" ==> Encode.chars (V.toChars (Manager.treeVersion node))
        , "kind" ==> Encode.chars (kindChars (Manager.treeKind node))
        , "dependencies" ==> Encode.list (Encode.chars . Pkg.toChars) (Manager.treeDependencies node)
        ]


printError :: Bool -> Manager.Error -> IO ()
printError json problem =
  do  if json
        then printJson (Encode.object ["status" ==> Encode.chars "error", "error" ==> Encode.chars (errorCode problem)])
        else IO.hPutStrLn IO.stderr (errorMessage problem)
      SystemExit.exitFailure


errorCode :: Manager.Error -> String
errorCode problem =
  case problem of
    Manager.NoOutline -> "no-outline"
    Manager.RegistryProblem _ -> "registry"
    Manager.OutlineProblem _ -> "elm-json"
    Manager.NoSolution -> "no-solution"
    Manager.NoOfflineSolution -> "no-offline-solution"
    Manager.SolverProblem _ -> "solver"
    Manager.VerificationProblem _ -> "verification"
    Manager.TestRefreshProblem _ -> "test-refresh"
    Manager.AnalysisProblem _ -> "analysis"
    Manager.RequiredDependency package -> "required-dependency:" ++ Pkg.toChars package
    Manager.UnsupportedPackageUpgrade -> "unsupported-package-upgrade"
    Manager.UnsavedChanges -> "unsaved-changes"
    Manager.StalePlan -> "stale-plan"
    Manager.DuplicateRequirement package -> "duplicate-requirement:" ++ Pkg.toChars package
    Manager.UnsupportedPackageExactRequirement package -> "unsupported-package-exact-requirement:" ++ Pkg.toChars package


errorMessage :: Manager.Error -> String
errorMessage problem =
  case problem of
    Manager.RequiredDependency package -> Pkg.toChars package ++ " is required by this project and cannot be removed."
    Manager.UnsupportedPackageUpgrade -> "Upgrading package-project constraints is not supported yet."
    Manager.NoOfflineSolution -> "No compatible dependency solution is available offline."
    Manager.NoSolution -> "No compatible dependency solution exists."
    Manager.UnsavedChanges -> "elm.json has unsaved in-memory changes. Save it before changing dependencies."
    Manager.StalePlan -> "elm.json changed after this dependency operation was planned. Run the command again."
    Manager.DuplicateRequirement package -> Pkg.toChars package ++ " was specified more than once."
    Manager.UnsupportedPackageExactRequirement package -> "Package projects cannot persist an exact constraint for " ++ Pkg.toChars package ++ "."
    _ -> "Dependency operation failed (" ++ errorCode problem ++ ")."


printJson :: Encode.Value -> IO ()
printJson value =
  Lazy.putStrLn (Builder.toLazyByteString (Encode.encodeUgly value))

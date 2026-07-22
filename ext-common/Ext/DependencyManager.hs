{-# OPTIONS_GHC -Wall #-}
module Ext.DependencyManager
  ( Scope(..)
  , UpgradePolicy(..)
  , PackageRequirement(..)
  , VersionRequirement(..)
  , parsePackageRequirement
  , Change(..)
  , DependencyValue(..)
  , DependencyKind(..)
  , DependencyChange(..)
  , Plan(..)
  , planChanged
  , Error(..)
  , TreeKind(..)
  , TreeNode(..)
  , DependencyTree(..)
  , planInstall
  , planInstallRequirements
  , planInstallWithScope
  , planInstallRequirementsWithScope
  , planInstallWith
  , planInstallRequirementsWith
  , planUninstall
  , planUninstallWithScope
  , planUninstallWith
  , planUpgrade
  , planUpgradeWithScope
  , planUpgradeWith
  , dependencyTree
  , dependencyTreeWith
  , dependencyTreeFromSolution
  , filterDependencyTree
  , findUnused
  , unusedDirectDependencies
  , applyPlan
  , applyPlanWith
  , detectChanges
  , diffOutlines
  , diffAppOutlines
  , diffPkgOutlines
  , installAppRoots
  , installAppRootsWith
  , packageConstraintForRequirement
  , packageRequirementSupported
  , uninstallAppRoots
  , upgradeAppRoots
  ) where


import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Utf8 as Utf8
import Data.Word (Word16)
import qualified Text.Read as Read
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as Lazy
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import System.FilePath ((</>))
import qualified Control.Exception as Exception

import qualified BackgroundWriter as BW
import qualified Deps.Solver as Solver
import qualified Elm.Constraint as Con
import qualified Elm.Details as Details
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Ext.FileCache as FileCache
import qualified Ext.Test.Compile as TestCompile
import qualified Ext.Test.Generate as TestGenerate
import qualified Json.Encode as Encode
import qualified Reporting.Exit as Exit
import qualified Reporting
import qualified Stuff


data Scope
  = Production
  | Test
  deriving (Eq, Ord, Show)


data UpgradePolicy
  = Compatible
  | AllowMajor
  deriving (Eq, Show)


data PackageRequirement =
  PackageRequirement Pkg.Name VersionRequirement
  deriving (Eq)


data VersionRequirement
  = Latest
  | Major Word16
  | Exact V.Version
  deriving (Eq)


parsePackageRequirement :: String -> Maybe PackageRequirement
parsePackageRequirement raw =
  case break (== '@') raw of
    (packageChars, "") ->
      fmap (\name -> PackageRequirement name Latest) (parsePackageName packageChars)
    (packageChars, '@' : versionChars) ->
      do  name <- parsePackageName packageChars
          requirement <- parseVersionRequirement versionChars
          return (PackageRequirement name requirement)
    _ -> Nothing


parsePackageName :: String -> Maybe Pkg.Name
parsePackageName raw =
  case break (== '/') raw of
    (author, '/' : project)
      | validPart author && validPart project && '/' `notElem` project ->
          Just (Pkg.toName (Utf8.fromChars author) project)
    _ -> Nothing
  where
    validPart part = not (null part) && all validChar part
    validChar char =
      (char >= 'a' && char <= 'z') || (char >= '0' && char <= '9') || char == '-'


parseVersionRequirement :: String -> Maybe VersionRequirement
parseVersionRequirement chars =
  case splitOn '.' chars of
    [majorChars] -> Major <$> readWord16 majorChars
    [majorChars, minorChars, patchChars] ->
      Exact <$> (V.Version <$> readWord16 majorChars <*> readWord16 minorChars <*> readWord16 patchChars)
    _ -> Nothing


readWord16 :: String -> Maybe Word16
readWord16 chars =
  do  value <- Read.readMaybe chars :: Maybe Integer
      if value >= 0 && value <= fromIntegral (maxBound :: Word16)
      then Just (fromIntegral value)
      else Nothing


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn separator values =
  case break (== separator) values of
    (before, []) -> [before]
    (before, _ : after) -> before : splitOn separator after


data Change a
  = Added a
  | Removed a
  | Changed a a
  deriving (Eq, Show)


data DependencyValue
  = AppVersion V.Version
  | PackageConstraint Con.Constraint
  deriving (Eq)


data DependencyKind
  = DirectDependency
  | IndirectDependency
  deriving (Eq, Ord, Show)


data DependencyChange
  = DependencyAdded Scope Pkg.Name DependencyValue
  | DependencyRemoved Scope Pkg.Name DependencyValue
  | DependencyChanged Scope Pkg.Name DependencyValue DependencyValue
  | DependencyMoved Scope Scope Pkg.Name DependencyValue DependencyValue
  | DependencyReclassified Scope Pkg.Name DependencyKind DependencyKind DependencyValue
  deriving (Eq)


data Plan =
  Plan
    { oldOutline :: Outline.Outline
    , newOutline :: Outline.Outline
    , changes :: [DependencyChange]
    }


data Error
  = NoOutline
  | RegistryProblem Exit.RegistryProblem
  | OutlineProblem Exit.Outline
  | NoSolution
  | NoOfflineSolution
  | SolverProblem Exit.Solver
  | VerificationProblem Exit.Details
  | TestRefreshProblem Exit.Reactor
  | AnalysisProblem Exit.Details
  | RequiredDependency Pkg.Name
  | UnsupportedPackageUpgrade
  | UnsavedChanges
  | StalePlan
  | DuplicateRequirement Pkg.Name
  | UnsupportedPackageExactRequirement Pkg.Name


data TreeKind
  = ProductionRoot
  | TestRoot
  | Indirect
  deriving (Eq, Show)


data TreeNode =
  TreeNode
    { treePackage :: Pkg.Name
    , treeVersion :: V.Version
    , treeKind :: TreeKind
    , treeDependencies :: [Pkg.Name]
    }
  deriving (Eq)


data DependencyTree =
  DependencyTree
    { treeRoot :: String
    , treeScope :: Scope
    , treeNodes :: [TreeNode]
    }
  deriving (Eq)


planInstall :: FilePath -> Scope -> [Pkg.Name] -> IO (Either Error Plan)
planInstall root scope packages =
  planInstallRequirements root scope (map (\name -> PackageRequirement name Latest) packages)


planInstallRequirements :: FilePath -> Scope -> [PackageRequirement] -> IO (Either Error Plan)
planInstallRequirements root scope requirements =
  withProject root (\env outline -> planInstallRequirementsWith env scope requirements outline)


planInstallWithScope :: Scope -> [Pkg.Name] -> FilePath -> IO (Either Error Plan)
planInstallWithScope scope packages root =
  planInstall root scope packages


planInstallRequirementsWithScope :: Scope -> [PackageRequirement] -> FilePath -> IO (Either Error Plan)
planInstallRequirementsWithScope scope requirements root =
  planInstallRequirements root scope requirements


planInstallWith :: Solver.Env -> Scope -> [Pkg.Name] -> Outline.Outline -> IO (Either Error Plan)
planInstallWith env scope packages old =
  planInstallRequirementsWith env scope (map (\name -> PackageRequirement name Latest) packages) old


planInstallRequirementsWith :: Solver.Env -> Scope -> [PackageRequirement] -> Outline.Outline -> IO (Either Error Plan)
planInstallRequirementsWith env scope requirements old =
  let active = filter (not . installRequirementIsNoop scope old) requirements
  in case firstDuplicateRequirement requirements of
    Just name -> return (Left (DuplicateRequirement name))
    Nothing -> case old of
      Outline.Pkg _ ->
        case firstUnsupportedPackageRequirement requirements of
          Just name -> return (Left (UnsupportedPackageExactRequirement name))
          Nothing -> continue active
      Outline.App _ -> continue active
  where
    continue active =
      case incompatibleAvailableRequirements scope old requirements of
        True -> return (Left NoSolution)
        False | null active -> return (Right (unchangedPlan old))
        False -> case old of
          Outline.App app -> solveAppInstall env old scope active app
          Outline.Pkg pkg -> installPkg env old scope active pkg


firstDuplicateRequirement :: [PackageRequirement] -> Maybe Pkg.Name
firstDuplicateRequirement requirements =
  go Set.empty requirements
  where
    go _ [] = Nothing
    go seen (PackageRequirement name _ : remaining) =
      if Set.member name seen then Just name else go (Set.insert name seen) remaining


firstUnsupportedPackageRequirement :: [PackageRequirement] -> Maybe Pkg.Name
firstUnsupportedPackageRequirement requirements =
  case filter (not . packageRequirementSupported) requirements of
    PackageRequirement name _ : _ -> Just name
    [] -> Nothing


packageRequirementSupported :: PackageRequirement -> Bool
packageRequirementSupported (PackageRequirement _ requirement) =
  case requirement of
    Exact _ -> False
    _ -> True


planUninstall :: FilePath -> Scope -> [Pkg.Name] -> IO (Either Error Plan)
planUninstall root scope packages =
  withProject root (\env outline -> planUninstallWith env scope packages outline)


planUninstallWithScope :: Scope -> [Pkg.Name] -> FilePath -> IO (Either Error Plan)
planUninstallWithScope scope packages root =
  planUninstall root scope packages


planUninstallWith :: Solver.Env -> Scope -> [Pkg.Name] -> Outline.Outline -> IO (Either Error Plan)
planUninstallWith env scope packages old =
  if not (hasUninstallChange scope packages old)
  then return (Right (unchangedPlan old))
  else case old of
    Outline.App app ->
      if scope == Production && Pkg.core `elem` packages && Map.member Pkg.core (Outline._app_deps_direct app)
      then return (Left (RequiredDependency Pkg.core))
      else solveAppUninstall env old (uninstallAppRoots scope packages app) app

    Outline.Pkg pkg ->
      uninstallPkg env old scope packages pkg


planUpgrade :: FilePath -> Scope -> UpgradePolicy -> Maybe [Pkg.Name] -> Bool -> IO (Either Error Plan)
planUpgrade root scope policy targets allScopes =
  withProject root (\env outline -> planUpgradeWith env scope policy targets allScopes outline)


planUpgradeWithScope :: Scope -> UpgradePolicy -> Maybe [Pkg.Name] -> Bool -> FilePath -> IO (Either Error Plan)
planUpgradeWithScope scope policy targets allScopes root =
  planUpgrade root scope policy targets allScopes


planUpgradeWith :: Solver.Env -> Scope -> UpgradePolicy -> Maybe [Pkg.Name] -> Bool -> Outline.Outline -> IO (Either Error Plan)
planUpgradeWith env scope policy targets allScopes old =
  case old of
    Outline.Pkg _ ->
      return (Left UnsupportedPackageUpgrade)

    Outline.App app ->
      if null (selectedUpgradeNames scope targets allScopes old)
      then return (Right (unchangedPlan old))
      else solveAppPlan env old (upgradeAppRoots scope policy targets allScopes app) (Just ensureNoTargetDowngrades)
  where
    ensureNoTargetDowngrades solution =
      let
        oldVersions = appVersions old
        newVersions = Solver._new solution
        selected = selectedUpgradeNames scope targets allScopes old
        downgraded name = Map.lookup name newVersions < Map.lookup name oldVersions
      in
      if any downgraded selected then Left NoSolution else Right ()


dependencyTree :: FilePath -> Scope -> IO (Either Error DependencyTree)
dependencyTree root scope =
  withProject root $ \env outline ->
    do  result <- dependencyTreeWith env scope outline
        return (fmap (\tree -> tree { treeRoot = dependencyTreeRoot root outline }) result)


dependencyTreeWith :: Solver.Env -> Scope -> Outline.Outline -> IO (Either Error DependencyTree)
dependencyTreeWith (Solver.Env cache _ connection registry) scope outline =
  do  result <- Solver.verify cache connection registry (treeConstraints outline)
      return $
        case result of
          Solver.Ok solution -> Right (dependencyTreeFromSolution scope outline solution)
          Solver.NoSolution -> Left NoSolution
          Solver.NoOfflineSolution -> Left NoOfflineSolution
          Solver.Err problem -> Left (SolverProblem problem)


findUnused :: FilePath -> Scope -> IO (Either Error [Pkg.Name])
findUnused root scope =
  do  eitherOutline <- Outline.read root
      case eitherOutline of
        Left problem -> return (Left (OutlineProblem problem))
        Right outline ->
          BW.withScope $ \buildScope ->
            do  productionDetails <- Details.load Reporting.silent buildScope root
                case productionDetails of
                  Left problem -> return (Left (AnalysisProblem problem))
                  Right details ->
                    do  testDirectoryExists <- Dir.doesDirectoryExist (root </> "tests")
                        eitherTestUsed <-
                          if testDirectoryExists
                          then loadTestUsedPackages buildScope root
                          else return (Right Set.empty)
                        case eitherTestUsed of
                          Left problem -> return (Left problem)
                          Right testUsed ->
                            let
                              productionUsed = Set.union (usedPackages details) testUsed
                              candidates = directNames scope outline
                              used = if scope == Production then productionUsed else testUsed
                              protectedUsed =
                                case outline of
                                  Outline.App _ -> Set.insert Pkg.json used
                                  Outline.Pkg _ -> used
                                    in
                                    return (Right (unusedDirectDependencies candidates protectedUsed))


unusedDirectDependencies :: [Pkg.Name] -> Set.Set Pkg.Name -> [Pkg.Name]
unusedDirectDependencies candidates used =
  filter (\name -> name /= Pkg.core && Set.notMember name used) candidates


loadTestUsedPackages :: BW.Scope -> FilePath -> IO (Either Error (Set.Set Pkg.Name))
loadTestUsedPackages buildScope root =
  do  refresh <- TestCompile.regenerateTestElmJson root
      case refresh of
        Left problem -> return (Left (TestRefreshProblem problem))
        Right () ->
          do  testDetails <- Details.load Reporting.silent buildScope (TestGenerate.generatedDir root)
              case testDetails of
                Left problem -> return (Left (AnalysisProblem problem))
                Right testInfo -> return (Right (usedPackages testInfo))


usedPackages :: Details.Details -> Set.Set Pkg.Name
usedPackages details =
  let
    foreigns = Details._foreigns details
    importedModules = concatMap Details._deps (Map.elems (Details._locals details))
    addOwner moduleName found =
      case Map.lookup moduleName foreigns of
        Nothing -> found
        Just (Details.Foreign owner alternatives) -> Set.union found (Set.fromList (owner : alternatives))
  in
  foldr addOwner Set.empty importedModules


directNames :: Scope -> Outline.Outline -> [Pkg.Name]
directNames scope outline =
  case (scope, outline) of
    (Production, Outline.App app) -> Map.keys (Outline._app_deps_direct app)
    (Test, Outline.App app) -> Map.keys (Outline._app_test_direct app)
    (Production, Outline.Pkg pkg) -> Map.keys (Outline._pkg_deps pkg)
    (Test, Outline.Pkg pkg) -> Map.keys (Outline._pkg_test_deps pkg)


applyPlan :: FilePath -> Plan -> IO (Either Error ())
applyPlan root plan =
  applyPlanWith root verify refresh plan
  where
    verify outline =
      do  eitherEnv <- Solver.initEnv
          case eitherEnv of
            Left problem -> return (Left (RegistryProblem problem))
            Right env ->
              BW.withScope $ \scope ->
                do  result <- Details.verifyInstall scope root env outline
                    return (either (Left . VerificationProblem) (const (Right ())) result)

    refresh =
      do  result <- TestCompile.regenerateTestElmJsonPersisted root
          return (either (Left . TestRefreshProblem) (const (Right ())) result)


applyPlanWith :: FilePath -> (Outline.Outline -> IO (Either Error ())) -> IO (Either Error ()) -> Plan -> IO (Either Error ())
applyPlanWith root verify refresh plan =
  if not (planChanged plan)
  then return (Right ())
  else Stuff.withRootLock root $
    do  let path = root </> "elm.json"
            testPath = TestGenerate.generatedDir root </> "elm.json"
        rootSnapshot@(PersistedFileSnapshot maybeOriginalBytes originalCacheBytes) <- snapshotPersistedFile path
        testSnapshot <- snapshotPersistedFile testPath
        case maybeOriginalBytes of
          Nothing -> return (Left NoOutline)
          Just originalBytes -> case originalCacheBytes of
            Just cachedBytes | cachedBytes /= originalBytes -> return (Left UnsavedChanges)
            _ ->
              do  current <- Outline.read root
                  case current of
                    Left problem -> return (Left (OutlineProblem problem))
                    Right currentOutline | outlineBytes currentOutline /= outlineBytes (oldOutline plan) -> return (Left StalePlan)
                    Right _ ->
                      let
                        restore =
                          do  restorePersistedFile path rootSnapshot
                              restorePersistedFile testPath testSnapshot
                        transaction =
                          do  writeOutlinePersisted root (newOutline plan)
                              verified <- verify (newOutline plan)
                              case verified of
                                Left problem -> restore >> return (Left problem)
                                Right () ->
                                  do  refreshed <- refresh
                                      case refreshed of
                                        Left problem -> restore >> return (Left problem)
                                        Right () -> return (Right ())
                      in transaction `Exception.onException` restore


data PersistedFileSnapshot =
  PersistedFileSnapshot (Maybe BS.ByteString) (Maybe BS.ByteString)


snapshotPersistedFile :: FilePath -> IO PersistedFileSnapshot
snapshotPersistedFile path =
  do  exists <- Dir.doesFileExist path
      diskBytes <- if exists then fmap Just (BS.readFile path) else pure Nothing
      cached <- FileCache.lookup path
      pure (PersistedFileSnapshot diskBytes (fmap snd cached))


restorePersistedFile :: FilePath -> PersistedFileSnapshot -> IO ()
restorePersistedFile path (PersistedFileSnapshot diskBytes cacheBytes) =
  do  case diskBytes of
        Just bytes -> writeBytesPersisted path bytes
        Nothing ->
          do  exists <- Dir.doesFileExist path
              if exists then Dir.removeFile path else pure ()
      case cacheBytes of
        Just bytes -> FileCache.insert path bytes
        Nothing -> FileCache.delete path


writeOutlinePersisted :: FilePath -> Outline.Outline -> IO ()
writeOutlinePersisted root outline =
  let
    path = root </> "elm.json"
    temporary = root </> ".elm.json.elm-dev.tmp"
    bytes = Lazy.toStrict (Builder.toLazyByteString (Encode.encode (Outline.encode outline) <> Builder.char7 '\n'))
  in
  writeBytesAtomically temporary path bytes


writeBytesPersisted :: FilePath -> BS.ByteString -> IO ()
writeBytesPersisted path bytes =
  writeBytesAtomically (path ++ ".elm-dev.tmp") path bytes


writeBytesAtomically :: FilePath -> FilePath -> BS.ByteString -> IO ()
writeBytesAtomically temporary path bytes =
  do  BS.writeFile temporary bytes
      Dir.renameFile temporary path
      FileCache.insert path bytes


withProject :: FilePath -> (Solver.Env -> Outline.Outline -> IO (Either Error a)) -> IO (Either Error a)
withProject root callback =
  do  eitherEnv <- Solver.initEnv
      case eitherEnv of
        Left problem ->
          return (Left (RegistryProblem problem))

        Right env ->
          do  eitherOutline <- Outline.read root
              case eitherOutline of
                Left problem -> return (Left (OutlineProblem problem))
                Right outline -> callback env outline


installRequirementIsNoop :: Scope -> Outline.Outline -> PackageRequirement -> Bool
installRequirementIsNoop scope outline (PackageRequirement name requirement) =
  case outline of
    Outline.App app ->
      case scope of
        Production -> requirement == Latest && Map.member name (Outline._app_deps_direct app)
        Test ->
          maybe False (requirementAccepts requirement) (Map.lookup name (Map.union (Outline._app_deps_direct app) (Outline._app_deps_indirect app)))
            || (requirement == Latest && Map.member name (Outline._app_test_direct app))

    Outline.Pkg pkg ->
      case scope of
        Production -> requirement == Latest && Map.member name (Outline._pkg_deps pkg)
        Test ->
          maybe False (packageRequirementCompatible requirement) (Map.lookup name (Outline._pkg_deps pkg))
            || (requirement == Latest && Map.member name (Outline._pkg_test_deps pkg))


incompatibleAvailableRequirements :: Scope -> Outline.Outline -> [PackageRequirement] -> Bool
incompatibleAvailableRequirements scope outline requirements =
  scope == Test && any incompatible requirements
  where
    productionVersions = appVersions outline
    incompatible (PackageRequirement name requirement) =
      case (outline, Map.lookup name productionVersions) of
        (Outline.App app, Just version) ->
          Map.member name (Map.union (Outline._app_deps_direct app) (Outline._app_deps_indirect app))
            && not (requirementAccepts requirement version)
        (Outline.Pkg pkg, _) ->
          case Map.lookup name (Outline._pkg_deps pkg) of
            Nothing -> False
            Just constraint -> not (packageRequirementCompatible requirement constraint)
        _ -> False


requirementAccepts :: VersionRequirement -> V.Version -> Bool
requirementAccepts requirement version =
  case requirement of
    Latest -> True
    Major major -> Con.satisfies (Con.untilNextMajor (V.Version major 0 0)) version
    Exact required -> required == version


packageRequirementCompatible :: VersionRequirement -> Con.Constraint -> Bool
packageRequirementCompatible requirement constraint =
  case requirement of
    Latest -> True
    Major major -> constraintsOverlap constraint (Con.untilNextMajor (V.Version major 0 0))
    Exact version -> Con.satisfies constraint version


constraintsOverlap :: Con.Constraint -> Con.Constraint -> Bool
constraintsOverlap one two =
  case Con.intersect one two of
    Just _ -> True
    Nothing -> False


hasUninstallChange :: Scope -> [Pkg.Name] -> Outline.Outline -> Bool
hasUninstallChange scope packages outline =
  case (scope, outline) of
    (Production, Outline.App app) -> any (\name -> Map.member name (Outline._app_deps_direct app)) packages
    (Test, Outline.App app) -> any (\name -> Map.member name (Outline._app_test_direct app)) packages
    (Production, Outline.Pkg pkg) -> any (\name -> Map.member name (Outline._pkg_deps pkg)) packages
    (Test, Outline.Pkg pkg) -> any (\name -> Map.member name (Outline._pkg_test_deps pkg)) packages


solveAppPlan :: Solver.Env -> Outline.Outline -> (Map.Map Pkg.Name Con.Constraint, Map.Map Pkg.Name Con.Constraint) -> Maybe (Solver.AppSolution -> Either Error ()) -> IO (Either Error Plan)
solveAppPlan env old rootConstraints validate =
  solveAppPlanWithExtra env old rootConstraints Map.empty validate


solveAppPlanWithExtra :: Solver.Env -> Outline.Outline -> (Map.Map Pkg.Name Con.Constraint, Map.Map Pkg.Name Con.Constraint) -> Map.Map Pkg.Name Con.Constraint -> Maybe (Solver.AppSolution -> Either Error ()) -> IO (Either Error Plan)
solveAppPlanWithExtra (Solver.Env cache _ connection registry) old@(Outline.App app) (direct, testDirect) extra validate =
  do  result <- Solver.solveAppWith cache connection registry direct testDirect extra app
      return $
        case result of
          Solver.Ok solution@(Solver.AppSolution _ _ newApp) ->
            case validate of
              Nothing -> Right (makePlan old (Outline.App newApp))
              Just check ->
                case check solution of
                  Left problem -> Left problem
                  Right () -> Right (makePlan old (Outline.App newApp))

          Solver.NoSolution -> Left NoSolution
          Solver.NoOfflineSolution -> Left NoOfflineSolution
          Solver.Err problem -> Left (SolverProblem problem)
solveAppPlanWithExtra _ _ _ _ _ =
  error "compiler bug manifesting in Ext.DependencyManager.solveAppPlanWithExtra"


solveAppUninstall :: Solver.Env -> Outline.Outline -> (Map.Map Pkg.Name Con.Constraint, Map.Map Pkg.Name Con.Constraint) -> Outline.AppOutline -> IO (Either Error Plan)
solveAppUninstall env@(Solver.Env cache _ connection registry) old rootConstraints app =
  do  first <- solveAppPlan env old rootConstraints (Just ensureValidApp)
      case first of
        Left problem -> return (Left problem)
        Right firstPlan ->
          let
            stillRequired = appVersions (newOutline firstPlan)
            oldIndirect = Map.union (Outline._app_deps_indirect app) (Outline._app_test_indirect app)
            pinned = Map.map Con.exactly (Map.intersection oldIndirect stillRequired)
            (direct, testDirect) = rootConstraints
          in
          do  result <- Solver.solveAppWith cache connection registry direct testDirect pinned app
              return $
                case result of
                  Solver.Ok solution@(Solver.AppSolution _ _ newApp) ->
                    case ensureValidApp solution of
                      Left problem -> Left problem
                      Right () -> Right (makePlan old (Outline.App newApp))
                  Solver.NoSolution -> Right firstPlan
                  Solver.NoOfflineSolution -> Right firstPlan
                  Solver.Err _ -> Right firstPlan


solveAppInstall :: Solver.Env -> Outline.Outline -> Scope -> [PackageRequirement] -> Outline.AppOutline -> IO (Either Error Plan)
solveAppInstall env old scope requirements app =
  let
    requestedNames = map requirementName requirements
    oldIndirect = Map.union (Outline._app_deps_indirect app) (Outline._app_test_indirect app)
    pinnedIndirect = Map.map Con.exactly (foldr Map.delete oldIndirect requestedNames)
  in
  tryAttempts (attempts scope pinnedIndirect)
  where
    attempts installScope pinned =
      [ (attemptRoots Con.exactly, pinned)
      , (attemptRoots Con.exactly, Map.empty)
      , (attemptRoots Con.untilNextMinor, Map.empty)
      , (attemptRoots Con.untilNextMajor, Map.empty)
      , (attemptRoots (\_ -> Con.anything), Map.empty)
      ]
      where
        attemptRoots oldConstraint =
          if installScope == Test
          then installAppRootsWith Con.exactly oldConstraint installScope requirements app
          else installAppRootsWith oldConstraint oldConstraint installScope requirements app

    tryAttempts remainingAttempts =
      case remainingAttempts of
        [] -> return (Left NoSolution)
        (constraints, extra) : remaining ->
          do  result <- solveAppPlanWithExtra env old constraints extra Nothing
              case (result, remaining) of
                (Left NoSolution, _ : _) -> tryAttempts remaining
                (Left NoOfflineSolution, _ : _) -> tryAttempts remaining
                _ -> return result


installAppRoots :: (V.Version -> Con.Constraint) -> Scope -> [PackageRequirement] -> Outline.AppOutline -> (Map.Map Pkg.Name Con.Constraint, Map.Map Pkg.Name Con.Constraint)
installAppRoots oldConstraint scope requirements app =
  installAppRootsWith oldConstraint oldConstraint scope requirements app


installAppRootsWith :: (V.Version -> Con.Constraint) -> (V.Version -> Con.Constraint) -> Scope -> [PackageRequirement] -> Outline.AppOutline -> (Map.Map Pkg.Name Con.Constraint, Map.Map Pkg.Name Con.Constraint)
installAppRootsWith productionConstraint testConstraint scope requirements app =
  let
    direct = Outline._app_deps_direct app
    testDirect = Outline._app_test_direct app
    allVersions = appVersions (Outline.App app)
    requested = Map.fromList (map (requirementConstraint allVersions) requirements)
    packages = map requirementName requirements
    fixedDirect = Map.map productionConstraint direct
    fixedTest = Map.map testConstraint testDirect
  in
  case scope of
    Production ->
      (Map.union requested fixedDirect, foldr Map.delete fixedTest packages)

    Test ->
      let available = Map.union (Outline._app_deps_direct app) (Outline._app_deps_indirect app)
      in (fixedDirect, Map.union (Map.difference requested available) fixedTest)


requirementConstraint :: Map.Map Pkg.Name V.Version -> PackageRequirement -> (Pkg.Name, Con.Constraint)
requirementConstraint versions (PackageRequirement name requirement) =
  (name, case requirement of
    Latest -> maybe Con.anything Con.exactly (Map.lookup name versions)
    Major major -> Con.untilNextMajor (V.Version major 0 0)
    Exact version -> Con.exactly version
  )


requirementName :: PackageRequirement -> Pkg.Name
requirementName (PackageRequirement name _) = name


uninstallAppRoots :: Scope -> [Pkg.Name] -> Outline.AppOutline -> (Map.Map Pkg.Name Con.Constraint, Map.Map Pkg.Name Con.Constraint)
uninstallAppRoots scope packages app =
  let
    direct = Map.map Con.exactly (Outline._app_deps_direct app)
    testDirect = Map.map Con.exactly (Outline._app_test_direct app)
  in
  case scope of
    Production -> (foldr Map.delete direct packages, testDirect)
    Test -> (direct, foldr Map.delete testDirect packages)


ensureValidApp :: Solver.AppSolution -> Either Error ()
ensureValidApp (Solver.AppSolution _ _ app) =
  if Map.member Pkg.json (Map.union (Outline._app_deps_direct app) (Outline._app_deps_indirect app))
  then Right ()
  else Left (RequiredDependency Pkg.json)


upgradeAppRoots :: Scope -> UpgradePolicy -> Maybe [Pkg.Name] -> Bool -> Outline.AppOutline -> (Map.Map Pkg.Name Con.Constraint, Map.Map Pkg.Name Con.Constraint)
upgradeAppRoots scope policy targets allScopes app =
  let
    selected = selectedUpgradeNames scope targets allScopes (Outline.App app)
    constraint name version =
      if name `elem` selected
      then upgradeConstraint policy version
      else Con.exactly version
  in
  ( Map.mapWithKey constraint (Outline._app_deps_direct app)
  , Map.mapWithKey constraint (Outline._app_test_direct app)
  )


upgradeConstraint :: UpgradePolicy -> V.Version -> Con.Constraint
upgradeConstraint policy version =
  case policy of
    Compatible -> Con.untilNextMajor version
    AllowMajor -> Con.atLeast version


selectedUpgradeNames :: Scope -> Maybe [Pkg.Name] -> Bool -> Outline.Outline -> [Pkg.Name]
selectedUpgradeNames scope targets allScopes outline =
  case outline of
    Outline.Pkg _ -> []
    Outline.App app ->
      let
        direct = Map.keys (Outline._app_deps_direct app)
        testDirect = Map.keys (Outline._app_test_direct app)
        eligible =
          if allScopes
          then direct ++ testDirect
          else case scope of
            Production -> direct
            Test -> testDirect
      in
      case targets of
        Nothing -> eligible
        Just names -> filter (`elem` names) eligible


installPkg :: Solver.Env -> Outline.Outline -> Scope -> [PackageRequirement] -> Outline.PkgOutline -> IO (Either Error Plan)
installPkg env old scope requirements pkg =
  let
    direct = Outline._pkg_deps pkg
    testDirect = Outline._pkg_test_deps pkg
    requested = Map.fromList (map (requirementConstraint Map.empty) requirements)
    packages = map requirementName requirements
    (candidateDirect, candidateTest, additions) =
      case scope of
        Production ->
          (Map.union requested direct, foldr Map.delete testDirect packages, packages)
        Test ->
          (direct, Map.union requested testDirect, packages)
  in
  verifyPkg env (Map.union candidateDirect candidateTest) $ \solution ->
    let
      requirementByName = Map.fromList (map (\requirement -> (requirementName requirement, requirement)) requirements)
      constraints = Map.fromList (map (toPackageConstraint requirementByName solution) additions)
      newPkg =
        case scope of
          Production -> pkg { Outline._pkg_deps = Map.union constraints direct, Outline._pkg_test_deps = foldr Map.delete testDirect packages }
          Test -> pkg { Outline._pkg_test_deps = Map.union constraints testDirect }
    in
    Right (makePlan old (Outline.Pkg newPkg))


toPackageConstraint :: Map.Map Pkg.Name PackageRequirement -> Map.Map Pkg.Name Solver.Details -> Pkg.Name -> (Pkg.Name, Con.Constraint)
toPackageConstraint requirements solution name =
  case (Map.lookup name requirements, Map.lookup name solution) of
    (Just requirement, Just (Solver.Details version _)) -> (name, packageConstraintForRequirement requirement version)
    _ -> error "compiler bug manifesting in Ext.DependencyManager.toPackageConstraint"


packageConstraintForRequirement :: PackageRequirement -> V.Version -> Con.Constraint
packageConstraintForRequirement (PackageRequirement _ requirement) solvedVersion =
  case requirement of
    Latest -> Con.untilNextMajor solvedVersion
    Major _ -> Con.untilNextMajor solvedVersion
    Exact version -> Con.exactly version


uninstallPkg :: Solver.Env -> Outline.Outline -> Scope -> [Pkg.Name] -> Outline.PkgOutline -> IO (Either Error Plan)
uninstallPkg env old scope packages pkg =
  if scope == Production && Pkg.core `elem` packages && Map.member Pkg.core (Outline._pkg_deps pkg)
  then return (Left (RequiredDependency Pkg.core))
  else
    let
      newPkg =
        case scope of
          Production -> pkg { Outline._pkg_deps = foldr Map.delete (Outline._pkg_deps pkg) packages }
          Test -> pkg { Outline._pkg_test_deps = foldr Map.delete (Outline._pkg_test_deps pkg) packages }
      constraints = Map.union (Outline._pkg_deps newPkg) (Outline._pkg_test_deps newPkg)
    in
    verifyPkg env constraints (\_ -> Right (makePlan old (Outline.Pkg newPkg)))


verifyPkg :: Solver.Env -> Map.Map Pkg.Name Con.Constraint -> (Map.Map Pkg.Name Solver.Details -> Either Error a) -> IO (Either Error a)
verifyPkg (Solver.Env cache _ connection registry) constraints callback =
  do  result <- Solver.verify cache connection registry constraints
      return $
        case result of
          Solver.Ok solution -> callback solution
          Solver.NoSolution -> Left NoSolution
          Solver.NoOfflineSolution -> Left NoOfflineSolution
          Solver.Err problem -> Left (SolverProblem problem)


makePlan :: Outline.Outline -> Outline.Outline -> Plan
makePlan old new =
  Plan old new (diffOutlines old new)


unchangedPlan :: Outline.Outline -> Plan
unchangedPlan outline =
  Plan outline outline []


planChanged :: Plan -> Bool
planChanged plan =
  outlineBytes (oldOutline plan) /= outlineBytes (newOutline plan)


outlineBytes :: Outline.Outline -> BS.ByteString
outlineBytes outline =
  Lazy.toStrict (Builder.toLazyByteString (Encode.encode (Outline.encode outline) <> Builder.char7 '\n'))


detectChanges :: (Ord key, Eq value) => Map.Map key value -> Map.Map key value -> [(key, Change value)]
detectChanges old new =
  Map.toAscList $
    Map.mergeWithKey
      (\_ oldValue newValue -> if oldValue == newValue then Nothing else Just (Changed oldValue newValue))
      (Map.map Removed)
      (Map.map Added)
      old
      new


diffOutlines :: Outline.Outline -> Outline.Outline -> [DependencyChange]
diffOutlines old new =
  case (old, new) of
    (Outline.App oldApp, Outline.App newApp) -> diffAppOutlines oldApp newApp
    (Outline.Pkg oldPkg, Outline.Pkg newPkg) -> diffPkgOutlines oldPkg newPkg
    _ -> []


diffAppOutlines :: Outline.AppOutline -> Outline.AppOutline -> [DependencyChange]
diffAppOutlines old new =
  diffScoped
    (scopedAppValues old)
    (scopedAppValues new)


diffPkgOutlines :: Outline.PkgOutline -> Outline.PkgOutline -> [DependencyChange]
diffPkgOutlines old new =
  diffScoped
    (scopedPkgValues old)
    (scopedPkgValues new)


diffScoped :: Map.Map Pkg.Name (Scope, DependencyKind, DependencyValue) -> Map.Map Pkg.Name (Scope, DependencyKind, DependencyValue) -> [DependencyChange]
diffScoped old new =
  map toDependencyChange (detectChanges old new)


toDependencyChange :: (Pkg.Name, Change (Scope, DependencyKind, DependencyValue)) -> DependencyChange
toDependencyChange (name, change) =
  case change of
    Added (scope, _, value) -> DependencyAdded scope name value
    Removed (scope, _, value) -> DependencyRemoved scope name value
    Changed (oldScope, oldKind, oldValue) (newScope, newKind, newValue) ->
      if oldScope == newScope
      then if oldKind == newKind
        then DependencyChanged oldScope name oldValue newValue
        else DependencyReclassified oldScope name oldKind newKind newValue
      else DependencyMoved oldScope newScope name oldValue newValue


scopedAppValues :: Outline.AppOutline -> Map.Map Pkg.Name (Scope, DependencyKind, DependencyValue)
scopedAppValues app =
  Map.unions
    [ Map.map (\version -> (Production, DirectDependency, AppVersion version)) (Outline._app_deps_direct app)
    , Map.map (\version -> (Production, IndirectDependency, AppVersion version)) (Outline._app_deps_indirect app)
    , Map.map (\version -> (Test, DirectDependency, AppVersion version)) (Outline._app_test_direct app)
    , Map.map (\version -> (Test, IndirectDependency, AppVersion version)) (Outline._app_test_indirect app)
    ]


scopedPkgValues :: Outline.PkgOutline -> Map.Map Pkg.Name (Scope, DependencyKind, DependencyValue)
scopedPkgValues pkg =
  Map.union
    (Map.map (\constraint -> (Production, DirectDependency, PackageConstraint constraint)) (Outline._pkg_deps pkg))
    (Map.map (\constraint -> (Test, DirectDependency, PackageConstraint constraint)) (Outline._pkg_test_deps pkg))


appVersions :: Outline.Outline -> Map.Map Pkg.Name V.Version
appVersions outline =
  case outline of
    Outline.Pkg _ -> Map.empty
    Outline.App app ->
      Map.unions
        [ Outline._app_deps_direct app
        , Outline._app_deps_indirect app
        , Outline._app_test_direct app
        , Outline._app_test_indirect app
        ]


treeConstraints :: Outline.Outline -> Map.Map Pkg.Name Con.Constraint
treeConstraints outline =
  case outline of
    Outline.App _ -> Map.map Con.exactly (appVersions outline)
    Outline.Pkg pkg -> Map.union (Outline._pkg_deps pkg) (Outline._pkg_test_deps pkg)


dependencyTreeFromSolution :: Scope -> Outline.Outline -> Map.Map Pkg.Name Solver.Details -> DependencyTree
dependencyTreeFromSolution scope outline solution =
  let
    (productionRoots, testRoots) = roots outline
    includedRoots = if scope == Production then productionRoots else productionRoots ++ testRoots
    included = reachable solution includedRoots
    toNode name (Solver.Details version deps) =
      TreeNode name version (kindFor productionRoots testRoots name) (Map.keys (Map.intersection deps included))
  in
  DependencyTree (defaultDependencyTreeRoot outline) scope (Map.elems (Map.mapWithKey toNode (Map.intersection solution included)))


dependencyTreeRoot :: FilePath -> Outline.Outline -> String
dependencyTreeRoot root outline =
  case outline of
    Outline.App _ -> FilePath.takeFileName (FilePath.dropTrailingPathSeparator root)
    Outline.Pkg pkg -> Pkg.toChars (Outline._pkg_name pkg)


defaultDependencyTreeRoot :: Outline.Outline -> String
defaultDependencyTreeRoot outline =
  case outline of
    Outline.App _ -> "project"
    Outline.Pkg pkg -> Pkg.toChars (Outline._pkg_name pkg)


filterDependencyTree :: Pkg.Name -> DependencyTree -> DependencyTree
filterDependencyTree target tree =
  let
    nodes = Map.fromList (map (\node -> (treePackage node, node)) (treeNodes tree))
    keep = Set.fromList [ name | name <- Map.keys nodes, reachesTarget nodes Set.empty name target ]
    keepNode node = Set.member (treePackage node) keep
    trim node = node { treeDependencies = filter (`Set.member` keep) (treeDependencies node) }
  in
  tree { treeNodes = map trim (filter keepNode (treeNodes tree)) }


reachesTarget :: Map.Map Pkg.Name TreeNode -> Set.Set Pkg.Name -> Pkg.Name -> Pkg.Name -> Bool
reachesTarget nodes visited current target =
  current == target ||
    if Set.member current visited
    then False
    else case Map.lookup current nodes of
      Nothing -> False
      Just node -> any (\child -> reachesTarget nodes (Set.insert current visited) child target) (treeDependencies node)


roots :: Outline.Outline -> ([Pkg.Name], [Pkg.Name])
roots outline =
  case outline of
    Outline.App app -> (Map.keys (Outline._app_deps_direct app), Map.keys (Outline._app_test_direct app))
    Outline.Pkg pkg -> (Map.keys (Outline._pkg_deps pkg), Map.keys (Outline._pkg_test_deps pkg))


kindFor :: [Pkg.Name] -> [Pkg.Name] -> Pkg.Name -> TreeKind
kindFor productionRoots testRoots name =
  if name `elem` productionRoots
  then ProductionRoot
  else if name `elem` testRoots
  then TestRoot
  else Indirect


reachable :: Map.Map Pkg.Name Solver.Details -> [Pkg.Name] -> Map.Map Pkg.Name ()
reachable solution unvisited =
  go unvisited Map.empty
  where
    go names visited =
      case names of
        [] -> visited
        name : remaining ->
          if Map.member name visited
          then go remaining visited
          else
            case Map.lookup name solution of
              Nothing -> go remaining visited
              Just (Solver.Details _ deps) -> go (Map.keys deps ++ remaining) (Map.insert name () visited)

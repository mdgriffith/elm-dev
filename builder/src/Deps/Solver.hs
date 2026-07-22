{-# LANGUAGE OverloadedStrings, Rank2Types #-}
module Deps.Solver
  ( Solver
  , Result(..)
  , Connection(..)
  --
  , Details(..)
  , verify
  --
  , AppSolution(..)
  , addToApp
  , solveApp
  , solveAppWith
  , classifyApp
  --
  , Env(..)
  , initEnv
  )
  where


import Control.Monad (foldM)
import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar)
import qualified Data.Map as Map
import Data.Map ((!))
import qualified System.Directory as Dir
import System.FilePath ((</>))

import qualified Deps.Registry as Registry
import qualified Deps.Website as Website
import qualified Elm.Constraint as C
import qualified Elm.Package as Pkg
import qualified Elm.Outline as Outline
import qualified Elm.Version as V
import qualified Ext.FileProxy as File
import qualified Http
import qualified Json.Decode as D
import qualified Reporting.Exit as Exit
import qualified Stuff



-- SOLVER


newtype Solver a =
  Solver
  (
    forall b.
      State
      -> (State -> a -> (State -> IO b) -> IO b)
      -> (State -> IO b)
      -> (Exit.Solver -> IO b)
      -> IO b
  )


data State =
  State
    { _cache :: Stuff.PackageCache
    , _connection :: Connection
    , _registry :: Registry.Registry
    , _constraints :: Map.Map (Pkg.Name, V.Version) Constraints
    }


data Constraints =
  Constraints
    { _elm :: C.Constraint
    , _deps :: Map.Map Pkg.Name C.Constraint
    }


data Connection
  = Online Http.Manager
  | Offline



-- RESULT


data Result a
  = Ok a
  | NoSolution
  | NoOfflineSolution
  | Err Exit.Solver



-- VERIFY -- used by Elm.Details


data Details =
  Details V.Version (Map.Map Pkg.Name C.Constraint)


verify :: Stuff.PackageCache -> Connection -> Registry.Registry -> Map.Map Pkg.Name C.Constraint -> IO (Result (Map.Map Pkg.Name Details))
verify cache connection registry constraints =
  Stuff.withRegistryLock cache $
  case try constraints of
    Solver solver ->
      solver (State cache connection registry Map.empty)
        (\s a _ -> return $ Ok (Map.mapWithKey (addDeps s) a))
        (\_     -> return $ noSolution connection)
        (\e     -> return $ Err e)


addDeps :: State -> Pkg.Name -> V.Version -> Details
addDeps (State _ _ _ constraints) name vsn =
  case Map.lookup (name, vsn) constraints of
    Just (Constraints _ deps) -> Details vsn deps
    Nothing                   -> error "compiler bug manifesting in Deps.Solver.addDeps"


noSolution :: Connection -> Result a
noSolution connection =
  case connection of
    Online _ -> NoSolution
    Offline -> NoOfflineSolution



-- ADD TO APP - used in Install


data AppSolution =
  AppSolution
    { _old :: Map.Map Pkg.Name V.Version
    , _new :: Map.Map Pkg.Name V.Version
    , _app :: Outline.AppOutline
    }


addToApp :: Stuff.PackageCache -> Connection -> Registry.Registry -> Pkg.Name -> Outline.AppOutline -> IO (Result AppSolution)
addToApp cache connection registry pkg outline@(Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  Stuff.withRegistryLock cache $
  let
    allIndirects = Map.union indirect testIndirect
    allDirects = Map.union direct testDirect
    allDeps = Map.union allDirects allIndirects

    attempt toConstraint deps =
      try (Map.insert pkg C.anything (Map.map toConstraint deps))
  in
  case
    oneOf
      ( attempt C.exactly allDeps )
      [ attempt C.exactly allDirects
      , attempt C.untilNextMinor allDirects
      , attempt C.untilNextMajor allDirects
      , attempt (\_ -> C.anything) allDirects
      ]
  of
    Solver solver ->
      solver (State cache connection registry Map.empty)
        (\s a _ -> return $ Ok (toApp s pkg outline allDeps a))
        (\_     -> return $ noSolution connection)
        (\e     -> return $ Err e)


-- SOLVE APP


solveApp :: Stuff.PackageCache -> Connection -> Registry.Registry -> Map.Map Pkg.Name C.Constraint -> Map.Map Pkg.Name C.Constraint -> Outline.AppOutline -> IO (Result AppSolution)
solveApp cache connection registry directConstraints testConstraints outline@(Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  solveAppWith cache connection registry directConstraints testConstraints Map.empty outline


solveAppWith :: Stuff.PackageCache -> Connection -> Registry.Registry -> Map.Map Pkg.Name C.Constraint -> Map.Map Pkg.Name C.Constraint -> Map.Map Pkg.Name C.Constraint -> Outline.AppOutline -> IO (Result AppSolution)
solveAppWith cache connection registry directConstraints testConstraints extraConstraints outline@(Outline.AppOutline _ _ direct indirect testDirect testIndirect) =
  Stuff.withRegistryLock cache $
  case try (Map.unions [directConstraints, testConstraints, extraConstraints]) of
    Solver solver ->
      let
        old = Map.unions [direct, indirect, testDirect, testIndirect]
      in
      solver (State cache connection registry Map.empty)
        (\s new _ -> return $ Ok (toAppWithRoots s outline old directConstraints testConstraints new))
        (\_       -> return $ noSolution connection)
        (\e       -> return $ Err e)


toApp :: State -> Pkg.Name -> Outline.AppOutline -> Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name V.Version -> AppSolution
toApp (State _ _ _ constraints) pkg (Outline.AppOutline elm srcDirs direct _ testDirect _) old new =
  let
    d   = Map.intersection new (Map.insert pkg V.one direct)
    i   = Map.difference (getTransitive constraints new (Map.toList d) Map.empty) d
    td  = Map.intersection new (Map.delete pkg testDirect)
    ti  = Map.difference new (Map.unions [d,i,td])
  in
  AppSolution old new (Outline.AppOutline elm srcDirs d i td ti)


toAppWithRoots :: State -> Outline.AppOutline -> Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name C.Constraint -> Map.Map Pkg.Name C.Constraint -> Map.Map Pkg.Name V.Version -> AppSolution
toAppWithRoots state outline old directConstraints testConstraints new =
  AppSolution old new (classifyApp outline directConstraints testConstraints (Map.mapWithKey (addDeps state) new))


classifyApp :: Outline.AppOutline -> Map.Map Pkg.Name C.Constraint -> Map.Map Pkg.Name C.Constraint -> Map.Map Pkg.Name Details -> Outline.AppOutline
classifyApp (Outline.AppOutline elm srcDirs _ _ _ _) directConstraints testConstraints solution =
  let
    versions = Map.map (\(Details version _) -> version) solution
    directNames = Map.map (const V.one) directConstraints
    testNames = Map.map (const V.one) testConstraints
    direct = Map.intersection versions directNames
    indirect = Map.difference (getDetailsTransitive solution (Map.toList direct) Map.empty) direct
    production = Map.union direct indirect
    testRoots = Map.difference (Map.intersection versions testNames) production
    testClosure = getDetailsTransitive solution (Map.toList testRoots) Map.empty
    testDirect = Map.difference testRoots production
    testIndirect = Map.difference testClosure (Map.unions [production, testDirect])
  in
  Outline.AppOutline elm srcDirs direct indirect testDirect testIndirect


getDetailsTransitive :: Map.Map Pkg.Name Details -> [(Pkg.Name,V.Version)] -> Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name V.Version
getDetailsTransitive solution unvisited visited =
  case unvisited of
    [] ->
      visited

    (pkg,vsn) : remaining ->
      if Map.member pkg visited
      then getDetailsTransitive solution remaining visited
      else
        case Map.lookup pkg solution of
          Nothing ->
            getDetailsTransitive solution remaining (Map.insert pkg vsn visited)

          Just (Details _ deps) ->
            let
              versions = Map.map (\(Details version _) -> version) solution
              newUnvisited = Map.toList (Map.intersection versions (Map.difference deps visited))
            in
            getDetailsTransitive solution (newUnvisited ++ remaining) (Map.insert pkg vsn visited)


getTransitive :: Map.Map (Pkg.Name, V.Version) Constraints -> Map.Map Pkg.Name V.Version -> [(Pkg.Name,V.Version)] -> Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name V.Version
getTransitive constraints solution unvisited visited =
  case unvisited of
    [] ->
      visited

    info@(pkg,vsn) : infos ->
      if Map.member pkg visited
      then getTransitive constraints solution infos visited
      else
        let
          newDeps = _deps (constraints ! info)
          newUnvisited = Map.toList (Map.intersection solution (Map.difference newDeps visited))
          newVisited = Map.insert pkg vsn visited
        in
        getTransitive constraints solution infos $
          getTransitive constraints solution newUnvisited newVisited



-- TRY


try :: Map.Map Pkg.Name C.Constraint -> Solver (Map.Map Pkg.Name V.Version)
try constraints =
  exploreGoals (Goals constraints Map.empty)



-- EXPLORE GOALS


data Goals =
  Goals
    { _pending :: Map.Map Pkg.Name C.Constraint
    , _solved :: Map.Map Pkg.Name V.Version
    }


exploreGoals :: Goals -> Solver (Map.Map Pkg.Name V.Version)
exploreGoals (Goals pending solved) =
  case Map.minViewWithKey pending of
    Nothing ->
      return solved

    Just ((name, constraint), otherPending) ->
      do  let goals1 = Goals otherPending solved
          let addVsn = addVersion goals1 name
          (v,vs) <- getRelevantVersions name constraint
          goals2 <- oneOf (addVsn v) (map addVsn vs)
          exploreGoals goals2


addVersion :: Goals -> Pkg.Name -> V.Version -> Solver Goals
addVersion (Goals pending solved) name version =
  do  (Constraints elm deps) <- getConstraints name version
      if C.goodElm elm
        then
          do  newPending <- foldM (addConstraint solved) pending (Map.toList deps)
              return (Goals newPending (Map.insert name version solved))
        else
          backtrack


addConstraint :: Map.Map Pkg.Name V.Version -> Map.Map Pkg.Name C.Constraint -> (Pkg.Name, C.Constraint) -> Solver (Map.Map Pkg.Name C.Constraint)
addConstraint solved unsolved (name, newConstraint) =
  case Map.lookup name solved of
    Just version ->
      if C.satisfies newConstraint version
      then return unsolved
      else backtrack

    Nothing ->
      case Map.lookup name unsolved of
        Nothing ->
          return $ Map.insert name newConstraint unsolved

        Just oldConstraint ->
          case C.intersect oldConstraint newConstraint of
            Nothing ->
              backtrack

            Just mergedConstraint ->
              if oldConstraint == mergedConstraint
              then return unsolved
              else return (Map.insert name mergedConstraint unsolved)



-- GET RELEVANT VERSIONS


getRelevantVersions :: Pkg.Name -> C.Constraint -> Solver (V.Version, [V.Version])
getRelevantVersions name constraint =
  Solver $ \state@(State _ _ registry _) ok back _ ->
    case Registry.getVersions name registry of
      Just (Registry.KnownVersions newest previous) ->
        case filter (C.satisfies constraint) (newest:previous) of
          []   -> back state
          v:vs -> ok state (v,vs) back

      Nothing ->
        back state



-- GET CONSTRAINTS


getConstraints :: Pkg.Name -> V.Version -> Solver Constraints
getConstraints pkg vsn =
  Solver $ \state@(State cache connection registry cDict) ok back err ->
    do  let key = (pkg, vsn)
        case Map.lookup key cDict of
          Just cs ->
            ok state cs back

          Nothing ->
            do  let toNewState cs = State cache connection registry (Map.insert key cs cDict)
                let home = Stuff.package cache pkg vsn
                let path = home </> "elm.json"
                outlineExists <- File.exists path
                if outlineExists
                  then
                    do  bytes <- File.readUtf8 path
                        case D.fromByteString constraintsDecoder bytes of
                          Right cs ->
                            case connection of
                              Online _ ->
                                ok (toNewState cs) cs back

                              Offline ->
                                do  srcExists <- Dir.doesDirectoryExist (Stuff.package cache pkg vsn </> "src")
                                    if srcExists
                                      then ok (toNewState cs) cs back
                                      else back state

                          Left  _  ->
                            do  File.remove path
                                err (Exit.SolverBadCacheData pkg vsn)
                  else
                    case connection of
                      Offline ->
                        back state

                      Online manager ->
                        do  let url = Website.metadata pkg vsn "elm.json"
                            result <- Http.get manager url [] id (return . Right)
                            case result of
                              Left httpProblem ->
                                err (Exit.SolverBadHttp pkg vsn httpProblem)

                              Right body ->
                                case D.fromByteString constraintsDecoder body of
                                  Right cs ->
                                    do  Dir.createDirectoryIfMissing True home
                                        File.writeUtf8 path body
                                        ok (toNewState cs) cs back

                                  Left _ ->
                                    err (Exit.SolverBadHttpData pkg vsn url)


constraintsDecoder :: D.Decoder () Constraints
constraintsDecoder =
  do  outline <- D.mapError (const ()) Outline.decoder
      case outline of
        Outline.Pkg (Outline.PkgOutline _ _ _ _ _ deps _ elmConstraint) ->
          return (Constraints elmConstraint deps)

        Outline.App _ ->
          D.failure ()



-- ENVIRONMENT


data Env =
  Env Stuff.PackageCache Http.Manager Connection Registry.Registry


initEnv :: IO (Either Exit.RegistryProblem Env)
initEnv =
  do  mvar  <- newEmptyMVar
      _     <- forkIO $ putMVar mvar =<< Http.getManager
      cache <- Stuff.getPackageCache
      Stuff.withRegistryLock cache $
        do  maybeRegistry <- Registry.read cache
            manager       <- readMVar mvar

            case maybeRegistry of
              Nothing ->
                do  eitherRegistry <- Registry.fetch manager cache
                    case eitherRegistry of
                      Right latestRegistry ->
                        return $ Right $ Env cache manager (Online manager) latestRegistry

                      Left problem ->
                        return $ Left $ problem

              Just cachedRegistry ->
                do  eitherRegistry <- Registry.update manager cache cachedRegistry
                    case eitherRegistry of
                      Right latestRegistry ->
                        return $ Right $ Env cache manager (Online manager) latestRegistry

                      Left _ ->
                        return $ Right $ Env cache manager Offline cachedRegistry



-- INSTANCES


instance Functor Solver where
  fmap func (Solver solver) =
    Solver $ \state ok back err ->
      let
        okA stateA arg backA = ok stateA (func arg) backA
      in
      solver state okA back err


instance Applicative Solver where
  pure a =
    Solver $ \state ok back _ -> ok state a back

  (<*>) (Solver solverFunc) (Solver solverArg) =
    Solver $ \state ok back err ->
      let
        okF stateF func backF =
          let
            okA stateA arg backA = ok stateA (func arg) backA
          in
          solverArg stateF okA backF err
      in
      solverFunc state okF back err


instance Monad Solver where
  return a =
    Solver $ \state ok back _ -> ok state a back

  (>>=) (Solver solverA) callback =
    Solver $ \state ok back err ->
      let
        okA stateA a backA =
          case callback a of
            Solver solverB -> solverB stateA ok backA err
      in
      solverA state okA back err


oneOf :: Solver a -> [Solver a] -> Solver a
oneOf solver@(Solver solverHead) solvers =
  case solvers of
    [] ->
      solver

    s:ss ->
      Solver $ \state0 ok back err ->
        let
          tryTail state1 =
            let
              (Solver solverTail) = oneOf s ss
            in
            solverTail state1 ok back err
        in
        solverHead state0 ok tryTail err


backtrack :: Solver a
backtrack =
  Solver $ \state _ back _ -> back state

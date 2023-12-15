module Ext.Dev.Project.Ports (empty, Ports(..), toPorts, PortGroup, findPorts) where

import qualified Control.Monad as Monad
import qualified Control.Concurrent.MVar as MVar

import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.List as List

import qualified AST.Optimized as Opt
import qualified AST.Canonical as Can

import qualified Elm.ModuleName as ModuleName
import qualified Elm.Interface
import qualified Build
import qualified Ext.CompileProxy

import qualified Ext.CompileHelpers.Generic
import qualified System.IO
import qualified Ext.Project.Find

{-
    UNDER CONSTRUCTION!

    Still need to figure out the best way to get port definitions for a project.



-}


data Ports =
  Ports
    -- subs
    { _incoming :: PortGroup
    
    -- cmds
    , _outgoing :: PortGroup
    }
    deriving (Show)


type PortGroup = Map.Map (ModuleName.Raw, Name.Name) Can.Type

empty :: Ports
empty =
  Ports
    { _incoming = Map.empty
    , _outgoing = Map.empty
    }



findPorts :: FilePath -> IO Ports
findPorts root = do
    portsResult <- Ext.Project.Find.findPorts root
    case portsResult of
        Left err ->
            pure empty

        Right portPaths ->
            Monad.foldM (capturePorts root) empty portPaths
    
    
capturePorts :: FilePath -> Ports -> FilePath -> IO Ports
capturePorts root ports path = do
    (Ext.CompileProxy.Single source warnings interfaces maybeCanonical compiled) <- Ext.CompileProxy.loadSingle root path
    case maybeCanonical of
        Nothing ->
            pure ports

        Just canonical ->
            let 
                effects = Can._effects canonical

                moduleName = ModuleName._module (Can._name canonical)
            in
            case effects of
                Can.NoEffects ->
                    pure ports

                Can.Manager _ _ _ _ -> 
                    pure ports

                Can.Ports portMap ->
                    pure (Map.foldrWithKey (mergeInPort moduleName) ports portMap)



mergeInPort :: ModuleName.Raw -> Name.Name -> Can.Port -> Ports -> Ports
mergeInPort moduleName name port (Ports incoming outgoing) =
    case port of
        Can.Incoming freeVars payload func ->
            Ports
                (Map.insert (moduleName, name) payload incoming)
                outgoing

        Can.Outgoing freeVars payload func ->
            Ports
                incoming
                (Map.insert (moduleName, name) payload outgoing)




toPorts :: Build.Artifacts -> IO Ports
toPorts (Build.Artifacts name deps roots mods) = do
  incoming <- Monad.mapM moduleToIncoming mods

  pure $
    Ports
        { _incoming = Map.unions incoming
                -- Map.foldrWithKey (getIncoming ifaces) Map.empty nodes
        , _outgoing = Map.empty 
                -- Map.foldrWithKey (getOutgoing ifaces) Map.empty nodes
        }


moduleToIncoming :: Build.Module -> IO PortGroup
moduleToIncoming modul =
    let 
        group = Map.empty
    in
    case modul of 
        Build.Fresh modName interface local ->
            pure group
        
        Build.Cached modName isMain mvarCached -> do
            cached <- MVar.readMVar mvarCached
            case cached of
                Build.Unneeded -> pure group
                Build.Corrupted -> pure group
                Build.Loaded interface -> pure group
                    


getIncoming :: Map.Map ModuleName.Raw Elm.Interface.Interface -> Opt.Global -> Opt.Node -> PortGroup -> PortGroup
getIncoming interfaces (Opt.Global moduleName name) node group =
    case node of
        Opt.PortIncoming _ _ ->
            case Map.lookup (ModuleName._module moduleName) interfaces of
                Nothing ->
                    group

                Just interface ->
                    case Map.lookup name (Elm.Interface._values interface) of
                        Nothing ->
                            group

                        Just (Can.Forall _ type_) ->
                            Map.insert ((ModuleName._module moduleName), name) type_ group

        _ ->
            group


getOutgoing :: Map.Map ModuleName.Raw Elm.Interface.Interface -> Opt.Global -> Opt.Node -> PortGroup -> PortGroup
getOutgoing interfaces (Opt.Global moduleName name) node group =
    case node of
        Opt.PortOutgoing _ _ ->
            case Map.lookup (ModuleName._module moduleName) interfaces of
                Nothing ->
                    group

                Just interface ->
                    case Map.lookup name (Elm.Interface._values interface) of
                        Nothing ->
                            group

                        Just (Can.Forall _ type_) ->
                            Map.insert ((ModuleName._module moduleName), name) type_ group

        _ ->
            group
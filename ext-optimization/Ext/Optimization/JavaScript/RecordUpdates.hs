{-# LANGUAGE OverloadedStrings #-}
module Ext.Optimization.JavaScript.RecordUpdates
  ( RecordShapes
  , recordShapes
  , constructorName
  , cloneName
  , oldName
  , updatedName
  )
  where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name

import qualified AST.Optimized as Opt
import qualified Generate.JavaScript.Name as JsName



type RecordShapes =
  Map.Map [Name.Name] Int


recordShapes :: Opt.GlobalGraph -> RecordShapes
recordShapes (Opt.GlobalGraph nodes _) =
  List.foldl' addShape Map.empty $ concatMap collectNodeRecords (Map.elems nodes)


addShape :: RecordShapes -> [Name.Name] -> RecordShapes
addShape shapes fields =
  Map.insertWith keepExisting fields (Map.size shapes + 1) shapes


keepExisting :: Int -> Int -> Int
keepExisting _ old =
  old


collectNodeRecords :: Opt.Node -> [[Name.Name]]
collectNodeRecords node =
  case node of
    Opt.Define expr _ ->
      collectExprRecords expr

    Opt.DefineTailFunc _ body _ ->
      collectExprRecords body

    Opt.Cycle _ values functions _ ->
      concatMap (collectExprRecords . snd) values ++ concatMap collectDefRecords functions

    _ ->
      []


collectDefRecords :: Opt.Def -> [[Name.Name]]
collectDefRecords def =
  case def of
    Opt.Def _ expr ->
      collectExprRecords expr

    Opt.TailDef _ _ expr ->
      collectExprRecords expr



collectExprRecords :: Opt.Expr -> [[Name.Name]]
collectExprRecords expr =
  case expr of
    Opt.List entries ->
      concatMap collectExprRecords entries

    Opt.Function _ body ->
      collectExprRecords body

    Opt.Call func args ->
      collectExprRecords func ++ concatMap collectExprRecords args

    Opt.TailCall _ args ->
      concatMap (collectExprRecords . snd) args

    Opt.If branches final ->
      concatMap collectBranchRecords branches ++ collectExprRecords final

    Opt.Let def body ->
      collectDefRecords def ++ collectExprRecords body

    Opt.Destruct _ body ->
      collectExprRecords body

    Opt.Case _ _ _ jumps ->
      concatMap (collectExprRecords . snd) jumps

    Opt.Access record _ ->
      collectExprRecords record

    Opt.Update record fields ->
      collectExprRecords record ++ concatMap collectExprRecords (Map.elems fields)

    Opt.Record fields ->
      Map.keys fields : concatMap collectExprRecords (Map.elems fields)

    Opt.Tuple a b maybeC ->
      collectExprRecords a ++ collectExprRecords b ++ maybe [] collectExprRecords maybeC

    _ ->
      []
  where
    collectBranchRecords (condition, branch) =
      collectExprRecords condition ++ collectExprRecords branch


constructorName :: Int -> JsName.Name
constructorName index =
  JsName.fromLocal (Name.fromChars ("$$Record" ++ show index))


cloneName :: JsName.Name
cloneName =
  JsName.fromLocal "$c"


oldName :: JsName.Name
oldName =
  JsName.fromLocal "$old"


updatedName :: JsName.Name
updatedName =
  JsName.fromLocal "$updated"

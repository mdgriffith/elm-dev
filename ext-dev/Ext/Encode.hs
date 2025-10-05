{-# LANGUAGE OverloadedStrings #-}

module Ext.Encode (docs) where


import qualified AST.Utils.Binop as Binop

import qualified Data.Aeson
import qualified Data.ByteString.Builder
import qualified Data.Map as Map
import qualified Data.Text
import qualified Data.Vector

import qualified Data.Name as Name
import qualified Json.String
import qualified Json.Encode as ElmJson
import qualified Elm.Docs
import qualified Elm.Compiler.Type
import qualified Reporting.Doc
import qualified Reporting.Render.Type
import qualified Reporting.Render.Type.Localizer



docs :: Elm.Docs.Documentation -> Data.Aeson.Value
docs documentation =
  -- Elm.Docs.encode produces a JSON array of modules (Map.elems)
  Data.Aeson.toJSON (map encodeModule (Map.elems documentation))


encodeModule :: Elm.Docs.Module -> Data.Aeson.Value
encodeModule (Elm.Docs.Module name comment unions aliases values binops) =
  Data.Aeson.object
    [ ("name", Data.Aeson.toJSON (toModuleNameText name))
    , ("comment", Data.Aeson.toJSON (toCommentText comment))
    , ("unions", Data.Aeson.toJSON (map encodeUnion (Map.toList unions)))
    , ("aliases", Data.Aeson.toJSON (map encodeAlias (Map.toList aliases)))
    , ("values", Data.Aeson.toJSON (map encodeValue (Map.toList values)))
    , ("binops", Data.Aeson.toJSON (map encodeBinop (Map.toList binops)))
    ]


encodeUnion :: (Name.Name, Elm.Docs.Union) -> Data.Aeson.Value
encodeUnion (name, Elm.Docs.Union comment args cases) =
  Data.Aeson.object
    [ ("name", Data.Aeson.toJSON (toNameText name))
    , ("comment", Data.Aeson.toJSON (toCommentText comment))
    , ("args", Data.Aeson.toJSON (map toNameText args))
    , ("cases", Data.Aeson.toJSON (map encodeCase cases))
    ]


encodeCase :: (Name.Name, [Elm.Compiler.Type.Type]) -> Data.Aeson.Value
encodeCase (tag, args) =
  -- Elm.Docs.encodeCase encodes as a 2-element JSON array: [ tag, [ types... ] ]
  Data.Aeson.toJSON
    [ Data.Aeson.toJSON (toNameText tag)
    , Data.Aeson.toJSON (map encodeType args)
    ]


encodeAlias :: (Name.Name, Elm.Docs.Alias) -> Data.Aeson.Value
encodeAlias (name, Elm.Docs.Alias comment args tipe) =
  Data.Aeson.object
    [ ("name", Data.Aeson.toJSON (toNameText name))
    , ("comment", Data.Aeson.toJSON (toCommentText comment))
    , ("args", Data.Aeson.toJSON (map toNameText args))
    , ("type", encodeType tipe)
    ]


encodeValue :: (Name.Name, Elm.Docs.Value) -> Data.Aeson.Value
encodeValue (name, Elm.Docs.Value comment tipe) =
  Data.Aeson.object
    [ ("name", Data.Aeson.toJSON (toNameText name))
    , ("comment", Data.Aeson.toJSON (toCommentText comment))
    , ("type", encodeType tipe)
    ]


encodeBinop :: (Name.Name, Elm.Docs.Binop) -> Data.Aeson.Value
encodeBinop (name, Elm.Docs.Binop comment tipe assoc prec) =
  Data.Aeson.object
    [ ("name", Data.Aeson.toJSON (toNameText name))
    , ("comment", Data.Aeson.toJSON (toCommentText comment))
    , ("type", encodeType tipe)
    , ("associativity", Data.Aeson.toJSON (encodeAssoc assoc))
    , ("precedence", Data.Aeson.toJSON (encodePrecedence prec))
    ]


encodeAssoc :: Binop.Associativity -> Data.Text.Text
encodeAssoc assoc =
  case assoc of
    Binop.Left -> Data.Text.pack "left"
    Binop.Non -> Data.Text.pack "non"
    Binop.Right -> Data.Text.pack "right"


encodePrecedence :: Binop.Precedence -> Int
encodePrecedence (Binop.Precedence n) = n


encodeType :: Elm.Compiler.Type.Type -> Data.Aeson.Value
encodeType tipe =
    Data.Aeson.toJSON 
        (Reporting.Doc.toLine (Elm.Compiler.Type.toDoc Reporting.Render.Type.Localizer.empty Reporting.Render.Type.None tipe))


toModuleNameText :: Name.Name -> Data.Text.Text
toModuleNameText = Data.Text.pack . Name.toChars


toNameText :: Name.Name -> Data.Text.Text
toNameText = Data.Text.pack . Name.toChars


toCommentText :: Json.String.String -> Data.Text.Text
toCommentText = Data.Text.pack . Json.String.toChars


-- elmJsonToAeson :: ElmJson.Value -> Data.Aeson.Value
-- elmJsonToAeson v =
--   let lbs = Data.ByteString.Builder.toLazyByteString (ElmJson.encodeUgly v)
--   in case Data.Aeson.eitherDecode lbs of
--        Right val -> val
--        Left _ -> Data.Aeson.object [ ("error", Data.Aeson.toJSON (Data.Text.pack "failed_to_decode_elm_json")) ]
    
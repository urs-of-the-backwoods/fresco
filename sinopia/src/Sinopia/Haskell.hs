--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2017 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: src/Sinopia/Haskell.hs
--

{-# LANGUAGE OverloadedStrings #-}

module Sinopia.Haskell 
(
    hConvertible
) where

import Sinopia.Data
import Sinopia.Parser
import Sinopia.Util

import qualified Data.List as L
import qualified Data.Text as T
import Data.Monoid

import Numeric

-- create output for Haskell

tN' tlt = cap1 (typeName tlt)

structElemN' :: TypeName -> FieldName -> T.Text
structElemN' n fn = low1 n <> cap1 fn

enumElemN' :: TypeName -> FieldName -> T.Text
enumElemN' n fn = cap1 fn

bT' = \t -> case t of
    BT_PT PT_Null -> "()"    
    BT_PT PT_Bool -> "Bool"    
    BT_PT PT_Int8 -> "Int8"
    BT_PT PT_Int16 -> "Int16"
    BT_PT PT_Int32 -> "Int"
    BT_PT PT_Int64 -> "Int64"
    BT_PT PT_UInt8 -> "Word8"
    BT_PT PT_UInt16 -> "Word16"
    BT_PT PT_UInt32 -> "Word32"
    BT_PT PT_UInt64 -> "Word64"
    BT_PT PT_Float32 -> "Float"
    BT_PT PT_Float64 -> "Double"
    BT_PT PT_Text -> "Text"
    BT_PT PT_Data -> "ByteString"
    BT_LT a -> "[" <> (bT' a) <> "]"
    BT_TN n -> n
--    _ -> error "Unknown Base Type in hBType!"

typeDef' :: Bool -> TopLevelType -> T.Text
typeDef' _ t = case t of
    TL_ET e@(EnumType en fs) -> "data " <> tN' t <> " = " <> enumFields en fs <> "    deriving (Eq, Read, Show)\n\n"
    TL_ST s@(StructType sn fs) -> "data " <> tN' t <> " = " <> tN' t <> " {\n    " <> structFields sn fs <> "    } deriving (Eq, Read, Show)\n\n"
    TL_ID (Id64 tn i) -> "ct" <> tN' t <> " :: ComponentType " <> tN' t <> "\n"
                         <> "ct" <> tN' t <> " = ComponentType 0x" <> (T.pack (showHex i "")) <> "\n\n"
    TL_TD (TypeDeclaration tn bt) -> "type " <> tN' t <> " = " <> bT' bt <> "\n\n"
    _ -> ""
    where
      enumFields en fs = T.concat (L.intersperse ("\n    | ") (map (\ef@(EnumField cn bts) -> 
        enumElemN' en cn <> (foldl (\b bt -> b <> " " <> (bT' bt)) "" bts)) fs)) <> "\n"
      structFields sn fs = T.concat (L.intersperse (",\n    ") (map (\sf@(StructField en bt) ->
        structElemN' sn en <> "::" <> bT' bt) fs)) <> "\n"

serDef' _ t = case t of
    TL_ET e@(EnumType tn fs) -> "instance Serialise " <> tN' t <> " where\n" <>
                            eList tn fs <>
                            dList tn fs
                            where
                                eList tn fs = T.concat (L.map fEncode (zip fs [0..]))
                                fEncode ((EnumField n ts), i) = "    encode (" <> enumElemN' tn n <> tsEncodeL ts <>") = " <> arrRList ts <> " encode (" <> (T.pack . show) i <> "::Int) " <> tsEncodeR ts <> "\n"
                                tsEncodeL ts = T.concat (map (\t -> " v" <> (T.pack . show) t) [1 .. length ts])
                                arrRList ts = "encodeListLen " <> ((T.pack . show) ((length ts) + 1)) <> " <> " 
                                tsEncodeR ts = T.concat (map (\t -> "<> encode v" <> (T.pack . show) t) [1 .. length ts])
                                dList tn fs = decodeHead <> T.concat (L.map fDecode (zip fs [0..])) <> "\n"
                                decodeHead = "    decode = do\n        decodeListLen\n        i <- decode :: Decoder s Int\n        case i of\n"
                                fDecode ((EnumField n ts), i) = "            " <> ((T.pack .show) i)<> " -> (" <> decodeR n ts <> ")\n"
                                decodeR n ts = if length ts == 0 
                                                    then "pure " <> enumElemN' tn n
                                                    else enumElemN' tn n <> " <$> " <> T.concat (L.intersperse " <*> " (map (const "decode") [1..length ts]))

    TL_ST s@(StructType _ fs) -> "instance Serialise " <> tN' t <> " where\n" <>
                            "    encode (" <> tN' t <> vLList <> ") = " <> arrRList <> vRList <> "\n" <>
                            "    decode = decodeListLenOf " <> ((T.pack . show) (length fs)) <> " >> " <> tN' t <> " <$> " <> dList <> "\n\n"
                            where
                                arrRList = "encodeListLen " <> ((T.pack . show) (length fs)) <> " <> "
                                nList = [1 .. length fs]
                                vLList = T.concat (L.map (\n -> " v" <> ((T.pack . show) n)) nList)  
                                vRList = T.concat (L.intersperse " <> " (map (\n -> "encode v" <> ((T.pack . show) n)) nList))
                                dList = T.concat (L.intersperse " <*> " (map (\n -> "decode") nList) )                  

                        
    _ -> ""

nsEnd' = ""

headDef' _ f t = let 
    fi l = filter (\v -> case v of 
                        TL_NS _ -> True
                        _ -> False) l 
    mod = case fi t of
            ( TL_NS (Namespace n) : _) -> "module " <> n <> "\nwhere\n\n"
            [] -> ""
    in
        mod <>
        "import Fresco\n" <>
        "import Data.Binary.Serialise.CBOR\n" <>
        "import Data.Binary.Serialise.CBOR.Encoding\n" <>
        "import Data.Binary.Serialise.CBOR.Decoding\n\n" <>

        "import Data.Text\n" <>
        "import Data.Monoid\n" <>
        "import Control.Applicative\n\n"

footDef' _ _ = ""

ctDef' :: Bool -> TopLevelType -> T.Text
ctDef' _ _ = ""

hConvertible = Convertible tN' structElemN' enumElemN' bT' (headDef' False) (typeDef' False) (serDef' False) nsEnd' (ctDef' False) (footDef' False) 


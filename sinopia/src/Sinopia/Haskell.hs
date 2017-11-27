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
  writeHaskellFile
) where

import Sinopia.Data
import Sinopia.Parser
import Sinopia.Util

import qualified Data.List as L
import qualified Data.Text as T
import Data.Monoid

import Numeric

-- create output for Haskell

-- haskell names of basetypes
btToHt :: BaseType -> T.Text
btToHt = \t -> case t of
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
    BT_LT a -> "[" <> (btToHt a) <> "]"
    BT_TN n -> n

--  haskell typedeclarations 
tdToHt :: TypeDeclaration -> T.Text
tdToHt td = case td of

  TD_TD (TypeDefinition tn bt mbCmt) -> cmt mbCmt <> "type " <> cap1 tn <> " = " <> btToHt bt <> "\n\n"
  TD_ST (StructType tn sfs mbCmt) -> cmt mbCmt <> "data " <> cap1 tn <> " = " <> cap1 tn <> " {\n    " <> structFields tn sfs <> "    } deriving (Eq, Read, Show)\n\n"
  TD_ET (EnumType tn efs mbCmt) -> cmt mbCmt <> "data " <> cap1 tn <> " = " <> enumFields efs <> "\n    deriving (Eq, Read, Show)\n\n"

  where
    cmt c = case c of 
      Nothing -> ""
      Just (t : ts) -> "-- | " <> t <> "\n" <> (T.concat (map (\t' -> "-- " <> t' <> "\n") ts))
    cmt'' c = case c of 
      Nothing -> ""
      Just (t : ts) -> " -- ^ " <> t <> " " <> (T.concat (map (\t' -> t' <> " ") ts))
    cmt' c = let
        c' = cmt c
        in if T.length c' > 0 
            then c' <> "    "
            else ""
    enumFields fs = T.concat (L.intersperse ("\n    | ") (map (\ef@(EnumField fn bts c) -> 
        cap1 fn <> (foldl (\b bt -> b <> " " <> (btToHt bt) ) "" bts) <> cmt'' c) fs))
    structFields tn fs = T.concat (L.intersperse (",\n    ") (map (\sf@(StructField fn bt c) ->
        cmt' c <> low1 tn <> cap1 fn <> "::" <> btToHt bt) fs)) <> "\n"

-- haskell statements
stToHt :: Statement -> T.Text
stToHt st = case st of
  ST_TD td -> tdToHt td
  ST_ID (Id64 tn i) ->  "ct" <> name <> " :: ComponentType " <> name <> "\n"
                         <> "ct" <> name <> " = ComponentType 0x" <> (T.pack (showHex i "")) <> "\n\n"
                         where name = cap1 tn
  ST_IM im -> "" -- to be done


-- serialization definitions
haskellSerializer :: Statement -> T.Text
haskellSerializer td = case td of

    ST_TD (TD_ET e@(EnumType tn fs _)) -> "instance Serialise " <> cap1 tn <> " where\n" <>
                            eList tn fs <>
                            dList tn fs
                            where
                                eList tn fs = T.concat (L.map fEncode (zip fs [0..]))
                                fEncode ((EnumField n ts _), i) = "    encode (" <> cap1 n <> tsEncodeL ts <>") = " <> arrRList ts <> " encode (" <> (T.pack . show) i <> "::Int) " <> tsEncodeR ts <> "\n"
                                tsEncodeL ts = T.concat (map (\t -> " v" <> (T.pack . show) t) [1 .. length ts])
                                arrRList ts = "encodeListLen " <> ((T.pack . show) ((length ts) + 1)) <> " <> " 
                                tsEncodeR ts = T.concat (map (\t -> "<> encode v" <> (T.pack . show) t) [1 .. length ts])
                                dList tn fs = decodeHead <> T.concat (L.map fDecode (zip fs [0..])) <> "\n"
                                decodeHead = "    decode = do\n        decodeListLen\n        i <- decode :: Decoder s Int\n        case i of\n"
                                fDecode ((EnumField n ts _), i) = "            " <> ((T.pack .show) i)<> " -> (" <> decodeR n ts <> ")\n"
                                decodeR n ts = if length ts == 0 
                                                    then "pure " <> cap1 n
                                                    else cap1 n <> " <$> " <> T.concat (L.intersperse " <*> " (map (const "decode") [1..length ts]))

    ST_TD (TD_ST s@(StructType tn fs _)) -> "instance Serialise " <> cap1 tn <> " where\n" <>
                            "    encode (" <> cap1 tn <> vLList <> ") = " <> arrRList <> vRList <> "\n" <>
                            "    decode = decodeListLenOf " <> ((T.pack . show) (length fs)) <> " >> " <> cap1 tn <> " <$> " <> dList <> "\n\n"
                            where
                                arrRList = "encodeListLen " <> ((T.pack . show) (length fs)) <> " <> "
                                nList = [1 .. length fs]
                                vLList = T.concat (L.map (\n -> " v" <> ((T.pack . show) n)) nList)  
                                vRList = T.concat (L.intersperse " <> " (map (\n -> "encode v" <> ((T.pack . show) n)) nList))
                                dList = T.concat (L.intersperse " <*> " (map (\n -> "decode") nList) )                  

                        
    _ -> ""


-- Haskell header content
haskellHeader :: T.Text -> T.Text -> T.Text
haskellHeader f m = let
    mod = if T.length m > 0 && T.length f > 0
        then "module " <> m <> "." <> f <> "\nwhere\n\n"
        else ""
    in
        mod <>
        "import Data.Binary.Serialise.CBOR\n" <>
        "import Data.Binary.Serialise.CBOR.Encoding\n" <>
        "import Data.Binary.Serialise.CBOR.Decoding\n\n" <>

        "import Data.Text\n" <>
        "import Data.Monoid\n" <>
        "import Control.Applicative\n\n"


writeHaskellFile :: T.Text -> T.Text -> [Statement] -> T.Text
writeHaskellFile fname mname sts =
    haskellHeader fname mname <> 
    "\n" <>
    (T.concat (
      (map stToHt sts) ++
      (map haskellSerializer sts)
    ))
  

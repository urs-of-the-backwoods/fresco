--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2017 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: src/Sinopia/JavaScript.hs
--

{-# LANGUAGE OverloadedStrings #-}

module Sinopia.JavaScript
(
  writeJavaScriptFile
) where

import qualified Data.Text as T
import Sinopia.Data
import Sinopia.Util
import Data.Monoid

-- header, footer, ...
jsHeader :: T.Text
jsHeader = "import CBOR from '../other_libs/cbor';\n\n"

-- struct class
jsBaseStructClass ::  T.Text
jsBaseStructClass = T.concat [
   "class CborStructItem {\n",
   "\n",
   "    constructor(...args) {\n",
   "       this.setValue(...args);\n",
   "    }\n",
   "\n",
   "    setValue(...args) {\n",
   "       this.record = args;\n",
   "    }\n",
   "\n",
   "    fromCBOR (ser_data) {\n",
   "       var json_data = CBOR.decode(ser_data);\n",
   "       this.fromData(json_data);\n",
   "       return this;\n",
   "    }\n",
   "\n",
   "    toCBOR () {\n",
   "       var data = this.toData();\n",
   "       return CBOR.encode(data);\n",
   "    }\n",
   "\n",
   "    toData () {\n",
   "       return this.record;\n",
   "    }\n",
   "\n",
   "    fromData (json_data) {\n",
   "       this.setValue(...json_data);\n",
   "       return this;\n",
   "    }\n",
   "}\n",
    "\n"
   ]

-- enum class
jsBaseEnumClass ::  T.Text
jsBaseEnumClass = T.concat [
   "class CborEnumItem extends CborStructItem {\n",
   "    setValue(...args) {\n",
   "       if (args.length > 0) {\n",
   "         this.selector = args[0];\n",
   "         this.record = args.slice(1);\n",
   "       }\n",
   "    }\n",
   "\n",
   "    toData () {\n",
   "       return [this.selector, ...this.record];\n",
   "    }\n",
   "}\n",
   "\n"
   ]

-- class for enums
jsStructClass :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text
jsStructClass cname recordFunctions toData fromData = T.concat [
   "class " <> cname <> " extends CborStructItem {\n",
   recordFunctions,
   toData,
   fromData,
   "\n"
   ]

-- class for enums
jsEnumClass :: T.Text -> T.Text -> T.Text -> T.Text
jsEnumClass cname toData fromData = T.concat [
   "class " <> cname <> " extends CborEnumItem {\n",
   toData,
   fromData,
   "}\n",
   "\n"
   ]

-- check if basetype is not pure JSON
checkBt :: BaseType -> Bool
checkBt bt = case bt of
  BT_PT _ -> False
  BT_LT a -> True 
  BT_TN _ -> True

 --- check if we have non-Json fields in data structure (including sub-structures)
jsStructCheckTransformNeeded :: [StructField] -> Bool
jsStructCheckTransformNeeded fs = let
  checkField (StructField _ bt _) = checkBt bt
  in foldl (\a b -> a || checkField b) False fs
 
---- check if we have non-Json fields in data structure (including sub-structures)
jsEnumCheckTransformNeeded :: [EnumField] -> Bool
jsEnumCheckTransformNeeded fs = let
  checkField (EnumField _ bts _) = foldl (\a b -> a  || checkBt b) False bts
  in foldl (\a b -> a || checkField b) False fs

transBt :: BaseType -> T.Text -> Bool -> T.Text
transBt bt toDataOrFromData boolSpace = case bt of
    BT_PT _ -> addSpace boolSpace <> "       arr_out.push(arr_in.shift());\n"
    BT_TN cname -> addSpace boolSpace <> if (toDataOrFromData == "toData")
                 then 
                   "       arr_out.push(arr_in.shift().toData());\n"
                 else
                   "       arr_out.push((new " <> cap1 cname <> "()).fromData(arr_in.shift()));\n"
    BT_LT a -> addSpace boolSpace <> "       arr_out.push(\n" <>
               addSpace boolSpace <> "         arr_in.shift().map(function (a) { var arr_in = [a]; var arr_out = [];\n"
                                  <> "  " <> transBt a toDataOrFromData boolSpace <>
               addSpace boolSpace <> "         return arr_out[0]; }) \n" <>
               addSpace boolSpace <> "       );\n"
    where addSpace b = if b then "     " else ""
   
-- to data, check if needed, then write code to transform to pure json data
jsStructToData :: [StructField] -> T.Text
jsStructToData fields = let
  transF (StructField fn bt _) = transBt bt "toData" False <> "\n"
  transform fs = foldl (\a f -> a <> transF f) "" fs
  wrap d = "    toData () {\n" <>
           "       var arr_in = this.record.slice(); var arr_out = [];\n" <>
           d <>
           "       return arr_out;\n" <>
           "    }\n\n"
  in if jsStructCheckTransformNeeded fields then wrap (transform fields) else "" 

-- to data, check if needed, then write code to transform to pure json data
jsEnumToData :: [EnumField] -> T.Text
jsEnumToData fields = let
  transF (EnumField fn bts _) n = "       if (this.selector == " <> T.pack (show n) <> ") {\n" <> (foldl (\a b -> a <> transBt b "toData" True) "" bts) <> "       }\n"
  transform fs = foldl (\a (f, n) -> a <> transF f n) "" (zip fs [0..])
  wrap d = "    toData () {\n" <>
           "       var arr_in = this.record.slice(); var arr_out = [];\n" <>
           d <>
           "       return [this.selector, ...arr_out];\n" <>
           "    }\n\n"
  in if jsEnumCheckTransformNeeded fields then wrap (transform fields) else "" 

-- from data, check if needed, then write code to transform from pure json data
jsStructFromData :: [StructField] -> T.Text
jsStructFromData fields = let
  transF (StructField fn bt _) = transBt bt "fromData" False <> "\n"
  -- write transformation for all fields, but make sure, only the right one is used
  transform fs = foldl (\a f -> a <> transF f) "" fs
  wrap d = "    fromData (json_data) {\n" <>
           "      var arr_in = json_data.slice(); var arr_out = [];\n" <>
           d <>
           "      this.record = arr_out;\n" <>
           "      return this;\n" <>
           "    }\n"
           
  -- finally do it
  in if jsStructCheckTransformNeeded fields then wrap (transform fields) else "" 

-- from data, check if needed, then write code to transform from pure json data
jsEnumFromData :: [EnumField] -> T.Text
jsEnumFromData fields = let
  transF (EnumField fn bts _) n = "       if (json_data[0] == " <> T.pack (show n) <> ") {\n" <> (foldl (\a b -> a <> transBt b "fromData" True) "" bts) <> "       }\n"
  -- write transformation for all fields, but make sure, only the right one is used
  transform fs = foldl (\a (f, n) -> a <> transF f n) "" (zip fs [0..])
  wrap d = "     fromData (json_data) {\n" <>
           "       var arr_in = json_data.slice(1); var arr_out = [];\n" <>
           d <>
           "       this.selector = json_data[0];\n" <>
           "       this.record = arr_out;\n" <>
           "       return this;\n" <>
           "     }\n"
           
  -- finally do it
  in if jsEnumCheckTransformNeeded fields then wrap (transform fields) else "" 

-- record functions for structs
jsRecordFunctions :: [StructField] -> T.Text
jsRecordFunctions fields = let
  recordField (StructField fn bt mbC) n = T.concat [
    "    set " <> cap1 fn <> " (v) { self.record[" <> T.pack (show n) <> "] = v; }\n",
    "    get " <> cap1 fn <> " { return self.record[" <> T.pack (show n) <> "]; }\n",
    "\n" ]
  in foldl (\a (b, n) -> a <> recordField b n) "" (zip fields [0..])

-- selector for enums
jsEnumSelector :: T.Text -> [EnumField] -> T.Text
jsEnumSelector cname fields = T.concat $ map (\(EnumField fn _ mbC, n) -> 
  ( cname <> "." <> cap1 fn <> " = " <> (T.pack (show n)) <> ";" <> 
    (case mbC of
       Just c -> "   // " <> T.concat c
       Nothing -> ""                 )  <> "\n" ) )
  (zip fields [0..])

topCmts :: Maybe [T.Text] -> T.Text
topCmts mbC = case mbC of
  Nothing -> ""
  Just cmts -> T.concat (map (\c -> "// " <> c <> "\n") cmts)
  
writeJavaScriptFile :: T.Text -> T.Text -> [Statement] -> T.Text
writeJavaScriptFile fname mname sts = jsHeader <> jsBaseStructClass <> jsBaseEnumClass <> T.concat (map f sts) where
  f s = case s of
    -- write enum types
    ST_TD (TD_ET (EnumType tn fields mbC) ) ->
      topCmts mbC <>
      jsEnumClass (cap1 tn) (jsEnumToData fields) (jsEnumFromData fields) <>
      jsEnumSelector (cap1 tn) fields <>
      "\n"
    -- write struct types
    ST_TD (TD_ST (StructType tn fields mbC) ) ->
      topCmts mbC <>
      jsStructClass (cap1 tn) (jsRecordFunctions fields) (jsStructToData fields) (jsStructFromData fields) <>
      "\n"
    -- to be done: add comments everywhere, add imports, ids and other types
    _ -> ""


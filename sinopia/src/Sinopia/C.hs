--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2017 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: src/Sinopia/C.hs
--

{-# LANGUAGE OverloadedStrings #-}

module Sinopia.C 
(
  writeHppFile,
  writeCppFile
)
where 

import Sinopia.Data
import Sinopia.Parser
import Sinopia.Util

import qualified Data.List as L
import qualified Data.Text as T
import Data.Monoid

import Numeric

-- create output for C and C++

-- helper type names in C for basetypes
btToCt :: BaseType -> T.Text
btToCt bt = case bt of
    BT_PT PT_Null -> "struct {}"
    BT_PT PT_Bool -> "bool"    
    BT_PT PT_Int8 -> "int8_t"
    BT_PT PT_Int16 -> "int16_t"
    BT_PT PT_Int32 -> "int32_t"
    BT_PT PT_Int64 -> "int64_t"
    BT_PT PT_UInt8 -> "uint8_t"
    BT_PT PT_UInt16 -> "uint16_t"
    BT_PT PT_UInt32 -> "uint32_t"
    BT_PT PT_UInt64 -> "uint64_t"
    BT_PT PT_Float32 -> "float"
    BT_PT PT_Float64 -> "double"
    BT_PT PT_Text -> "std::string"
    BT_PT PT_Data -> "std::vector<uint8_t>"
    BT_LT a -> "std::vector<" <> (btToCt a) <> ">"
    BT_TN n -> n

-- type declaration in C
tdToCt :: TypeDeclaration -> T.Text
tdToCt td = case td of
    TD_ET (EnumType tn fs _) -> enumEnumDef tn fs <> enumStructDef tn fs
        where
            enumEnumDef tn fs = "typedef enum {\n" <> enumEnumFields fs <> "} Enum" <> cap1 tn <> ";\n\n"
            enumEnumFields fs = T.concat (L.map enumEnumField (zip fs [0..]))
            enumEnumField (EnumField fn ts _, i) = "    " <> cap1 fn <> " = " <> (T.pack (show i)) <> ",\n"

            enumStructDef tn fs = "typedef struct {\n    " <> "Enum" <> cap1 tn <> " selector;\n" <> enumStructUnion fs <> "} " <> cap1 tn <> ";\n\n"
            enumStructUnion fs = "    struct {\n" <> enumStructFields fs <> "    } data;\n"
            enumStructFields fs = T.concat (L.map enumStructField fs)
            enumStructField (EnumField fn bts _) = "        struct {\n" <> esfTypes bts <> "        } " <> fn <>";\n"
            esfTypes bts = T.concat (L.map esfType (zip [0..] bts))
            esfType (i, bt) = "            " <> btToCt bt <> " value" <> (T.pack (show i)) <> ";\n"

    TD_ST (StructType tn fs _) -> "typedef struct {\n" <> structStructFields fs <> "} " <> cap1 tn <> ";\n\n"
        where
            structStructFields fs = T.concat (L.map structStructField fs)
            structStructField (StructField fn bt _) = "    " <> btToCt bt <> " " <> fn <> ";\n"

    TD_TD (TypeDefinition tn bt _) -> "typedef " <> btToCt bt <> " " <> cap1 tn <> ";\n\n"
    

 -- statements in C - type definitions, id64 definition, header
stTdsHeader :: Statement -> T.Text
stTdsHeader st = case st of
    ST_TD td -> tdToCt td
    _ -> ""

 -- statements in C - id64
stId64 :: Statement -> T.Text
stId64 st = case st of
    ST_ID (Id64 tn i) -> "const uint64_t ct" <> cap1 tn <> " = 0x" <> (T.pack (showHex i "")) <> ";\n"
    _ -> ""

-- statements in C - id64 - Header Only
stId64Header :: Statement -> T.Text
stId64Header st = case st of
   ST_ID (Id64 tn i) -> "extern const uint64_t ct" <> cap1 tn <> ";\n"
   _ -> ""


-- serializer for C
  
readEnterContainer = "{ CborValue itb; CborValue *ita = it; CborValue *it = &itb; cbor_value_enter_container(ita, it);\n" 
readLeaveContainer = "cbor_value_leave_container(ita, it); }\n"
writeEnterContainer' l = "{ CborEncoder encb; CborEncoder *enca = enc; CborEncoder *enc = &encb; cbor_encoder_create_array(enca, enc, " <> l <> ");\n"
writeEnterContainer l = writeEnterContainer' (T.pack (show l)) 
writeLeaveContainer = "cbor_encoder_close_container_checked(enca, enc); }\n"

getReaderDef :: BaseType -> (T.Text -> T.Text)
getReaderDef bt = case bt of
    BT_PT PT_Null -> \t -> "{ uint8_t i; cbor_value_get_simple_type(it, &i);} cbor_value_advance_fixed(it);\n"
    BT_PT PT_Bool -> \t -> "cbor_value_get_boolean(it, &(" <> t <> ")); cbor_value_advance_fixed(it);\n"
    BT_PT PT_Int8 -> \t -> "{ int i; cbor_value_get_int(it, &i); " <> t <> " = (int8_t)i;} cbor_value_advance_fixed(it);\n"
    BT_PT PT_Int16 -> \t -> "{ int i; cbor_value_get_int(it, &i); " <> t <> " = (int16_t)i;} cbor_value_advance_fixed(it);\n"
    BT_PT PT_Int32 -> \t -> "{ int i; cbor_value_get_int(it, &i); " <> t <> " = (int32_t)i;} cbor_value_advance_fixed(it);\n"
    BT_PT PT_Int64 -> \t -> "cbor_value_get_int64(it, &(" <> t <> ")); cbor_value_advance_fixed(it);\n"
    BT_PT PT_UInt8 -> \t -> "{ int i; cbor_value_get_int(it, &i); " <> t <> " = (uint8_t)i;} cbor_value_advance_fixed(it);\n"
    BT_PT PT_UInt16 -> \t -> "{ int i; cbor_value_get_int(it, &i); " <> t <> " = (uint16_t)i;} cbor_value_advance_fixed(it);\n"
    BT_PT PT_UInt32 -> \t -> "{ uint64_t i; cbor_value_get_uint64(it, &i); " <> t <> " = (uint32_t)i;} cbor_value_advance_fixed(it);\n"
    BT_PT PT_UInt64 -> \t -> "cbor_value_get_uint64(it, &(" <> t <> ")); cbor_value_advance_fixed(it);\n"
    BT_PT PT_Float32 -> \t -> "cbor_value_get_float(it, &(" <> t <> ")); cbor_value_advance_fixed(it);\n"
    BT_PT PT_Float64 -> \t -> "cbor_value_get_double(it, &(" <> t <> ")); cbor_value_advance_fixed(it);\n"
    BT_PT PT_Text -> \t -> "{ size_t l; cbor_value_calculate_string_length(it, &l); " <> t <> ".resize(l+1);\n" <>
        "        cbor_value_copy_text_string(it, (char *)(" <> t <> ".c_str()), &l, NULL); cbor_value_advance(it);}\n"

    BT_PT PT_Data ->  \t -> "{ size_t l; cbor_value_get_array_length(it, &l); " <> t <> ".resize(l+1);\n" <>
        "        cbor_value_copy_byte_string(it, " <> t <> ".data(), &l, NULL); cbor_value_advance(it);}\n"

    BT_LT a -> \t -> readEnterContainer <>
                     "  size_t l; cbor_value_get_array_length(it, &l); \n" <>
                     "  " <> t <> ".clear();\n" <>
                     "  while (!cbor_value_at_end(it)) { " <> 
                     "  " <> btToCt a <> " item; " <> getReaderDef a "item" <> "; " <> t <> ".push_back(item); } \n" <>  
                     readLeaveContainer

    BT_TN n -> \t -> "read" <> n <> "(it, &(" <> t <> "));\n"

getWriterDef :: BaseType -> (T.Text -> T.Text)
getWriterDef bt = case bt of
    BT_PT PT_Null -> \t -> "cbor_encode_simple_value(enc, NullValue);\n"
    BT_PT PT_Bool -> \t -> "cbor_encode_boolean(enc, " <> t <> ");\n"
    BT_PT PT_Int8 -> \t -> "cbor_encode_int(enc, (int64_t)" <> t <> ");\n"
    BT_PT PT_Int16 -> \t -> "cbor_encode_int(enc, (int64_t)" <> t <> ");\n"
    BT_PT PT_Int32 -> \t -> "cbor_encode_int(enc, (int64_t)" <> t <> ");\n"
    BT_PT PT_Int64 -> \t -> "cbor_encode_int(enc, " <> t <> ");\n"
    BT_PT PT_UInt8 -> \t -> "cbor_encode_uint(enc, (uint64_t)" <> t <> ");\n"
    BT_PT PT_UInt16 -> \t -> "cbor_encode_uint(enc, (uint64_t)" <> t <> ");\n"
    BT_PT PT_UInt32 -> \t -> "cbor_encode_uint(enc, (uint64_t)" <> t <> ");\n"
    BT_PT PT_UInt64 -> \t -> "cbor_encode_uint(enc, " <> t <> ");\n"
    BT_PT PT_Float32 -> \t -> "cbor_encode_float(enc, " <> t <> ");\n"
    BT_PT PT_Float64 -> \t -> "cbor_encode_double(enc, " <> t <> ");\n"
    BT_PT PT_Text ->  \t -> "cbor_encode_text_stringz(enc, " <> t <> ".c_str());\n"
    BT_PT PT_Data ->   \t -> "cbor_encode_byte_string(enc, " <> t <> ".data(), " <> t <> ".size());\n"
    BT_LT a -> \t -> "{ size_t l; l = " <> t <> ".size(); \n" <>
                     writeEnterContainer' "l" <>
                     "for (int i = 0; i < l; i++) {" <> 
                     getWriterDef a (t <> "[i]")  <> "; }" <>
                     writeLeaveContainer <> "}\n"
    BT_TN n -> \t -> "write" <> n <> "(enc, " <> t <> ");\n"
    
cSerializer :: Bool -> Statement -> T.Text
cSerializer defOnly st = let

    readFunctionHead tn = "void read" <> cap1 tn <> "(CborValue *it, " <> cap1 tn <> " *" <> low1 tn <> ")"
    writeFunctionHead tn = "void write" <> cap1 tn <> "(CborEncoder *enc, " <> cap1 tn <> " " <> low1 tn <> ")"
    functionHeaders tn = readFunctionHead tn <> ";\n" <> writeFunctionHead tn <> ";\n" 
    readFunction tn fbody = readFunctionHead tn <> " {\n" <> fbody <> "}\n\n"
    writeFunction tn fbody = writeFunctionHead tn <> " {\n" <> fbody <> "}\n\n"
    functions tn readBody writeBody = readFunction tn readBody <> writeFunction tn writeBody

    in 
        if defOnly
            then case st of
                ST_TD (TD_ET (EnumType tn _ _)) -> functionHeaders tn
                ST_TD (TD_ST (StructType tn _ _)) -> functionHeaders tn
                ST_TD (TD_TD (TypeDefinition tn bt _)) -> functionHeaders tn
                _ -> ""

            else case st of
                ST_TD(TD_ET (EnumType tn fs _)) -> functions tn readBody writeBody where
                    readBody = readEnterContainer <> readSelector <> readFElems <> readLeaveContainer
                    readSelector = "int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);\n    " 
                                   <> low1 tn <> "->selector = (Enum" <> cap1 tn <> ")i;\n"
                    readFElems = T.concat (map readFElem (zip [0..] fs))
                    readFElem (i, (EnumField fn bts _)) = "    if (" <> low1 tn <> "->selector == " <> (T.pack (show i)) <> ") {\n" <> T.concat (map (readFBt fn) (zip [0..] bts)) <> "    };\n"
                    readFBt tn (n, bt) =  "        " <> ((getReaderDef bt) (low1 tn <> "->data." <> tn <> ".value" <> (T.pack (show n))))


                    writeBody = T.concat (map writeFElem (zip [0..] fs))
                    writeFElem (i, (EnumField fn bts _)) = writeFElemHead i <> writeEnterContainer ((length bts) + 1) <> writeFElemNum (low1 tn) <> 
                                                           writeFElemFBs fn bts <> writeLeaveContainer 
                    writeFElemHead i = "    if (" <> low1 tn <> ".selector == " <> (T.pack (show i)) <> ") \n"
                    writeFElemNum sN = "        cbor_encode_uint(enc, (uint64_t)" <> low1 tn <> ".selector);\n"
                    writeFElemFBs fn bts = T.concat (map (writeFBt fn) (zip [0..] bts))
                    writeFBt fn (n, bt) =  "        " <> ((getWriterDef bt) (low1 tn <> ".data." <> fn <> ".value" <> (T.pack (show n))))

                ST_TD(TD_ST (StructType tn fs _)) -> functions tn readBody writeBody where
                    readBody = readEnterContainer <> readFElems <> readLeaveContainer
                    readFElems = T.concat (map readFElem fs)
                    readFElem (StructField fn bt _) = "    " <> ((getReaderDef bt) (low1 tn <> "->" <> fn))

                    writeBody = writeEnterContainer (length fs) <> writeFElems <> writeLeaveContainer
                    writeFElems = T.concat (map writeFElem fs)
                    writeFElem (StructField fn bt _) = writeFBt fn bt
                    writeFBt fn bt =  "    " <> ((getWriterDef bt) (low1 tn <> "." <> fn ))

                ST_TD(TD_TD (TypeDefinition tn bt _)) ->  readFunction tn readBody <> writeFunction tn writeBody where
                    readBody = "    " <> btToCt bt <> " rval;\n    " <> ((getReaderDef bt) "rval") <> "    *" <> low1 tn <> " = rval;\n"
                    writeBody =  "    " <> ((getWriterDef bt) (low1 tn))
                _ -> ""


-- header and footer
otherIncludes sts = (T.concat (map imF sts)) where
  imF st = case st of
    ST_IM (Import n _) -> "#include \"" <> n <> "Cbor.hpp\"\n"
    _ -> ""

includeHead fname = T.concat ["#ifndef __", fname, "_cbor__\n#define __", fname, "_cbor__\n\n"]

addNamespace = "namespace cbd {\n"
namespaceEnd = "\n} // end of namespacd cdb\n"

includes = 
    "#include <stdint.h>\n" <>
    "#include <stdbool.h>\n" <>
    "#include <string>\n" <>
    "#include <vector>\n" <>
    "#include \"cbor.h\"\n" <>
    "#include \"cborconstants_p.h\"\n\n"

fnameInclude fname = T.concat ["#include \"", fname, "Cbor.hpp\"\n\n"]

headerC :: T.Text -> T.Text -> [Statement] -> T.Text
headerC fname mname sts = 
    if T.length fname > 0
        then fnameInclude fname <> addNamespace
        else includes <> otherIncludes sts <> addNamespace

headerCHeader :: T.Text -> T.Text -> [Statement] -> T.Text
headerCHeader fname mname sts = 
    if T.length fname > 0
        then includeHead fname <> includes <> otherIncludes sts <> addNamespace
        else includes <> otherIncludes sts <> addNamespace
        
footerCHeader :: T.Text -> T.Text
footerCHeader fname = if (T.length fname > 0) then "#endif\n" else ""


-- finally the both exported functions, which are writing the outputnfiles

writeHppFile :: T.Text -> T.Text -> [Statement] -> T.Text
writeHppFile fname mname sts =
    headerCHeader fname mname sts <> 
    "\n" <>
    (T.concat (
      (map stTdsHeader sts) ++
      (map (cSerializer True) sts)
    )) <>
    namespaceEnd <>
    "\n" <> 
    (T.concat (map stId64Header sts)) <>
    footerCHeader fname 
  
writeCppFile :: T.Text -> T.Text -> [Statement] -> T.Text
writeCppFile fname mname sts =
    headerC fname mname sts <> 
    "\n" <>
    (T.concat (
      (map (cSerializer False) sts)
    )) <>
    namespaceEnd <>
    "\n" <> 
    (T.concat (map stId64 sts)) <>
    footerCHeader fname 
  


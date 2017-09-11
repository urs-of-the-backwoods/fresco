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
    ciConvertible,
    cdConvertible
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

tN' tlt = cap1 (typeName tlt)

structElemN' :: TypeName -> FieldName -> T.Text
structElemN' n fn = low1 n <> cap1 fn

enumElemN' :: TypeName -> FieldName -> T.Text
enumElemN' n fn = cap1 fn

bT' = \t -> case t of
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
    BT_LT a -> "std::vector<" <> (bT' a) <> ">"
    BT_TN n -> n

typeDef' :: Bool -> TopLevelType -> T.Text
typeDef' defOnly t = if defOnly 
    then case t of
        TL_ET e@(EnumType en fs) -> enumEnumDef en fs <> enumStructDef en fs where
            enumEnumDef en fs = "typedef enum {\n" <> enumEnumFields en fs <> "} Enum" <> tN' t <> ";\n\n"
            enumEnumFields en fs = T.concat (L.map (enumEnumField en) (zip fs [0..]))
            enumEnumField en (EnumField n ts, i) = "    " <> enumElemN' en n <> " = " <> (T.pack (show i)) <> ",\n"

            enumStructDef en fs = "typedef struct {\n    " <> "Enum" <> tN' t <> " selector;\n" <> enumStructUnion en fs <> "} " <> tN' t <> ";\n\n"
            enumStructUnion en fs = "    struct {\n" <> enumStructFields en fs <> "    } data;\n"
            enumStructFields en fs = T.concat (L.map (enumStructField en) fs)
            enumStructField en (EnumField cn bts) = "        struct {\n" <> esfTypes bts <> "        } " <> cn <>";\n"
            esfTypes bts = T.concat (L.map esfType (zip [0..] bts))
            esfType (i, bt) = "            " <> bT' bt <> " value" <> (T.pack (show i)) <> ";\n"
        
        TL_ST s@(StructType sn fs) -> "typedef struct {\n" <> structStructFields sn fs <> "} " <> tN' t <> ";\n\n" where
            structStructFields sn fs = T.concat (L.map (structStructField sn) fs)
            structStructField sn (StructField cn bt) = "    " <> bT' bt <> " " <> cn <> ";\n"

        TL_TD (TypeDeclaration tn bt) -> "typedef " <> bT' bt <> " " <> tN' t <> ";\n\n"
        _ -> ""
    else case t of
        _ -> ""

ctDef' :: Bool -> TopLevelType -> T.Text
ctDef' defOnly t = let
    tnt = cap1 (typeName t)
    in if defOnly 
        then case t of
            TL_ID (Id64 tn i) -> "extern const uint64_t ct" <> tnt <> ";\n"
            _ -> ""
        else case t of
            TL_ID (Id64 tn i) -> "const uint64_t ct" <> tnt <> " = 0x" <> (T.pack (showHex i "")) <> ";\n"
            _ -> ""


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

    --  cbor_value_get_array_length(const CborValue *value, size_t *length)

    BT_PT PT_Data ->  \t -> "{ size_t l; cbor_value_get_array_length(it, &l); " <> t <> ".resize(l+1);\n" <>
        "        cbor_value_copy_byte_string(it, " <> t <> ".data(), &l, NULL); cbor_value_advance(it);}\n"

    BT_LT a -> \t -> readEnterContainer <>
                     "  size_t l; cbor_value_get_array_length(it, &l); \n" <>
                     "  " <> t <> ".clear();\n" <>
                     "  while (!cbor_value_at_end(it)) { " <> 
                     "  " <> bT' a <> " item; " <> getReaderDef a "item" <> "; " <> t <> ".push_back(item); } \n" <>  
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
    
nsEnd' = "\n} // end of namespacd cdb\n"

serDef' defOnly t = let

    sN = low1 (typeName t)
    tnt = cap1 (typeName t)
    readFunctionHead = "void read" <> tnt <> "(CborValue *it, " <> tnt <> " *" <> sN <> ")"
    writeFunctionHead = "void write" <> tnt <> "(CborEncoder *enc, " <> tnt <> " " <> sN <> ")"
    functionHeaders = readFunctionHead <> ";\n" <> writeFunctionHead <> ";\n" 
    readFunction fbody = readFunctionHead <> " {\n" <> fbody <> "}\n\n"
    writeFunction fbody = writeFunctionHead <> " {\n" <> fbody <> "}\n\n"
    functions readBody writeBody = readFunction readBody <> writeFunction writeBody

    in 
        if defOnly
            then case t of
                TL_ET _ -> functionHeaders
                TL_ST _ -> functionHeaders
                TL_TD (TypeDeclaration tn bt) -> functionHeaders
                _ -> ""

            else case t of
                TL_ET e@(EnumType tn fs) -> functions readBody writeBody where
                    readBody = readEnterContainer <> readSelector <> readFElems <> readLeaveContainer
                    readSelector = "int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);\n    " 
                                   <> sN <> "->selector = (Enum" <> tnt <> ")i;\n"
                    readFElems = T.concat (map readFElem (zip [0..] fs))
                    readFElem (i, e@(EnumField cn bts)) = "    if (" <> sN <> "->selector == " <> (T.pack (show i)) <> ") {\n" <> T.concat (map (readFBt t cn) (zip [0..] bts)) <> "    };\n"
                    readFBt t cn (n, bt) =  "        " <> ((getReaderDef bt) (sN <> "->data." <> cn <> ".value" <> (T.pack (show n))))


                    writeBody = T.concat (map writeFElem (zip [0..] fs))
                    writeFElem (i, e@(EnumField cn bts)) = writeFElemHead i <> writeEnterContainer ((length bts) + 1) <> writeFElemNum sN <> 
                                                           writeFElemFBs t cn bts <> writeLeaveContainer 
                    writeFElemHead i = "    if (" <> sN <> ".selector == " <> (T.pack (show i)) <> ") \n"
                    writeFElemNum sN = "        cbor_encode_uint(enc, (uint64_t)" <> sN <> ".selector);\n"
                    writeFElemFBs t cn bts = T.concat (map (writeFBt t cn) (zip [0..] bts))
                    writeFBt t cn (n, bt) =  "        " <> ((getWriterDef bt) (sN <> ".data." <> cn <> ".value" <> (T.pack (show n))))

                TL_ST s@(StructType tn fs) -> functions readBody writeBody where
                    readBody = readEnterContainer <> readFElems <> readLeaveContainer
                    readFElems = T.concat (map readFElem fs)
                    readFElem f@(StructField cn bt) = "    " <> ((getReaderDef bt) (sN <> "->" <> cn))

                    writeBody = writeEnterContainer (length fs) <> writeFElems <> writeLeaveContainer
                    writeFElems = T.concat (map writeFElem fs)
                    writeFElem s@(StructField cn bt) = writeFBt t cn bt
                    writeFBt t cn bt =  "    " <> ((getWriterDef bt) (sN <> "." <> cn ))

                TL_TD (TypeDeclaration tn bt) ->  readFunction readBody <> writeFunction writeBody where
                    readBody = "    " <> bT' bt <> " rval;\n    " <> ((getReaderDef bt) "rval") <> "    *" <> sN <> " = rval;\n"
                    writeBody =  "    " <> ((getWriterDef bt) sN)
                _ -> ""


headDef' :: Bool -> T.Text -> [TopLevelType] -> T.Text
headDef' defOnly fname ts = if defOnly 
    then -- header
        if T.length fname > 0
            then includeHead fname <> includes <> otherIncludes <> addNamespace
            else includes <> otherIncludes <> addNamespace
    else -- cpp file
        if T.length fname > 0
            then fnameInclude <> addNamespace
            else includes <> otherIncludes <> addNamespace

    where
        hdImport t = case t of
                        TL_IM (Import iName iId) -> "#include \"" <> iName <> "Cbor.hpp\"\n"
                        _ -> ""
        includeHead fname = T.concat ["#ifndef __", fname, "_cbor__\n#define __", fname, "_cbor__\n\n"]
        otherIncludes = (T.concat (map hdImport ts))
        addNamespace = "namespace cbd {\n"
        includes = 
           "#include <stdint.h>\n" <>
           "#include <stdbool.h>\n" <>
           "#include <string>\n" <>
           "#include <vector>\n" <>
           "#include \"cbor.h\"\n" <>
           "#include \"cborconstants_p.h\"\n\n"
        fnameInclude = T.concat ["#include \"", fname, "Cbor.hpp\"\n\n"]

        
footDef' :: Bool -> T.Text -> T.Text
footDef' defOnly fname = if (T.length fname > 0) && defOnly then "#endif\n" else ""

ciConvertible = Convertible tN' structElemN' enumElemN' bT' (headDef' False) (typeDef' False) (serDef' False) nsEnd' (ctDef' False) (footDef' False) 
cdConvertible = Convertible tN' structElemN' enumElemN' bT' (headDef' True) (typeDef' True) (serDef' True) nsEnd' (ctDef' True) (footDef' True)  


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
    BT_LT a -> "std::vector<" <> (bT' (head a)) <> ">"
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
ctDef' defOnly t = if defOnly 
    then case t of
        TL_ID (Id64 tn i) -> "extern const uint64_t ct" <> tN' t <> ";\n"
        _ -> ""
    else case t of
        TL_ID (Id64 tn i) -> "const uint64_t ct" <> tN' t <> " = 0x" <> (T.pack (showHex i "")) <> ";\n"
        _ -> ""

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
    BT_PT PT_Data ->  \t -> "{ size_t l; cbor_value_calculate_string_length(it, &l); " <> t <> ".resize(l+1);\n" <>
        "        cbor_value_copy_byte_string(it, " <> t <> ".data(), &l, NULL); cbor_value_advance(it);}\n"
    BT_LT a -> const "/* TBD */"
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
    BT_LT a -> const "/* TBD */"
    BT_TN n -> \t -> "write" <> n <> "(enc, " <> t <> ");\n"
    
nsEnd' = "\n} // end of namespacd cdb\n"

serDef' defOnly t = if defOnly
    then case t of
        TL_ET e@(EnumType tn fs) -> readFunction t e <> writeFunction t e where
            readFunction t e = readFHead t e 
            sN = low1 $ tn
            tnt = tN' t 
            readFHead t _ = "void read" <> tnt <> "(CborValue *it0, " <> tnt <> " *" <> sN <> ");\n"

            writeFunction t e = writeFHead t e
            writeFHead t e = "void write" <> tnt <> "(CborEncoder *enc0, " <> tnt <> " " <> sN <> ");\n"

        TL_ST s@(StructType tn fs) -> readFunction t s <> writeFunction t s where
            readFunction t s = readFHead t s 
            sN = low1 $ tn
            tnt = tN' t 
            readFHead t _ = "void read" <> tnt <> "(CborValue *it0, " <> tnt <> " *" <> sN <> ");\n"

            writeFunction t s =  writeFHead t s 
            writeFHead t s =  "void write" <> tnt <> "(CborEncoder *enc0, " <> tnt <> " " <> sN <> ");\n"

        TL_TD (TypeDeclaration tn bt) ->  readFunction <> writeFunction where
            readFunction = readFHead 
            sN = low1 $ tn
            tnt = tN' t 
            readFHead = "void read" <> tnt <> "(CborValue *it, " <> tnt <> " *" <> sN <> ");\n" 

            writeFunction =  writeFHead 
            writeFHead = "void write" <> tnt <> "(CborEncoder *enc, " <> tnt <> " " <> sN <> ");\n"
        _ -> ""
    else case t of
        TL_ET e@(EnumType tn fs) -> readFunction t e <> writeFunction t e where
            readFunction t e = readFHead t e <> readFElems t e <> readFFooter
            sN = low1 $ tn
            tnt = tN' t 
            readFHead t _ = "void read" <> tnt <> "(CborValue *it0, " <> tnt <> " *" <> sN <> ")\n{\n" <> readFCont <>
                            "    int i; cbor_value_get_int(it, &i); cbor_value_advance_fixed(it);\n    " <> sN <> "->selector = (Enum" <> tnt <> ")i;\n"
            readFCont = "    CborValue it1; CborValue *it = &it1; cbor_value_enter_container(it0, it);\n" 
            readFElems t e@(EnumType tn fs) = T.concat (map readFElem (zip [0..] fs))
            readFElem (i, e@(EnumField cn bts)) = "    if (" <> sN <> "->selector == " <> (T.pack (show i)) <> ") {\n" <> T.concat (map (readFBt t cn) (zip [0..] bts)) <> "    };\n"
            readFBt t cn (n, bt) =  "        " <> ((getReaderDef bt) (sN <> "->data." <> cn <> ".value" <> (T.pack (show n))))
            readFFooter = "    cbor_value_leave_container(it0, it);\n}\n\n"

            writeFunction t e = writeFHead t e <> writeFElems t e <> "}\n\n"
            writeFHead t e = "void write" <> tnt <> "(CborEncoder *enc0, " <> tnt <> " " <> sN <> ")\n{\n"
            writeFElems t e = T.concat (map writeFElem (zip [0..] fs))
            writeFElem (i, e@(EnumField cn bts)) = writeFElemHead i <> writeFElemCont bts <> writeFElemNum sN <> writeFElemFBs t cn bts <> writeFElemFooter 
            writeFElemHead i = "    if (" <> sN <> ".selector == " <> (T.pack (show i)) <> ") {\n"
            writeFElemCont bts = "        CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, " <> (T.pack (show (length bts + 1))) <> ");\n" 
            writeFElemNum sN = "        cbor_encode_uint(enc, (uint64_t)" <> sN <> ".selector);\n"
            writeFElemFBs t cn bts = T.concat (map (writeFBt t cn) (zip [0..] bts))
            writeFElemFooter = "        cbor_encoder_close_container_checked(enc0, enc);\n    };\n"
            writeFBt t cn (n, bt) =  "        " <> ((getWriterDef bt) (sN <> ".data." <> cn <> ".value" <> (T.pack (show n))))

        TL_ST s@(StructType tn fs) -> readFunction t s <> writeFunction t s where
            readFunction t s = readFHead t s <> readFElems t s <> readFFooter
            sN = low1 $ tn
            tnt = tN' t 
            readFHead t _ = "void read" <> tnt <> "(CborValue *it0, " <> tnt <> " *" <> sN <> ")\n{\n" <> readFCont
            readFCont = "    CborValue it1; CborValue* it = &it1; cbor_value_enter_container(it0, it);\n" 
            readFElems t s@(StructType tn fs) = T.concat (map readFElem fs)
            readFElem f@(StructField cn bt) = "    " <> ((getReaderDef bt) (sN <> "->" <> cn))
            readFFooter = "    cbor_value_leave_container(it0, it);\n}\n\n"

            writeFunction t s =  writeFHead t s <> writeFElems t s <> writeFFooter
            writeFHead t s =  "void write" <> tnt <> "(CborEncoder *enc0, " <> tnt <> " " <> sN <> ")\n{\n" <> writeFCont
            writeFCont = "    CborEncoder enc1; CborEncoder* enc = &enc1; cbor_encoder_create_array(enc0, enc, " <> (T.pack (show (length fs))) <> ");\n"
            writeFElems t e = T.concat (map writeFElem fs)
            writeFElem s@(StructField cn bt) = writeFBt t cn bt
            writeFBt t cn bt =  "    " <> ((getWriterDef bt) (sN <> "." <> cn ))
            writeFFooter = "    cbor_encoder_close_container_checked(enc0, enc);\n}\n\n"

        TL_TD (TypeDeclaration tn bt) ->  readFunction <> writeFunction where
            readFunction = readFHead <> readFBt <> "}\n\n"
            sN = low1 $ tn
            tnt = tN' t 
            readFHead = "void read" <> tnt <> "(CborValue *it, " <> tnt <> " *" <> sN <> ")\n{\n" 
            readFBt = "    " <> bT' bt <> " rval;\n    " <> ((getReaderDef bt) "rval") <> "    *" <> sN <> " = rval;\n"

            writeFunction =  writeFHead <> writeFBt <> "}\n\n"
            writeFHead = "void write" <> tnt <> "(CborEncoder *enc, " <> tnt <> " " <> sN <> ")\n{\n"
            writeFBt =  "    " <> ((getWriterDef bt) sN)
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


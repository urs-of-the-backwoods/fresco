--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2016 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: src/Sinopia/Rust.hs
--

{-# LANGUAGE OverloadedStrings #-}

module Sinopia.Rust where 

import Sinopia.Data
import Sinopia.Parser
import Sinopia.Util

-- create output for Rust
{-

rTN :: TopLevelType -> String
rTN tlt = let (TypeName n) = (typeName tlt) in cap1 n

rStructElemName :: ElemName -> String
rStructElemName (ElemName c) = low1 c

rEnumElemName :: ConsName -> String
rEnumElemName (ConsName c) = cap1 c

rBT :: BaseType -> String
rBT t = case t of
    BT_PT Bool -> "bool"    
    BT_PT Int8 -> "i8"
    BT_PT Int16 -> "i16"
    BT_PT Int32 -> "i32"
    BT_PT Int64 -> "i64"
    BT_PT UInt8 -> "u8"
    BT_PT UInt16 -> "u16"
    BT_PT UInt32 -> "u32"
    BT_PT UInt64 -> "u64"
    BT_PT Float32 -> "f32"
    BT_PT Float64 -> "f64"
    BT_PT Text -> "String"
    BT_PT Data -> "Vec<i8>"
    BT_LT (List a) -> "Vec<" ++ (rBT a) ++ ">"
    BT_TN (TypeName n) -> n
    _ -> error "Unknown Base Type in rBT!"

rFields :: TopLevelType -> String
rFields tlt = let
    ti = adtInfo tlt
    in case ti of
        Left es -> concatMap (\(en, bt) -> "    " ++ rStructElemName en ++ ": " ++ rBT bt ++ ",\n") es
        Right es -> concatMap (\(cn, bts) -> "    " ++ rEnumElemName cn ++ (if length bts > 0 then ("(" ++ concat (L.intersperse ", " (map (\bt -> rBT bt) bts)) ++ ")") else "" ) ++ ",\n") es

rTypeDef :: TopLevelType -> String
rTypeDef t = case t of
    TL_ET _ -> "#[derive(Debug, PartialEq, RustcDecodable, RustcEncodable)]\nenum " ++ rTN t ++ " {\n" ++ rFields t ++ "}\n\n"
    TL_ST _ -> "#[derive(Debug, PartialEq, RustcDecodable, RustcEncodable)]\nstruct " ++ rTN t ++ " {\n" ++ rFields t ++ "}\n\n"
    TL_ID (Id64 tn i) -> "const CT_" ++ (map Char.toUpper (rTN t)) ++ ": u64 = 0x" ++ (showHex i "") ++ ";\n\n"
    TL_TD (TypeDeclaration _ bt) -> "type " ++ rTN t ++ " = " ++ rBT bt ++ ";\n\n"
    _ -> ""

rTLObjT v = ""
-}


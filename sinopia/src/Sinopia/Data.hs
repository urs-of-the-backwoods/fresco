--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2016 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: src/Sinopia/Data.hs
--

{-# LANGUAGE OverloadedStrings #-}

module Sinopia.Data where 

import qualified Data.Word as W
import qualified Data.Text as T

-- AST - define our data structure, we want to parse values into that

type TypeName = T.Text
type FieldName = T.Text
type NamespaceName = T.Text
type ImportName = T.Text

data Primitive = PT_Bool | PT_Null
                    | PT_Int8 | PT_Int16 | PT_Int32 | PT_Int64 
                    | PT_UInt8 | PT_UInt16 | PT_UInt32 | PT_UInt64 
                    | PT_Float32 | PT_Float64
                    | PT_Text
                    | PT_Data
                    deriving (Show, Read, Eq)

data BaseType = BT_PT Primitive
                | BT_TN TypeName
                | BT_LT BaseType
                deriving (Show, Read, Eq)

data EnumField = EnumField FieldName [BaseType]        
              deriving (Show, Read, Eq)

data EnumType = EnumType TypeName [EnumField]
                    deriving (Show, Read, Eq)

data StructField = StructField FieldName BaseType      
              deriving (Show, Read, Eq)

data StructType = StructType TypeName [StructField]
                    deriving (Show, Read, Eq)

data Namespace = Namespace NamespaceName
                    deriving (Show, Read, Eq)

data Import = Import ImportName W.Word64
                    deriving (Show, Read, Eq)

data TypeDeclaration = TypeDeclaration TypeName BaseType
                    deriving (Show, Read, Eq)

data Id64 = Id64 TypeName W.Word64
                    deriving (Show, Read, Eq)

data TopLevelType = TL_NS Namespace
                    | TL_IM Import
                    | TL_TD TypeDeclaration
                    | TL_ID Id64
                    | TL_ET EnumType
                    | TL_ST StructType
                    deriving (Show, Read, Eq)


-- functions, which deconstruct data structures

typeName :: TopLevelType -> TypeName
typeName tlt = case tlt of
    TL_TD (TypeDeclaration tn _) -> tn
    TL_ST (StructType tn _) -> tn
    TL_ET (EnumType tn _) -> tn
    TL_ID (Id64 tn _) -> tn
    _ -> ""

-- 
-- Check Rules of Data Files
--

{-

There are two types of files:

normal definition files:
Rule 1: there need to be exactly one id64 declaration with the main type being stamped
Rule 2: the structure being named with the id declaration needs to be in the file defined
Rule 3: normal files should NOT have a namespace

namespace declaration files:
Rule 4: files with namespaces should only have additional import declararations, they simply set a namespace on the imported items below it

-}

-- check of data structure

isId (TL_ID _) = True
isId _ = False

isNS (TL_NS _) = True
isNS _ = False

isIm (TL_IM _) = True
isIm _ = False

isNameSpaceFile :: [TopLevelType] -> Bool
isNameSpaceFile ast = length (filter isNS ast) > 0

checkParsedData :: [TopLevelType] -> (Bool, T.Text)
checkParsedData ast = 
  case isNameSpaceFile ast of
    False -> -- normal file, check rules 1 - 3 
      let
        ids = filter isId ast
        names = map typeName ast
        ns = filter isNS ast
        in if length ids /= 1 
          then (False, "one and only one id needed!")
          else if not ((typeName (ids !! 0)) `elem` names)
            then (False, "id type not defined!")
            else (True, "normal file, parsed Ok")
    True -> -- namespace file, check rule 4
      let nIm = length (filter isIm ast)
          nNS = length (filter isNS ast) 
          in case (nIm + nNS) == (length ast) of
               True -> (True, "namespace file, parsed Ok")
               False -> (False, "namespace files should not contain other elements than Imports and Namespaces!")


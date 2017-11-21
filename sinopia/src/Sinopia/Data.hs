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
type ImportName = T.Text
type Comment = [T.Text]

data PrimitiveType = PT_Bool | PT_Null
                    | PT_Int8 | PT_Int16 | PT_Int32 | PT_Int64 
                    | PT_UInt8 | PT_UInt16 | PT_UInt32 | PT_UInt64 
                    | PT_Float32 | PT_Float64
                    | PT_Text
                    | PT_Data
                    deriving (Show, Read, Eq)

data BaseType = BT_PT PrimitiveType
                | BT_TN TypeName
                | BT_LT BaseType
                deriving (Show, Read, Eq)

data TypeDefinition = TypeDefinition TypeName BaseType (Maybe Comment)
                    deriving (Show, Read, Eq)

data EnumField = EnumField FieldName [BaseType] (Maybe Comment)        
              deriving (Show, Read, Eq)

data EnumType = EnumType TypeName [EnumField] (Maybe Comment)
                    deriving (Show, Read, Eq)

data StructField = StructField FieldName BaseType (Maybe Comment)      
              deriving (Show, Read, Eq)

data StructType = StructType TypeName [StructField] (Maybe Comment)
                    deriving (Show, Read, Eq)

data TypeDeclaration = TD_TD TypeDefinition
                    | TD_ET EnumType
                    | TD_ST StructType
                    deriving (Show, Read, Eq)

data Id64 = Id64 TypeName W.Word64
                    deriving (Show, Read, Eq)

data Import = Import ImportName W.Word64
                    deriving (Show, Read, Eq)

data Statement = ST_TD TypeDeclaration
               | ST_ID Id64
               | ST_IM Import
                    deriving (Show, Read, Eq)


-- functions, which deconstruct data structures

typeName :: TypeDeclaration -> TypeName
typeName tlt = case tlt of
    TD_TD (TypeDefinition tn _ _) -> tn
    TD_ST (StructType tn _ _) -> tn
    TD_ET (EnumType tn _ _) -> tn


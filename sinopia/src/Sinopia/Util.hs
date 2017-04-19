--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2016 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: src/Sinopia/Util.hs
--

{-# LANGUAGE OverloadedStrings #-}

module Sinopia.Util where 

import qualified Data.Char as C
import qualified Data.Text as T
import Data.Monoid

import Sinopia.Data
import Sinopia.Parser

-- utilities

cap1 :: T.Text -> T.Text
cap1 "" = ""
cap1 t = T.cons ((C.toUpper . T.head) t) (T.tail t)

low1 :: T.Text -> T.Text
low1 "" = ""
low1 t = T.cons ((C.toLower . T.head) t) (T.tail t)

data Convertible = Convertible {
  tN :: TopLevelType -> T.Text       -- type name
, structElemN :: TypeName -> FieldName -> T.Text
, enumElemN :: TypeName -> FieldName -> T.Text 
, bT :: BaseType -> T.Text           -- base type, a being ignored
, headDef :: T.Text -> [TopLevelType] -> T.Text                  -- header definition for file, a being ignored
, typeDef :: TopLevelType -> T.Text  -- top level type definition
, serDef :: TopLevelType -> T.Text   -- serialization definition
, nsEnd :: T.Text
, ctDef :: TopLevelType -> T.Text  -- top level type definition
, footDef :: T.Text -> T.Text                  -- footer definition for file, a being ignored
}

conversion :: Convertible -> T.Text -> [TopLevelType] -> T.Text
conversion cn fname lTop = 
	(headDef cn) fname lTop <> 
    "\n" <>
    (T.concat (
      (map (typeDef cn) lTop) ++
      (map (serDef cn) lTop)
    )) <>
    (nsEnd cn) <>
    "\n" <> 
    (T.concat (map (ctDef cn) lTop)) <>
    (footDef cn) fname 

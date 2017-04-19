--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015 - 2017 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: haskell/Fresco/Component.hs
--
    
{-# LANGUAGE TypeSynonymInstances #-}

-- | Components of the Entity Component System of Fresco binding
module Fresco.Component
(
    ComponentType (..),
    Component
)
where

import Data.Word
import Data.ByteString.Lazy
import qualified Data.ByteString as BS
import Data.Text
import Data.Text.Encoding

-- | Components in Entities are indexed by ComponentType
data ComponentType a = ComponentType Word64 deriving (Eq, Show, Ord)

-- | Components are stored as ByteString
type Component = ByteString



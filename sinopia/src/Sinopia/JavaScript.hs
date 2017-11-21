--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2017 Peter Althainz
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

writeJavaScriptFile :: T.Text -> T.Text -> [Statement] -> T.Text
writeJavaScriptFile fname mname sts = ""

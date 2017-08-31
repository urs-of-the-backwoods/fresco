--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2017 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: test/ParserSpec.hs
--

{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module ParserSpec(main, spec) where

import Test.Hspec

import Sinopia.Data
import Sinopia.Parser

import Data.Either.Unwrap
import Data.String.Here

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Sinopia.Parser" $ do

  it "can parse a struct" $ do

    parseAST [here|

      struct Hello {
        coolInt : Int32 ; 
        coolBool : Bool ;
        }

      |] `shouldBe` 

  	  Right [TL_ST (StructType "Hello" 
             [StructField "coolInt" (BT_PT PT_Int32),
              StructField "coolBool" (BT_PT PT_Bool)
             ])]


  it "can parse a struct with a list" $ do

    parseAST [here|

      struct Hello {
        coolInt : List (Int32) ; 
        coolBool : Bool ;
        }

      |] `shouldBe` 

      Right [TL_ST (StructType "Hello" 
             [StructField "coolInt" (BT_LT (BT_PT PT_Int32)),
              StructField "coolBool" (BT_PT PT_Bool)
             ])]


  it "can detect a wrong struct" $ do

    (show . fromLeft . parseAST) [here|

      coolInt : Int32; 
      coolBool : Bool;
      }

      |] `shouldBe` 

      "\"string\""


  it "can detect trailing nonsense" $ do

    (show . fromLeft . parseAST) [here|

      id64 testId = 0x02 

      coolInt : Int32; 
      coolBool : Bool;
      }

    |] `shouldBe` 

      "\"endOfInput\""


  it "can parse an enum" $ do

    parseAST 
      "enum Hello {one; two Int64; three;}" 

      `shouldBe` 

	    Right [TL_ET (EnumType "Hello" [EnumField "one" [], EnumField "two" [BT_PT PT_Int64], EnumField "three" [] ])]


  it "can parse a typedef" $ do

    parseAST 
      "type NewType = Int64"

      `shouldBe` 

      Right [TL_TD (TypeDeclaration "NewType" (BT_PT PT_Int64))]


  it "can parse an import" $ do

    parseAST 
      "import Hello 0x08ff"

      `shouldBe` 

      Right [TL_IM (Import "Hello" 2303)]


  it "checks for missing Id's in file" $ do

    checkParsedData ( fromRight (parseAST [here|

      struct Hello {
        coolInt : Int32;
        coolBool : Bool;
        }

      id64 Hello = 0xff78
      |] )) `shouldBe`

      (True, "normal file, parsed Ok")
      
    checkParsedData ( fromRight (parseAST [here|

      id64 Hello = 0xff78
      struct Hello {
        coolInt : Int32;
        coolBool : Bool;
        }
      id64 Hello = 0xff78

      |] )) `shouldBe`

      (False, "one and only one id needed!")
      
    checkParsedData ( fromRight (parseAST [here|

      struct Hello {
        coolInt : Int32;
        coolBool : Bool;
        }

      |] )) `shouldBe`

      (False, "one and only one id needed!")
      

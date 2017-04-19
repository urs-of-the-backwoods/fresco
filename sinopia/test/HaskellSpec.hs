--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2017 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: test/HaskellSpec.hs
--

{-# LANGUAGE OverloadedStrings #-}

module HaskellSpec(main, spec) where

import Test.Hspec

import Sinopia.Data
import Sinopia.Parser
import Sinopia.Haskell

import Data.Either.Unwrap
import Numeric (showHex)
import Data.ByteString.Lazy
import Data.ByteString.Builder

import Fresco
import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Decoding
import Data.Binary.Serialise.CBOR.Encoding as E

import Data.Monoid

main :: IO ()
main = hspec spec


data Shape = Sphere
    | Cube
    | Plane
    | Cylinder Int
    | Pyramid Float Float
    | Torus
    deriving (Eq, Read, Show)

instance Serialise Shape where
  encode (Cube) = encode (0::Int)
  encode (Plane) = encode (1::Int)
  encode (Cylinder v1) = encode (2::Int) <> encode v1
  encode (Pyramid v1 v2) = encode (3::Int) <> encode v1 <> encode v2
  encode (Torus) = encode (4::Int)
  decode = do
    i <- decode :: Decoder Int
    case i of
        0 -> (pure Cube)
        1 -> (pure Plane)
        2 -> (Cylinder <$> decode)
        3 -> (Pyramid <$> decode <*> decode)
        4 -> (pure Torus)

data TrueColour = TrueColour Bool Colour

data Colour = Colour {
    colourRed::Float,
    colourGreen::Float,
    colourBlue::Float,
    colourAlpha::Float
} deriving (Eq, Show)

instance Serialise Colour where
  encode (Colour v1 v2 v3 v4) = encode v1 <> encode v2 <> encode v3 <> encode v4
  decode = Colour <$> decode <*> decode <*> decode <*> decode

instance Serialise TrueColour where
    encode (TrueColour isTrue c) = encode isTrue <> encode c
    decode = TrueColour <$> decode <*> decode

spec :: Spec
spec = describe "Sinopia.Haskell" $ do

  it "can output a simple datatype as CBOR data" $ do
    let c1 = Colour 1.0 2.0 3.0 4.0
      in (toLazyByteString . lazyByteStringHex) (serialise c1)
        `shouldBe`
           "fa3f800000fa40000000fa40400000fa40800000"

  it "can output a complex datatype as CBOR data" $ do
    let tc1 = TrueColour False (Colour 1.0 2.0 3.0 4.0)
      in (toLazyByteString . lazyByteStringHex) (serialise tc1)
        `shouldBe`
           "f4fa3f800000fa40000000fa40400000fa40800000"

  it "can input colour from CBOR data" $ do
    (deserialise (serialise (Colour 2.0 3.0 4.0 5.0)))
       `shouldBe`
         (Colour 2.0 3.0 4.0 5.0)    

  it "can output an enum as CBOR data" $ do
    let s1 = Plane
      in (toLazyByteString . lazyByteStringHex) (serialise s1)
        `shouldBe`
           "01"

  it "can output a more complex enum as CBOR data" $ do
    let s1 = Pyramid 1.0 2.0
      in (toLazyByteString . lazyByteStringHex) (serialise s1)
        `shouldBe`
           "03fa3f800000fa40000000"

  it "can input enum from CBOR data" $ do
    (deserialise (serialise (Pyramid 1.0 2.0)))
       `shouldBe`
         (Pyramid 1.0 2.0)    

  it "can input simple enum from CBOR data" $ do
    (deserialise (serialise (Plane)))
       `shouldBe`
         (Plane)    

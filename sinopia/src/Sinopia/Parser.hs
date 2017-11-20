--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2017 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: src/Sinopia/ParserC.hs
--

{-# LANGUAGE OverloadedStrings #-}

module Sinopia.Parser where 

import Control.Applicative
import Control.Monad

import Data.Attoparsec.Text
import qualified Data.Word as W
import qualified Data.Int as I
import Data.Char
import Data.Text

import Sinopia.Data

-- Tokenizer definition

cmnt :: Parser [Text]
cmnt = many1 (( char '#' 
                <|> (char '/' >> char '/')
                    ) >> spc >> do
                            l <- takeTill isEndOfLine
                            spc
                            return l)

spc' :: Parser ()
spc' = many' ( space >> return () ) >> return ()

spc :: Parser ()
spc = 
        (many1 ( (space >> return ()) ) >> return ())
    <|> do
            c <- peekChar
            case c of
                Nothing -> return ()
                Just c' -> if isLetter c' || isDigit c' then mzero else return ()

identifier :: Parser Text
identifier = do
    c <- letter
    t <- many' (letter <|> digit)
    spc
    return $ pack (c: t)

identifierP :: Parser Text
identifierP = do
    c <- letter
    t <- many' (letter <|> digit <|> char '.')
    spc
    return $ pack (c: t)

symbol :: Text -> Parser ()
symbol c = do
    v <- string c
    spc'
    return ()

rword :: Text -> Parser ()
rword c = do
    v <- string c
    spc
    return ()

integer :: Parser Integer
integer = do
    v <- (("0x" *> hexadecimal) <|> decimal)
    spc
    return v

between :: Parser a -> Parser b -> Parser c -> Parser c
between a b c = a >> c >>= \v -> (b >> return v) 

parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")
semicolon = symbol ";"
colon     = symbol ":"
equals    = symbol "="

-- token functions

parseId64 :: Parser Id64
parseId64 = do
    rword "id64"
    n <- identifier
    equals
    i <- integer
    return (Id64 n (fromIntegral i))

parsePT :: Parser Primitive
parsePT = 
    (rword "Bool" >> return PT_Bool)
    <|> (rword "Null" >> return PT_Null)
    <|> (rword "Int8" >> return PT_Int8)
    <|> (rword "Int16" >> return PT_Int16)
    <|> (rword "Int32" >> return PT_Int32)
    <|> (rword "Int64" >> return PT_Int64)
    <|> (rword "UInt8" >> return PT_UInt8)
    <|> (rword "UInt16" >> return PT_UInt16)
    <|> (rword "UInt32" >> return PT_UInt32)
    <|> (rword "UInt64" >> return PT_UInt64)
    <|> (rword "Float32" >> return PT_Float32)
    <|> (rword "Float64" >> return PT_Float64)
    <|> (rword "Text" >> return PT_Text)
    <|> (rword "Data" >> return PT_Data)

parseBT :: Parser BaseType
parseBT = 
        (parsePT >>= \v -> return (BT_PT v))
    <|> do
            rword "List" 
            t <- parens parseBT
            return (BT_LT t)
    <|> (identifier >>= \v -> return (BT_TN v))

parseEnumField :: Parser EnumField
parseEnumField = do
        n <- identifier
        ts <- many' parseBT
        semicolon
        c <- option Nothing (Just <$> cmnt)
        return (EnumField n ts c)

parseET :: Parser EnumType
parseET = do
        c <- option Nothing (Just <$> cmnt)
        rword "enum"
        n <- identifier
        values <- braces (many1 parseEnumField)
        return (EnumType n values c)

parseStructField :: Parser StructField
parseStructField = do
        n <- identifier
        colon
        t <- parseBT
        semicolon
        c <- option Nothing (Just <$> cmnt)
        return (StructField n t c)

parseST :: Parser StructType
parseST = do
        c <- option Nothing (Just <$> cmnt)
        rword "struct"
        n <- identifier
        fs <- braces $ many1 parseStructField
        return (StructType n fs c)

parseIM :: Parser Import
parseIM = do
    rword "import"
    a <- identifier 
    i <- integer
    return (Import a (fromIntegral i))

parseTD :: Parser TypeDeclaration
parseTD = do
    c <- option Nothing (Just <$> cmnt)
    rword "type"
    td <- identifier
    equals
    tn <- parseBT
    return (TypeDeclaration td tn c)

parseTL :: Parser TopLevelType
parseTL = do
        (parseIM >>= \v -> return (TL_IM v))
    <|> (parseTD >>= \v -> return (TL_TD v))
    <|> (parseId64 >>= \v -> return (TL_ID v))
    <|> (parseST >>= \v -> return (TL_ST v))
    <|> (parseET >>= \v -> return (TL_ET v))

parseTLS = do
    spc'
    v <- many1 parseTL
    endOfInput
    return v

parseAST inS = parseOnly parseTLS inS


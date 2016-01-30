--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2016 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: sinopia/stack.yaml
--

{-# LANGUAGE OverloadedStrings #-}

module Main where 

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Numeric

import System.Console.GetOpt
import Control.Monad
import System.IO
import System.Environment
import System.Exit
import Debug.Trace
import qualified Data.Word as W
import qualified Data.Int as I
import qualified Data.Char as Char
import qualified Data.List as L
import qualified Data.Maybe as M

-- Tokenizer definition

langDef   :: LanguageDef st
langDef    = LanguageDef
               { commentStart   = ""
               , commentEnd     = ""
               , commentLine    = "#"
               , nestedComments = True
               , identStart     = letter <|> char '_'
               , identLetter    = alphaNum <|> oneOf "_."
               , opStart        = opLetter emptyDef
               , opLetter       = oneOf ""
               , reservedOpNames= []
               , reservedNames  = [ "Bool", 
                                    "Int8", "Int16", "Int32", "UInt8", "UInt16", "UInt32", 
                                    "Float32", "Float64",
                                    "Text", "Data",
                                    "List",
                                    "enum",
                                    "struct",
                                    "namespace", "type", "id64", "import"
                                  ]
               , caseSensitive  = True
               }

lexer = makeTokenParser langDef


-- AST - define our data structure, we want to parse values into that

newtype TypeName = TypeName String
                    deriving (Show, Read, Eq)
newtype ConsName = ConsName String
                    deriving (Show, Read, Eq)
newtype ElemName = ElemName String
                    deriving (Show, Read, Eq)

data PrimitiveType = Bool 
                    | Int8 | Int16 | Int32 | Int64 
                    | UInt8 | UInt16 | UInt32 | UInt64 
                    | Float32 | Float64
                    | Text
                    | Data
                    deriving (Show, Read, Eq)

data ListType = List BaseType
                    deriving (Show, Read, Eq)

data BaseType = BT_PT PrimitiveType
                | BT_LT ListType
                | BT_TN TypeName
                deriving (Show, Read, Eq)

data EnumField = EnumField ConsName [BaseType]        
              deriving (Show, Read, Eq)

data StructField = StructField ElemName BaseType      
              deriving (Show, Read, Eq)

data EnumType = Enum TypeName [EnumField]
                    deriving (Show, Read, Eq)

data StructType = Struct TypeName [StructField]
                    deriving (Show, Read, Eq)

data Namespace = Namespace String
                    deriving (Show, Read, Eq)

data Import = Import String
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
    TL_ST (Struct tn _) -> tn
    TL_ET (Enum tn _) -> tn
    TL_ID (Id64 tn _) -> tn

adtInfo :: TopLevelType -> Either [(ElemName, BaseType)] [(ConsName, [BaseType])]   -- left struct, right enum
adtInfo adt = case adt of
    TL_ST (Struct _ fs) -> Left (map (\(StructField en bt) -> (en, bt)) fs)
    TL_ET (Enum _ fs) -> Right (map (\(EnumField cn bts) -> (cn, bts)) fs)
    _ -> error "used adtInfo on non struct or enum!"

-- parser functions

parseName :: Parsec String () TypeName
parseName = lexeme lexer $ do
                n <- identifier lexer
                return (TypeName n)

parseId64 :: Parsec String () Id64
parseId64 = lexeme lexer $ do
    string "id64" >> spaces
    n <- parseName
    spaces >> char '=' >> spaces
    i <- natural lexer
    return (Id64 n (fromIntegral i))

--parsePT :: Parsec String () PrimitiveType
parsePT = lexeme lexer $ do
    (string "Bool" >> return Bool)
    <|> try (string "Int8" >> return Int8)
    <|> try (string "Int16" >> return Int16)
    <|> try (string "Int32" >> return Int32)
    <|> try (string "Int64" >> return Int64)
    <|> try (string "UInt8" >> return UInt8)
    <|> try (string "UInt16" >> return UInt16)
    <|> try (string "UInt32" >> return UInt32)
    <|> try (string "UInt64" >> return UInt64)
    <|> try (string "Float32" >> return Float32)
    <|> try (string "Float64" >> return Float64)
    <|> try (string "Text" >> return Text)
    <|> try (string "Data" >> return Data)
    <|> (string "Bool" >> return Bool)

parseLT :: Parsec String () ListType
parseLT = do
    string "List(" 
    t <- parseBT
    char ')'
    return (List t)

parseBT :: Parsec String () BaseType
parseBT = lexeme lexer $ do
        try (parsePT >>= \v -> return (BT_PT v))
    <|> try (parseLT >>= \v -> return (BT_LT v))
    <|> (parseName >>= \v -> return (BT_TN v))


parseEnumField :: Parsec String () EnumField
parseEnumField = lexeme lexer $ do
        n <- identifier lexer
        spaces
        ts <- many parseBT
        spaces >> char ';' >> spaces
        return (EnumField (ConsName n) ts)

parseET :: Parsec String () EnumType
parseET = lexeme lexer $ do
        n <- string "enum" >> spaces >> parseName
        spaces >> char '{' >> spaces
        values <- many1 parseEnumField
        spaces >> char '}'
        return (Enum n values)

parseStructField :: Parsec String () StructField
parseStructField = lexeme lexer $ do
        n <- identifier lexer
        spaces >> char ':' >> spaces
        t <- parseBT
        spaces >> char ';' >> spaces
        return (StructField (ElemName n) t)

parseST :: Parsec String () StructType
parseST = lexeme lexer $ do
        string "struct" >> spaces
        n <- parseName
        spaces >> char '{' >> spaces
        fs <- many1 parseStructField
        char '}' 
        return (Struct n fs)

parseNS :: Parsec String () Namespace
parseNS = lexeme lexer $ do
    string "namespace" >> spaces
    a <- identifier lexer
    return (Namespace a)

parseIM :: Parsec String () Import
parseIM = lexeme lexer $ do
    string "import" >> spaces
    a <- identifier lexer
    return (Import a)

parseTD :: Parsec String () TypeDeclaration
parseTD = lexeme lexer $ do
    string "type" >> spaces
    td <- parseName
    spaces >> char '=' >> spaces
    tn <- parseBT
    return (TypeDeclaration td tn)

parseTL :: Parsec String () TopLevelType
parseTL = lexeme lexer $ do
        try (parseIM >>= \v -> return (TL_IM v))
    <|> try (parseNS >>= \v -> return (TL_NS v))
    <|> try (parseTD >>= \v -> return (TL_TD v))
    <|> try (parseId64 >>= \v -> return (TL_ID v))
    <|> try (parseST >>= \v -> return (TL_ST v))
    <|> (parseET >>= \v -> return (TL_ET v))

parseTLS = lexeme lexer $ do
    spaces
    v <- (many1 (do
                    spaces
                    v' <- parseTL
                    spaces
                    return v'))
    return v

-- check of data structure

isId (TL_ID _) = True
isId _ = False

isNS (TL_NS _) = True
isNS _ = False

checkParsedData :: [TopLevelType] -> (Bool, String)
checkParsedData ast = 
    -- check namespace exactly once
    if length (filter isNS ast) /= 1 then (False, "one and only one namespace needed!")
        else (True, "")

parseECS :: String -> IO (Maybe [TopLevelType])
parseECS inS = do

    let res = parse parseTLS "" inS
    val <- case res of
                Right ast -> case checkParsedData ast of
                                            (True, "") -> return (Just ast)
                                            (False, msg) -> do
                                                                hPutStr stderr ("ecs_parser - language error: \n" ++ msg)
                                                                return Nothing
                Left parseError -> do
                                        hPutStr stderr ("ecs_parser - syntax error: \n" ++ (show parseError))
                                        return Nothing
    return val

-- utilities

cap1 :: String -> String
cap1 "" = ""
cap1 (head:tail) = Char.toUpper head : tail

low1 :: String -> String
low1 "" = ""
low1 (head:tail) = Char.toLower head : tail

-- create output for Haskell

hTN :: TopLevelType -> String
hTN tlt = let (TypeName n) = (typeName tlt) in cap1 n

hStructElemName :: TypeName -> ElemName -> String
hStructElemName (TypeName n) (ElemName c) = low1 n ++ cap1 c

hEnumElemName :: ConsName -> String
hEnumElemName (ConsName c) = cap1 c

hBT :: BaseType -> String
hBT t = case t of
    BT_PT Bool -> "Bool"    
    BT_PT Int8 -> "Int8"
    BT_PT Int16 -> "Int16"
    BT_PT Int32 -> "Int"
    BT_PT Int64 -> "Int64"
    BT_PT UInt8 -> "Word8"
    BT_PT UInt16 -> "Word16"
    BT_PT UInt32 -> "Word32"
    BT_PT UInt64 -> "Word64"
    BT_PT Float32 -> "Float"
    BT_PT Float64 -> "Double"
    BT_PT Text -> "Text"
    BT_PT Data -> "ByteString"
    BT_LT (List a) -> "[" ++ (hBT a) ++ "]"
    BT_TN (TypeName n) -> n
    _ -> error "Unknown Base Type in hBType!"

hFields :: TopLevelType -> String
hFields tlt = let
    ti = adtInfo tlt
    in case ti of
        Left es -> concat (L.intersperse (",\n    ") (map (\(en, bt) -> hStructElemName (typeName tlt) en ++ "::" ++ hBT bt) es)) ++ "\n"
        Right es -> concat (L.intersperse ("\n    | ") (map (\(cn, bts) -> hEnumElemName cn ++ (foldl (\b bt -> b ++ " " ++ (hBT bt)) "" bts)) es)) ++ "\n"

hTypeDef :: TopLevelType -> String
hTypeDef t = case t of
    TL_ET _ -> "data " ++ hTN t ++ " = " ++ hFields t ++ "    deriving (Eq, Read, Show)\n\n"
    TL_ST _ -> "data " ++ hTN t ++ " = " ++ hTN t ++ " {\n    " ++ hFields t ++ "}\n\n"
    TL_ID (Id64 tn i) -> "ct" ++ hTN t ++ " :: ComponentType " ++ hTN t ++ "\n"
                         ++ "ct" ++ hTN t ++ " = ComponentType 0x" ++ (showHex i "") ++ "\n\n"
    TL_TD (TypeDeclaration tn bt) -> "type " ++ hTN t ++ " = " ++ hBT bt ++ "\n\n"
    _ -> ""

hBTObjT :: BaseType -> (String, Maybe (String, String))   -- Obj Type, toObj, fromObj 
hBTObjT t = case t of
    BT_PT Bool -> ("ObjectBool ", Nothing)    
    BT_PT Int8 -> ("ObjectInt ", Just ("fromIntegral", "fromIntegral"))  
    BT_PT Int16 -> ("ObjectInt ", Just ("fromIntegral", "fromIntegral"))  
    BT_PT Int32 -> ("ObjectInt ", Just ("fromIntegral", "fromIntegral"))  
    BT_PT Int64 -> ("ObjectInt ", Nothing)
    BT_PT UInt8 -> ("ObjectInt ", Just ("fromIntegral", "fromIntegral"))
    BT_PT UInt16 -> ("ObjectInt ", Just ("fromIntegral", "fromIntegral"))
    BT_PT UInt32 -> ("ObjectInt ", Just ("fromIntegral", "fromIntegral"))
    BT_PT UInt64 -> ("ObjectInt ", Just ("fromIntegral", "fromIntegral"))
    BT_PT Float32 -> ("ObjectFloat ", Nothing)
    BT_PT Float64 -> ("ObjectDouble ", Nothing)
    BT_PT Text -> ("", Just ("toObj","fromObj"))
    BT_PT Data -> ("ObjectBinary ", Nothing)
    BT_LT (List a) -> ("ObjectArray ", Just ("map toObj","map fromObj"))
    BT_TN tn -> ("", Just ("toObj","fromObj"))

varT :: String -> BaseType -> Bool -> Bool -> String
varT v bt isR isTo = let
    ruleT = (snd (hBTObjT bt))
    rule = case ruleT of 
            Just (toR, fromR) -> if isTo then toR else fromR
            Nothing -> undefined
    in if isR && (M.isJust ruleT) then ("(" ++ rule ++ " " ++ v ++ ")") else v

objT :: String -> BaseType -> Bool -> Bool -> String
objT v bt isR isTo = fst (hBTObjT bt) ++ varT v bt isR isTo

varTs :: Show i => Bool -> Bool -> [(BaseType, i)] -> String
varTs isR isTo btis = concatMap (\(bt, i) -> " " ++ varT ("v" ++ show i) bt isR isTo) btis

objTs :: Show i => Bool -> Bool -> [(BaseType, i)] -> String
objTs isR isTo btis = concat (L.intersperse ", " (map (\(bt, i) -> objT ("v" ++ show i) bt isR isTo) btis))

hTLObjT :: TopLevelType -> String
hTLObjT tlt = let
    g tlt = let
        tis = adtInfo tlt
        in  "instance ComponentClass " ++ hTN tlt ++ " where\n" ++
            case tis of
                Left es -> let
                    c = hTN tlt
                    btis = zip (map snd es) [1..]
                    in ("    toObj (" ++ c ++ varTs False True btis ++ ") = ObjectArray [" ++ objTs True True btis ++ "]\n" ++
                       "    fromObj (ObjectArray [" ++ objTs False False btis ++ "]) = " ++ c ++ varTs True False btis ++ "\n")
                Right es -> 
                    concatMap (\((cn, bts), n) -> let
                        c = hEnumElemName cn
                        btis = zip bts [1..]
                        in ("    toObj (" ++ c ++ varTs False True btis ++ ") = ObjectArray [ObjectInt " ++ (show n) ++ ", ObjectArray [" ++ objTs True True btis ++ "]]\n") ) (zip es [0..])

                    ++
                                                        
                    concatMap (\((cn, bts), n) -> let
                        c = hEnumElemName cn
                        btis = zip bts [1..]
                        in ("    fromObj (ObjectArray [ObjectInt " ++ (show n) ++ ", ObjectArray [" ++ objTs False False btis ++ "]]) = " ++ c ++ varTs True False btis ++ "\n") ) (zip es [0..])
        ++ "\n"
    in case tlt of
        TL_ST _ -> g tlt
        TL_ET _ -> g tlt
        _ -> ""

-- create output for Rust

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
    BT_LT (List a) -> "Vec<" ++ (hBT a) ++ ">"
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
    TL_ID (Id64 tn i) -> "const CT_" ++ (map Char.toUpper (hTN t)) ++ ": u64 = 0x" ++ (showHex i "") ++ ";\n\n"
    TL_TD (TypeDeclaration _ bt) -> "type " ++ rTN t ++ " = " ++ rBT bt ++ ";\n\n"
    _ -> ""

rTLObjT v = ""

-- Main Program

showTLT :: [TopLevelType] -> String
showTLT l = foldl (\s tt -> s ++ (show tt) ++ "\n") "" l

data Options = Options  { optVerbose    :: Bool
                        , optInput      :: IO String
                        , optOutput     :: String -> IO ()
                        , optFilter     :: [TopLevelType] -> String
                        }

startOptions :: Options
startOptions = Options  { optVerbose    = False
                        , optInput      = getContents
                        , optOutput     = putStr
                        , optFilter     = showTLT
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInput = readFile arg })
            "<file>")
        "Input file"
 
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = writeFile arg })
            "<file>")
        "Output file"
 
    , Option "s" ["string"]
        (ReqArg
            (\arg opt -> return opt { optInput = return arg })
            "<string>")
        "Input string"

    , Option "g" ["generate"]
        (ReqArg
            (\arg opt -> return opt { optFilter = case arg of
                                                    "Haskell" -> (\l -> concatMap (\v -> hTypeDef v ++ hTLObjT v) l)
                                                    "Rust" -> (\l -> concatMap (\v -> rTypeDef v ++ rTLObjT v) l)
--                                                    "C++" -> (\l -> concatMap (\v -> rTypeDef v ++ rTLObjT v) l)
--                                                    "Python" -> (\l -> concatMap (\v -> rTypeDef v ++ rTLObjT v) l)
--                                                    "JavaScript" -> (\l -> concatMap (\v -> rTypeDef v ++ rTLObjT v) l)
                                                    _ -> showTLT })
            "<language>")
        "Generator"
 
    , Option "v" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.0.1"
                exitWith ExitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]


main = do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt RequireOrder options args
 
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions
    let Options { optVerbose = verbose
                , optInput = input
                , optOutput = output
                , optFilter = choice   } = opts
 
    input >>= \s -> (do
                        ast <- parseECS s
                        let s' = case ast of 
                                    Just a -> choice a
                                    Nothing -> ""
                        return s') >>= output


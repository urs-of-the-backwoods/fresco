--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2016 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: src/sinopia.hs
--

{-# LANGUAGE OverloadedStrings #-}

module Main where 

import System.Console.GetOpt
import System.Exit
import System.IO
import System.Environment

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Monoid

import Sinopia.Data
import Sinopia.Parser
import Sinopia.Util
import Sinopia.Haskell
import Sinopia.Rust
import Sinopia.JavaScript
import Sinopia.C

-- Main Program

showTLT :: [TopLevelType] -> T.Text
showTLT l = foldl (\s tt -> s <> ((T.pack . show) tt) <> "\n") "" l

data Options = Options  { optVerbose    :: Bool
                        , optInput      :: IO T.Text
                        , optOutput     :: T.Text -> IO ()
                        , optFilter     :: [TopLevelType] -> T.Text
                        , optName       :: T.Text
                        , optModule     :: T.Text
                        }

startOptions :: Options
startOptions = Options  { optVerbose    = False
                        , optInput      = T.getContents
                        , optOutput     = T.putStr
                        , optFilter     = showTLT
                        , optName       = ""
                        , optModule       = ""
                        }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "i" ["input"]
        (ReqArg
            (\arg opt -> return opt { optInput = T.readFile arg })
            "<file>")
        "Input file"
 
    , Option "o" ["output"]
        (ReqArg
            (\arg opt -> return opt { optOutput = T.writeFile arg })
            "<file>")
        "Output file"
 
    , Option "s" ["string"]
        (ReqArg
            (\arg opt -> return opt { optInput = return (T.pack arg) })
            "<string>")
        "Input string"

    , Option "f" ["string"]
        (ReqArg
            (\arg opt -> return opt { optName = (T.pack arg) })
            "<string>")
        "Filename String"


    , Option "m" ["string"]
        (ReqArg
            (\arg opt -> return opt { optModule = (T.pack arg) })
            "<string>")
        "Module String"

    , Option "g" ["generate"]
        (ReqArg
            (\arg opt -> do
--                print $ T.concat ["Hi: ", optName opt]
                return opt { optFilter = case arg of
                "Haskell" -> \l -> conversion hConvertible (optName opt) (optModule opt) l
                "JavaScript" -> \l -> conversion jsConvertible (optName opt) (optModule opt) l
                "C-cpp" -> \l -> conversion ciConvertible (optName opt) (optModule opt) l -- implementation
                "C-hpp" -> \l -> conversion cdConvertible (optName opt) (optModule opt) l -- definition
                _ -> showTLT })
            "<language>")
        "Generator"
 
    , Option "v" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.2.0"
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
                        let ast = parseAST s
                        s' <- case ast of 
                                Right a -> return $ choice a
                                Left error -> print (show error) >> return ""
                        return s') >>= output


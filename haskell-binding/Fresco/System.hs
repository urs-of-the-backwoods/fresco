--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015 - 2017 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: haskell/Fresco/System.hs
--

{-# LANGUAGE ForeignFunctionInterface, CPP #-}

-- | Helper functions for binding ffi, encoding, decoding via messagepack
module Fresco.System

where

import Data.ByteString.Lazy
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.ByteString (packCStringLen)

import Data.Binary.Serialise.CBOR

import Data.Either
import Data.Maybe

import Foreign
import Foreign.C
import Foreign.Ptr

import Fresco.Component

#ifdef UseWinDLLLoading
import System.Win32.DLL
#else
import System.Posix.DynamicLinker
#endif

import System.Environment
import System.IO.Unsafe
import Data.IORef

toMsg :: Serialise o => o -> ByteString
toMsg o = serialise o

fromMsg :: Serialise o => ByteString -> o
fromMsg bs = deserialise bs

-- helper functions

-- different callback functions and dynamic wrapper generation

type MsgFn = Ptr () -> Ptr CChar -> Word32 -> IO ()

foreign import ccall "dynamic" 
   mkMsgFn :: FunPtr MsgFn -> MsgFn
foreign import ccall "wrapper"
   mkMsgFnPtr :: MsgFn -> IO (FunPtr MsgFn)

type MsgEntityFn = Ptr () -> Word64 -> Ptr CChar -> Word32 -> IO Int32

foreign import ccall "dynamic" 
   mkMsgEntityFn :: FunPtr MsgEntityFn -> MsgEntityFn
foreign import ccall "wrapper"
   mkMsgEntityFnPtr :: MsgEntityFn -> IO (FunPtr MsgEntityFn)

callMsgEntityFn :: FunPtr MsgEntityFn -> Ptr () -> Word64 -> ByteString -> IO Int
callMsgEntityFn mf p ct msg = do
      let f = mkMsgEntityFn mf
      let dat = msg
      unsafeUseAsCStringLen' dat $ \(dat'1, dat'2) -> f p ct dat'1  dat'2 >>= \res -> return (fromIntegral res)
--      unsafeUseAsCStringLen' dat $ \(dat'1, dat'2) -> print "msgfun" >> print dat'1 >> print dat'2 >> f p dat'1  dat'2 >>= \res -> return (fromIntegral res)

type InitFunction = Ptr () -> IO Word32
foreign import ccall "dynamic" 
   mkInitFun :: FunPtr InitFunction -> InitFunction

callInitFunction :: FunPtr InitFunction -> Ptr () -> IO Int
callInitFunction ifp p = do
    let f = mkInitFun ifp
    res <- f p
    return (fromIntegral res)


-- Entity Interface

type EntityCreateFunction = Ptr CChar -> Word32 -> Ptr (Ptr ()) -> IO ()
foreign import ccall "dynamic" 
   mkEntityCreateFunction :: FunPtr EntityCreateFunction -> EntityCreateFunction

type EntityDestroyFunction = Ptr () -> IO ()
foreign import ccall "dynamic" 
   mkEntityDestroyFunction :: FunPtr EntityDestroyFunction -> EntityDestroyFunction

type EntityReadComponentFunction = Ptr () -> Word64 -> Ptr () -> FunPtr MsgFn -> IO ()
foreign import ccall "dynamic" 
   mkEntityReadComponentFunction :: FunPtr EntityReadComponentFunction -> EntityReadComponentFunction

type EntityWriteComponentFunction = Ptr ()  -> Word64 -> Ptr CChar -> Word32 -> IO ()
foreign import ccall "dynamic" 
   mkEntityWriteComponentFunction :: FunPtr EntityWriteComponentFunction -> EntityWriteComponentFunction

type EntityReadIdFunction = Ptr () -> Ptr () -> FunPtr MsgFn -> IO ()
foreign import ccall "dynamic"
  mkEntityReadIdFunction :: FunPtr EntityReadIdFunction -> EntityReadIdFunction

-- Object Lib System Interface

type ObjectLibSystemInitFunction = Ptr CChar -> Ptr (Ptr ()) -> IO ()
foreign import ccall "dynamic" 
   mkObjectLibSystemInitFunction :: FunPtr ObjectLibSystemInitFunction -> ObjectLibSystemInitFunction

type ObjectLibSystemAddEntityFunction = Ptr () -> Ptr () -> IO ()
foreign import ccall "dynamic" 
   mkObjectLibSystemAddEntityFunction:: FunPtr ObjectLibSystemAddEntityFunction -> ObjectLibSystemAddEntityFunction
   
type ObjectLibSystemRemoveEntityFunction = Ptr () -> Ptr () -> IO ()
foreign import ccall "dynamic" 
   mkObjectLibSystemRemoveEntityFunction:: FunPtr ObjectLibSystemRemoveEntityFunction -> ObjectLibSystemRemoveEntityFunction
   
type ObjectLibSystemShutdownFunction = Ptr () -> IO ()
foreign import ccall "dynamic" 
   mkObjectLibSystemShutdownFunction:: FunPtr ObjectLibSystemShutdownFunction -> ObjectLibSystemShutdownFunction
   
type ObjectLibSystemStepFunction = Ptr () -> IO ()
foreign import ccall "dynamic" 
   mkObjectLibSystemStepFunction:: FunPtr ObjectLibSystemStepFunction -> ObjectLibSystemStepFunction
   
-- Callback System Interface

type CallbackSystemInitFunction = Ptr (Ptr ()) -> IO ()
foreign import ccall "dynamic" 
   mkCallbackSystemInitFunction :: FunPtr CallbackSystemInitFunction -> CallbackSystemInitFunction

type CallbackSystemRegisterReceiverFunction = Ptr () -> Ptr () -> Word64 -> FunPtr MsgEntityFn -> IO ()
foreign import ccall "dynamic"
   mkCallbackSystemRegisterReceiverFunction :: FunPtr CallbackSystemRegisterReceiverFunction -> CallbackSystemRegisterReceiverFunction

type CallbackSystemShutdownFunction = Ptr () -> IO ()
foreign import ccall "dynamic" 
   mkCallbackSystemShutdownFunction:: FunPtr CallbackSystemShutdownFunction -> CallbackSystemShutdownFunction
   
type CallbackSystemStepFunction = Ptr () -> IO ()
foreign import ccall "dynamic" 
   mkCallbackSystemStepFunction:: FunPtr CallbackSystemStepFunction -> CallbackSystemStepFunction
   

data EntityInterface = EntityInterface {
                        eCreate :: EntityCreateFunction,
                        eRead :: EntityReadComponentFunction,
                        eWrite :: EntityWriteComponentFunction,
                        eId :: EntityReadIdFunction,
                        eDestroy :: EntityDestroyFunction,

                        olsInit :: ObjectLibSystemInitFunction,
                        olsAddEntity :: ObjectLibSystemAddEntityFunction,
                        olsRemoveEntity :: ObjectLibSystemRemoveEntityFunction,
                        olsShutdown :: ObjectLibSystemShutdownFunction,
                        olsStep :: ObjectLibSystemStepFunction,

                        cbsInit :: CallbackSystemInitFunction,
                        cbsRegisterReceiver :: CallbackSystemRegisterReceiverFunction,
                        cbsShutdown :: CallbackSystemShutdownFunction,
                        cbsStep :: CallbackSystemStepFunction
                      }


#ifdef UseWinDLLLoading
dynamicEI :: IORef EntityInterface
{-# NOINLINE dynamicEI #-}
dynamicEI = unsafePerformIO (do
    libname <- getEnv "INTONACO"
    dll <- loadLibrary libname


    ec <- getProcAddress dll "inEntityCreate"
    let ec' = mkEntityCreateFunction $ castPtrToFunPtr ec

    er <- getProcAddress dll "inEntityReadComponent"
    let er' = mkEntityReadComponentFunction $ castPtrToFunPtr er

    ew <- getProcAddress dll "inEntityWriteComponent"
    let ew' = mkEntityWriteComponentFunction $ castPtrToFunPtr ew

    ei <- getProcAddress dll "inEntityId"
    let ei' = mkEntityReadIdFunction $ castPtrToFunPtr ei

    ed <- getProcAddress dll "inEntityDestroy"
    let ed' = mkEntityDestroyFunction $ castPtrToFunPtr ed


    oli <- getProcAddress dll "inObjectLibSystemInit" 
    let oli' = mkObjectLibSystemInitFunction $ castPtrToFunPtr oli

    ola <- getProcAddress dll "inObjectLibSystemAddEntity" 
    let ola' = mkObjectLibSystemAddEntityFunction $ castPtrToFunPtr ola

    olr <- getProcAddress dll "inObjectLibSystemRemoveEntity" 
    let olr' = mkObjectLibSystemRemoveEntityFunction $ castPtrToFunPtr olr

    olu <- getProcAddress dll "inObjectLibSystemShutdown" 
    let olu' = mkObjectLibSystemShutdownFunction $ castPtrToFunPtr olu

    ols <- getProcAddress dll "inObjectLibSystemStep" 
    let ols' = mkObjectLibSystemStepFunction $ castPtrToFunPtr ols


    cbi <- getProcAddress dll "inCallbackSystemInit" 
    let cbi' = mkCallbackSystemInitFunction $ castPtrToFunPtr cbi

    cbr <- getProcAddress dll "inCallbackSystemRegisterReceiver" 
    let cbr' = mkCallbackSystemRegisterReceiverFunction $ castPtrToFunPtr cbr

    cbu <- getProcAddress dll "inCallbackSystemShutdown" 
    let cbu' = mkCallbackSystemShutdownFunction $ castPtrToFunPtr cbu

    cbs <- getProcAddress dll "inCallbackSystemStep" 
    let cbs' = mkCallbackSystemStepFunction $ castPtrToFunPtr cbs

    ref <- newIORef $ EntityInterface ec' er' ew' ei' ed'  oli' ola' olr' olu' ols'  cbi' cbr' cbu' cbs' 
    return ref
    )

  

#else
dynamicEI :: IORef EntityInterface
{-# NOINLINE dynamicEI #-}
dynamicEI = unsafePerformIO ( 
  do
    libname <- getEnv "INTONACO"
    dll <- dlopen libname [RTLD_NOW]

    ec <- dlsym dll "inEntityCreate"
    let ec' = mkEntityCreateFunction ec

    er <- dlsym dll "inEntityReadComponent"
    let er' = mkEntityReadComponentFunction er

    ew <- dlsym dll "inEntityWriteComponent"
    let ew' = mkEntityWriteComponentFunction ew

    ei <- dlsym dll "inEntityId"
    let ei' = mkEntityReadIdFunction ei

    ed <- dlsym dll "inEntityDestroy"
    let ed' = mkEntityDestroyFunction ed


    oli <- dlsym dll "inObjectLibSystemInit" 
    let oli' = mkObjectLibSystemInitFunction oli

    ola <- dlsym dll "inObjectLibSystemAddEntity" 
    let ola' = mkObjectLibSystemAddEntityFunction ola

    olr <- dlsym dll "inObjectLibSystemRemoveEntity" 
    let olr' = mkObjectLibSystemRemoveEntityFunction olr

    olu <- dlsym dll "inObjectLibSystemShutdown" 
    let olu' = mkObjectLibSystemShutdownFunction olu

    ols <- dlsym dll "inObjectLibSystemStep" 
    let ols' = mkObjectLibSystemStepFunction ols


    cbi <- dlsym dll "inCallbackSystemInit" 
    let cbi' = mkCallbackSystemInitFunction cbi

    cbr <- dlsym dll "inCallbackSystemRegisterReceiver" 
    let cbr' = mkCallbackSystemRegisterReceiverFunction cbr

    cbu <- dlsym dll "inCallbackSystemShutdown" 
    let cbu' = mkCallbackSystemShutdownFunction cbu

    cbs <- dlsym dll "inCallbackSystemStep" 
    let cbs' = mkCallbackSystemStepFunction cbs

    ref <- newIORef $ EntityInterface ec' er' ew' ei' ed'  oli' ola' olr' olu' ols'  cbi' cbr' cbu' cbs'
    return ref
  )
#endif

type CStringCLen i = (CString, i)

unsafeUseAsCStringLen' :: (Integral i) => ByteString -> (CStringCLen i -> IO a) -> IO a
unsafeUseAsCStringLen' str fn =
   unsafeUseAsCStringLen (toStrict str) (\(ptr, len) -> fn (ptr, fromIntegral len))

entityCreate :: ByteString -> IO (Ptr ())
entityCreate a1 =
  unsafeUseAsCStringLen' a1 $ \(a1'1, a1'2) -> 
  alloca $ \a2' -> 
  (do
    dei <- readIORef dynamicEI
    (eCreate dei) a1'1 a1'2 a2') >>
  peek  a2' >>= \a2'' -> 
  return (a2'')

entityDestroy :: Ptr () -> IO ()
entityDestroy a1 =
  (do
    dei <- readIORef dynamicEI
    (eDestroy dei) a1) >>
  return ()


entityId :: Ptr () -> IO ByteString
entityId ep = do
  dei <- readIORef dynamicEI
  pbs <- newIORef undefined
  fp <- (mkMsgFnPtr (\_ p len -> do
    bs <- packCStringLen (p, fromIntegral len)
    writeIORef pbs bs
    return ()
    ))
  (eId dei) ep nullPtr fp
  bs <- readIORef pbs
  return (fromStrict bs)

entityWrite :: (Ptr ()) -> Word64 -> ByteString -> IO ()
entityWrite a1 a2 a3 =
  unsafeUseAsCStringLen' a3 $ \(a3'1, a3'2) -> 
  (do
    dei <- readIORef dynamicEI
    (eWrite dei) a1 a2 a3'1 a3'2) >>
  return ()

entityRead :: Ptr () -> Word64 -> IO ByteString
entityRead ep ct = do
  dei <- readIORef dynamicEI
  pbs <- newIORef undefined
  fp <- (mkMsgFnPtr (\_ p len -> do
    bs <- packCStringLen (p, fromIntegral len)
    writeIORef pbs bs
    return ()
    ))
  (eRead dei) ep ct (nullPtr) fp
  bs <- readIORef pbs
  return (fromStrict bs)

objectLibSystemCreate :: ByteString -> IO (Ptr ())
objectLibSystemCreate a1 =
  unsafeUseAsCStringLen' a1 $ \(a1'1, a1'2) -> 
  alloca $ \a2' -> 
  (do
    dei <- readIORef dynamicEI
    (olsInit dei) a1'1 a2') >>
  peek  a2'>>= \a2'' -> 
  return (a2'')

objectLibSystemAddEntity :: (Ptr ()) -> (Ptr ()) -> IO ()
objectLibSystemAddEntity a1 a2 =
  let {a1' = id a1; a2' = id a2} in 
  (do
    dei <- readIORef dynamicEI
    (olsAddEntity dei) a1' a2') >>
  return ()

objectLibSystemStep :: (Ptr ()) -> IO ()
objectLibSystemStep a1 =
  let {a1' = id a1} in 
  (do
    dei <- readIORef dynamicEI
    (olsStep dei) a1') >>
  return ()

objectLibSystemShutdown :: (Ptr ()) -> IO ()
objectLibSystemShutdown a1 =
  let {a1' = id a1} in 
  (do
    dei <- readIORef dynamicEI
    (olsShutdown dei) a1') >>
  return ()


callbackSystemCreate :: IO ((Ptr ()))
callbackSystemCreate =
  alloca $ \a1' -> 
  (do
    dei <- readIORef dynamicEI
    (cbsInit dei) a1') >>
  peek  a1'>>= \a1'' -> 
  return (a1'')

callbackSystemRegisterReceiver :: (Ptr ()) -> (Ptr ()) -> (Word64) -> (FunPtr (Ptr () -> Word64 -> Ptr CChar -> Word32 -> IO Int32)) -> IO ()
callbackSystemRegisterReceiver a1 a2 a3 a4 =
  let {a1' = id a1} in 
  let {a2' = id a2} in 
  let {a3' = fromIntegral a3} in 
  let {a4' = id a4} in 
  (do
    dei <- readIORef dynamicEI
    (cbsRegisterReceiver dei) a1' a2' a3' a4') >>
  return ()

callbackSystemStep :: (Ptr ()) -> IO ()
callbackSystemStep a1 =
  let {a1' = id a1} in 
  (do
    dei <- readIORef dynamicEI
    (cbsStep dei) a1') >>
  return ()

callbackSystemShutdown :: (Ptr ()) -> IO ()
callbackSystemShutdown a1 =
  let {a1' = id a1} in 
  (do
    dei <- readIORef dynamicEI
    (cbsShutdown dei) a1') >>
  return ()



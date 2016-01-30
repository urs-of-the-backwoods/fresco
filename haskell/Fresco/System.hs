--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2016 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: haskell/Fresco/System.hs
--

{-# LANGUAGE ForeignFunctionInterface #-}

-- | Helper functions for binding ffi, encoding, decoding via messagepack
module Fresco.System

where

import Data.ByteString
import Data.ByteString.Unsafe
import qualified Data.ByteString.Lazy as BL
import Data.MessagePack
import Data.Either
import Data.Maybe
import Data.Serialize

import Foreign
import Foreign.C
import Foreign.Ptr

import Fresco.Component

import System.Posix.DynamicLinker
import System.Environment
import System.IO.Unsafe
import Data.IORef

toMsg :: ComponentClass o => o -> ByteString
toMsg o = encode (toObj o)

fromMsg :: ComponentClass o => ByteString -> Maybe o
fromMsg bs = case decode bs of 
                Right o -> Just $ fromObj o
                _ -> Nothing

-- helper functions

type MsgFunction = Ptr () -> Ptr CChar -> CInt -> IO CInt
foreign import ccall "dynamic" 
   mkMsgFun :: FunPtr MsgFunction -> MsgFunction
foreign import ccall "wrapper"
   mkMsgFunPtr :: MsgFunction -> IO (FunPtr MsgFunction)

callMsgFunction :: FunPtr MsgFunction -> Ptr () -> ByteString -> IO Int
callMsgFunction mf p msg = do
      let f = mkMsgFun mf
      let dat = msg
      unsafeUseAsCStringLen' dat $ \(dat'1, dat'2) -> f p dat'1  dat'2 >>= \res -> return (fromIntegral res)
--      unsafeUseAsCStringLen' dat $ \(dat'1, dat'2) -> print "msgfun" >> print dat'1 >> print dat'2 >> f p dat'1  dat'2 >>= \res -> return (fromIntegral res)

type InitFunction = Ptr () -> IO CInt
foreign import ccall "dynamic" 
   mkInitFun :: FunPtr InitFunction -> InitFunction

callInitFunction :: FunPtr InitFunction -> Ptr () -> IO Int
callInitFunction ifp p = do
    let f = mkInitFun ifp
    res <- f p
    return (fromIntegral res)



-- Entity Interface

type EntityCreateFunction = ((Ptr CChar) -> (CInt -> ((Ptr (Ptr ())) -> (IO ())))) 
foreign import ccall "dynamic" 
   mkEntityCreateFunction :: FunPtr EntityCreateFunction -> EntityCreateFunction

type EntitySetFunction = ((Ptr CChar) -> (CInt -> ((Ptr ()) -> (IO ()))))
foreign import ccall "dynamic" 
   mkEntitySetFunction :: FunPtr EntitySetFunction -> EntitySetFunction


-- pub extern "C" fn callback_system_create(pp: *mut *mut CallbackSystem) {
type CallbackSystemCreateFunction = ((Ptr (Ptr ())) -> (IO ()))
foreign import ccall "dynamic" 
   mkCallbackSystemCreateFunction :: FunPtr CallbackSystemCreateFunction -> CallbackSystemCreateFunction

-- pub extern "C" fn callback_system_register_receiver (cbs: *mut CallbackSystem, ep: EntityPointer, ct: u64, mfp: MessageFunctionPointer) {
type CallbackSystemRegisterReceiverFunction = ((Ptr ()) -> ((Ptr ()) -> (CULong -> ((FunPtr ((Ptr ()) -> ((Ptr CChar) -> (CInt -> (IO CInt))))) -> (IO ())))))
foreign import ccall "dynamic"
   mkCallbackSystemRegisterReceiverFunction :: FunPtr CallbackSystemRegisterReceiverFunction -> CallbackSystemRegisterReceiverFunction

-- pub extern "C" fn callback_system_step(cbs: *mut CallbackSystem) {
type CallbackSystemStepFunction = ((Ptr ()) -> (IO ()))
foreign import ccall "dynamic" 
   mkCallbackSystemStepFunction:: FunPtr CallbackSystemStepFunction -> CallbackSystemStepFunction

data EntityInterface = EntityInterface {
                        efCreate :: EntityCreateFunction,
                        efSet :: EntitySetFunction,
                        cbsfCreate :: CallbackSystemCreateFunction,
                        cbsfRegisterReceiver :: CallbackSystemRegisterReceiverFunction,
                        cbsfStep :: CallbackSystemStepFunction
                      }

dynamicEI :: IORef EntityInterface
{-# NOINLINE dynamicEI #-}
dynamicEI = unsafePerformIO ( 
  do
    libname <- getEnv "INTONACO_LIB"
    dll <- dlopen libname [RTLD_NOW]
    efc <- dlsym dll "entity_create"
    let efc' = mkEntityCreateFunction efc
    efs <- dlsym dll "entity_set" 
    let efs' = mkEntitySetFunction efs

    cbc <- dlsym dll "callback_system_create" 
    let cbc' = mkCallbackSystemCreateFunction cbc

    cbr <- dlsym dll "callback_system_register_receiver" 
    let cbr' = mkCallbackSystemRegisterReceiverFunction cbr

    cbs <- dlsym dll "callback_system_step" 
    let cbs' = mkCallbackSystemStepFunction cbs


    ref <- newIORef $ EntityInterface efc' efs' cbc' cbr' cbs'
    return ref
  )


type CStringCLen i = (CString, i)

unsafeUseAsCStringLen' :: (Integral i) => ByteString -> (CStringCLen i -> IO a) -> IO a
unsafeUseAsCStringLen' str fn =
   unsafeUseAsCStringLen str (\(ptr, len) -> fn (ptr, fromIntegral len))

entityCreate :: (ByteString) -> IO ((Ptr ()))
entityCreate a1 =
  unsafeUseAsCStringLen' a1 $ \(a1'1, a1'2) -> 
  alloca $ \a2' -> 
  (do
    dei <- readIORef dynamicEI
    (efCreate dei) a1'1  a1'2 a2') >>
  peek  a2' >>= \a2'' -> 
  return (a2'')

{-# LINE 46 "CFunctions.chs" #-}


entitySet :: (ByteString) -> (Ptr ()) -> IO ()
entitySet a1 a2 =
  unsafeUseAsCStringLen' a1 $ \(a1'1, a1'2) -> 
  let {a2' = id a2} in 
  (do
    dei <- readIORef dynamicEI
    (efSet dei) a1'1  a1'2 a2') >>
  return ()

{-# LINE 53 "CFunctions.chs" #-}

callbackSystemCreate :: IO ((Ptr ()))
callbackSystemCreate =
  alloca $ \a1' -> 
  (do
    dei <- readIORef dynamicEI
    (cbsfCreate dei) a1') >>
  peek  a1'>>= \a1'' -> 
  return (a1'')

{-# LINE 59 "CFunctions.chs" #-}


callbackSystemRegisterReceiver :: (Ptr ()) -> (Ptr ()) -> (Word64) -> (FunPtr (Ptr () -> Ptr CChar -> CInt -> IO CInt)) -> IO ()
callbackSystemRegisterReceiver a1 a2 a3 a4 =
  let {a1' = id a1} in 
  let {a2' = id a2} in 
  let {a3' = fromIntegral a3} in 
  let {a4' = id a4} in 
  (do
    dei <- readIORef dynamicEI
    (cbsfRegisterReceiver dei) a1' a2' a3' a4') >>
  return ()

{-# LINE 68 "CFunctions.chs" #-}


callbackSystemStep :: (Ptr ()) -> IO ()
callbackSystemStep a1 =
  let {a1' = id a1} in 
  (do
    dei <- readIORef dynamicEI
    (cbsfStep dei) a1') >>
  return ()

{-# LINE 74 "CFunctions.chs" #-}



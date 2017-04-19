--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015 - 2017 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: haskell/Fresco/Entity.hs
--

{-# Language OverloadedStrings, ExistentialQuantification, FlexibleInstances #-}

-- | Entity of the Entity ComponentType System for Fresco Haskell Binding
module Fresco.Entity (

-- * EntityData Type
--   Entities are a kind of simplified extensible record system. They are basically a Map from ComponentType (64 bit id) to a data item with 
--   ComponentClass Typeclass. Basic entities are non-mutable but their exists the entity reference.

--  EntityData,
  (#:),
--  (#!),
--  (#),

-- * Entity Type
--   The ERef type, which puts an EntityData into
--   an IORef and serves as mutable data structure.
--   In HGamer3D those ERefs are also used as thread-safe communication vehicle towards the C/C++ implementation of multimedia functionality.

  Entity (..),
  newE,
  delE,
  idE,

 -- readE,
  readC,
  updateC,
  setC,
 -- _setC',

 ObjectLibSystem (..),
 createOLS,
 stepOLS,
 addEntityOLS,

 CallbackSystem (..),
 createCBS,
 stepCBS,
 registerReceiverCBS,

)
where

import Data.Maybe
import Data.ByteString.Lazy
import Data.ByteString (packCStringLen)
import qualified Data.Map as M
import Data.IORef

import Control.Concurrent
import Control.Applicative
import Foreign
import Foreign.C

import Data.Binary.Serialise.CBOR
import Data.Binary.Serialise.CBOR.Term

import Fresco.System
import Fresco.Component

import Numeric (showHex)


-- | EntityData, a simple non-mutable record type, implemented as Map
type EntityData = M.Map Word64 Component

-- | pair builder for nice construction syntax, allows [ ct #: val, ...] syntax
(#:) :: Serialise a => ComponentType a -> a -> (Word64, Component)
(ComponentType c) #: val = (c, toMsg val)

-- | Builder for entities, allows newE = entity [ct #: val, ...] syntax
entityData :: [(Word64, Component)] -> EntityData
entityData clist = M.fromList clist

-- | does the entity have the ComponentType
(#?) :: EntityData -> ComponentType a -> Bool
e #? (ComponentType c) = Prelude.elem c $ M.keys e

-- | get the ComponentType, throws exception, if ComponentType not present
(#!) :: Serialise a => EntityData -> ComponentType a -> a
e #! (ComponentType c) = fromJust $ M.lookup c e >>= fromMsg

-- | get the ComponentType as an maybe, in case wrong type
(#) :: Serialise a => EntityData -> ComponentType a -> Maybe a
e # (ComponentType c) = M.lookup c e >>= fromMsg

-- | modification function, throws exception, if ComponentType not present
updateDataC :: Serialise a => EntityData -> ComponentType a -> (a -> a) -> EntityData
updateDataC e c'@(ComponentType c) f = M.insert c ((toMsg . f) (e #! c')) e

-- | modification function, sets entity ComponentType, needed for events
setDataC :: Serialise a => EntityData -> ComponentType a -> a -> EntityData
setDataC e (ComponentType c) val = M.insert c (toMsg val) e


data CallbackSystem = CallbackSystem (Ptr ())

createCBS :: IO CallbackSystem
createCBS = do
  cbs <- callbackSystemCreate
  return $ CallbackSystem cbs

stepCBS :: CallbackSystem -> IO ()
stepCBS (CallbackSystem cbs) = callbackSystemStep cbs


registerReceiverCBS :: Serialise a => CallbackSystem -> Entity -> ComponentType a -> (a -> IO ()) -> IO ()
registerReceiverCBS (CallbackSystem cbs) (Entity ep) (ComponentType ct) f = do
  -- MsgFunction: Ptr () -> CULong -> Ptr CChar -> CInt -> IO CInt
  let f' = \_ _ cdata len -> do
                                bs <- packCStringLen (cdata, fromIntegral len)
                                let c = fromJust(fromMsg (fromStrict bs))
                                f c
                                return 0
  mf <- mkMsgEntityFnPtr f'
  callbackSystemRegisterReceiver cbs ep ct mf
  return ()

data ObjectLibSystem = ObjectLibSystem (Ptr ())

createOLS :: IO ObjectLibSystem
createOLS = do 
  ols <- objectLibSystemCreate "GIORNATA\0"
  return $ ObjectLibSystem ols

stepOLS :: ObjectLibSystem -> IO ()
stepOLS (ObjectLibSystem ols) = objectLibSystemStep ols

addEntityOLS :: ObjectLibSystem -> Entity -> IO ()
addEntityOLS (ObjectLibSystem ols) (Entity ep) = objectLibSystemAddEntity ols ep

-- References to Entities

-- besides Entity, we need atomic references to entities, we call them ERef
-- ERefs also have listeners for updates

-- Listener Map, for each k, manages a map of writers, writers geting the old and the new value after a change

-- type Listeners = IORef (M.Map Word64 [EntityData -> EntityData -> IO ()])
type Listeners = ()

-- | ERef, composable objects, referenced Entities with listeners
data Entity = Entity (Ptr ()) deriving (Eq)

msgFromE :: [(Word64, Component)] -> Component
msgFromE ed = let 
  bs = Prelude.map (\(a, b) -> Data.ByteString.Lazy.concat [toMsg a, toMsg b]) ed 
  in Data.ByteString.Lazy.concat bs

prettyPrint :: ByteString -> String
prettyPrint = Prelude.concat . Prelude.map (flip showHex " ") . unpack

-- | creates an Entity
newE :: [(Word64, Component)] -> IO Entity
newE inlist = do
     let msg = msgFromE inlist
     ep <- entityCreate msg
     return $ Entity ep

-- | destroys an Entity
delE :: Entity -> IO ()
delE (Entity ep) = entityDestroy ep

-- | gets id of an Entity
idE :: Entity -> IO ByteString
idE (Entity ep) = entityId ep

-- | reads one ComponentType, throws exception, if ComponentType not present, or wrong type
readC :: Serialise a => Entity -> ComponentType a -> IO a
readC (Entity ep) (ComponentType ct) = do
  bs <- entityRead ep ct
  return (fromMsg bs)

-- | updates one ComponentType
updateC :: Serialise a => Entity -> ComponentType a -> (a -> a) -> IO ()
updateC er@(Entity ep) c f = do
  val <- readC er c
  let val' = f val
  setC er c val'
  return ()

-- | sets one ComponentType
setC :: Serialise a => Entity -> ComponentType a -> a -> IO ()
setC er@(Entity ep) (ComponentType ct) val = do
--        let d = Data.ByteString.Lazy.concat [serialise (TInteger (fromIntegral ct)), toMsg val]
        entityWrite ep ct (toMsg val)
        return ()



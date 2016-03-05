--
--  Fresco Framework for Multi-Language Programming
--  Copyright 2015-2016 Peter Althainz
--    
--  Distributed under the Apache License, Version 2.0
--  (See attached file LICENSE or copy at 
--  http:--www.apache.org/licenses/LICENSE-2.0)
-- 
--  file: haskell/Fresco/Entity.hs
--

{-# Language ExistentialQuantification, FlexibleInstances #-}

-- | Entity of the Entity ComponentType System for Fresco Haskell Binding
module Fresco.Entity (

-- * EntityData Type
--   Entities are a kind of simplified extensible record system. They are basically a Map from ComponentType (64 bit id) to a data item with 
--   ComponentClass Typeclass. Basic entities are non-mutable but their exists the entity reference.

  EntityData,
  (#:),
  (#!),
  (#),

-- * Entity Type
--   The ERef type, which puts an EntityData into
--   an IORef and serves as mutable data structure.
--   In HGamer3D those ERefs are also used as thread-safe communication vehicle towards the C/C++ implementation of multimedia functionality.

  Entity (..),
  newE,
 -- readE,
  readC,
  updateC,
  setC,
 -- _setC',

 CallbackSystem (..),
 createCBS,
 stepCBS,
 registerReceiverCBS,
-- * Listener
-- Mechanism to register listener on ComponentTypes of ERef

--  addListener,
--  clearListeners

)
where

import Data.Maybe
import Data.ByteString
import qualified Data.Map as M
import Data.IORef

import Control.Concurrent
import Control.Applicative
import Foreign
import Foreign.C

import Data.MessagePack
import Data.Serialize

import Fresco.System
import Fresco.Component


-- | EntityData, a simple non-mutable record type, implemented as Map
type EntityData = M.Map Word64 Component

-- | pair builder for nice construction syntax, allows [ ct #: val, ...] syntax
(#:) :: ComponentClass a => ComponentType a -> a -> (Word64, Component)
(ComponentType c) #: val = (c, toMsg val)

-- | Builder for entities, allows newE = entity [ct #: val, ...] syntax
entityData :: [(Word64, Component)] -> EntityData
entityData clist = M.fromList clist

-- | does the entity have the ComponentType
(#?) :: EntityData -> ComponentType a -> Bool
e #? (ComponentType c) = Prelude.elem c $ M.keys e

-- | get the ComponentType, throws exception, if ComponentType not present
(#!) :: ComponentClass a => EntityData -> ComponentType a -> a
e #! (ComponentType c) = fromJust $ M.lookup c e >>= fromMsg

-- | get the ComponentType as an maybe, in case wrong type
(#) :: ComponentClass a => EntityData -> ComponentType a -> Maybe a
e # (ComponentType c) = M.lookup c e >>= fromMsg

-- | modification function, throws exception, if ComponentType not present
updateDataC :: ComponentClass a => EntityData -> ComponentType a -> (a -> a) -> EntityData
updateDataC e c'@(ComponentType c) f = M.insert c ((toMsg . f) (e #! c')) e

-- | modification function, sets entity ComponentType, needed for events
setDataC :: ComponentClass a => EntityData -> ComponentType a -> a -> EntityData
setDataC e (ComponentType c) val = M.insert c (toMsg val) e


-- 2nd level listener

data CallbackSystem = CallbackSystem (Ptr ())

createCBS :: IO CallbackSystem
createCBS = do
  cbs <- callbackSystemCreate
  return $ CallbackSystem cbs

stepCBS :: CallbackSystem -> IO ()
stepCBS (CallbackSystem cbs) = callbackSystemStep cbs


-- setC :: ComponentClass a => Entity -> ComponentType a -> a -> IO ()

registerReceiverCBS :: ComponentClass a => CallbackSystem -> Entity -> ComponentType a -> (a -> IO ()) -> IO ()
registerReceiverCBS (CallbackSystem cbs) (Entity _ _ ep) (ComponentType ct) f = do
  -- MsgFunction: Ptr () -> Ptr CChar -> CInt -> IO CInt
  let f' = \_ _ cdata len -> do
                                bs <- packCStringLen (cdata, fromIntegral len)
                                let c = fromJust(fromMsg bs)
                                f c
                                return 0
  mf <- mkMsgFunPtr f'
  callbackSystemRegisterReceiver cbs ep ct mf
  return ()




-- References to Entities

-- besides Entity, we need atomic references to entities, we call them ERef
-- ERefs also have listeners for updates

-- Listener Map, for each k, manages a map of writers, writers geting the old and the new value after a change

type Listeners = IORef (M.Map Word64 [EntityData -> EntityData -> IO ()])

-- | ERef, composable objects, referenced Entities with listeners
data Entity = Entity (IORef EntityData) Listeners (Ptr ()) deriving (Eq)




-- | Add an action (IO function), which will be executed when value of ComponentType is changed
addListener :: Entity -> ComponentType a -> (EntityData -> EntityData -> IO ()) -> IO ()
addListener (Entity _ tls ep) (ComponentType c) l = atomicModifyIORef tls (\m -> let
            l' = case M.lookup c m of
                           Just ol -> ol ++ [l]
                           Nothing -> [l]
            in (M.insert c l' m, ()))

-- | Clear all listeners from Entity
clearListeners :: Entity -> IO ()
clearListeners (Entity _ tls ep) = atomicWriteIORef tls (M.fromList [])

fireListeners :: Entity -> ComponentType a -> EntityData -> EntityData -> IO ()
fireListeners (Entity _ tls ep) (ComponentType c) val val' = do
              ls <- readIORef tls
              case M.lookup c ls of
                   Just l -> mapM (\f -> f val val') l >> return ()
                   Nothing -> return ()




msgFromE :: EntityData -> Component
msgFromE ed = let 
  pairs = (M.toList ed)               -- [(Word64, Component)]
  bs = Prelude.concatMap (\(a, b) -> [encode (ObjectUInt (fromIntegral a)), b]) pairs
  in Data.ByteString.concat bs

msgFromC :: ComponentClass a => ComponentType a -> EntityData -> Component
msgFromC (ComponentType u) e = let
  d = fromJust $ M.lookup u e
  in Data.ByteString.concat [encode (ObjectUInt (fromIntegral u)), d]

-- | creates an Entity
newE :: [(Word64, Component)] -> IO Entity
newE inlist = do
     let e = entityData inlist
     te <- newIORef e
     tl <- newIORef (M.fromList [])
     ep <- entityCreate (msgFromE e)
     return $ Entity te tl ep

-- | reads the EntityData from an Entity
readE :: Entity -> IO EntityData
readE (Entity te _ ep) = readIORef te

-- | reads one ComponentType, throws exception, if ComponentType not present, or wrong type
readC :: ComponentClass a => Entity -> ComponentType a -> IO a
readC (Entity te _ ep) (ComponentType ct) = do
  edat <- entityGetData ep ct
  bs <- entityDataRead edat
  entityDataRelease edat
  return (fromJust (fromMsg bs))

-- | updates one ComponentType
updateC :: ComponentClass a => Entity -> ComponentType a -> (a -> a) -> IO ()
updateC er@(Entity te tl ep) c f = do
        (e, e') <- atomicModifyIORef te (\olde -> let
                    newe = updateDataC olde c f
                    in (newe, (olde, newe)))
        entitySet (msgFromC c e) ep
        fireListeners er c e e'
        return ()

-- | sets one ComponentType
setC :: ComponentClass a => Entity -> ComponentType a -> a -> IO ()
setC er@(Entity te tl ep) c val = do
        (e, e') <- atomicModifyIORef te (\olde -> let
                    newe = setDataC olde c val
                    in (newe, (olde, newe)))
        entitySet (msgFromC c e) ep
        fireListeners er c e e'
        return ()

-- | sets one ComponentType as Component
_setC' :: Entity -> Word64 -> Component -> IO ()
_setC' er@(Entity te tls ep) c val = do
        (e, e') <- atomicModifyIORef te (\olde -> let
                    newe = M.insert c val olde
                    in (newe, (olde, newe)))
        ls <- readIORef tls
        case M.lookup c ls of
             Just l -> mapM (\f -> f e e') l >> return ()
             Nothing -> return ()
        return ()


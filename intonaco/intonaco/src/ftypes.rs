//
//  Fresco Framework for Multi-Language Programming
//  Copyright 2015-2016 Peter Althainz
//    
//  Distributed under the Apache License, Version 2.0
//  (See attached file LICENSE or copy at 
//  http://www.apache.org/licenses/LICENSE-2.0)
// 
//  file: intonacto/src/ftypes.rs
//

use entity::{EntityPointer};

//
// C-Interface
//

// Fresco types
//

pub type FrMsg = *const u8;
pub type FrMsgLength = u32;

pub type FrComponentType = u64;
pub type FrItemType = FrComponentType;
pub type FrPropertyType = FrComponentType;

// represents opaque pointer to external entity
pub enum FrItemStruct {}
unsafe impl Send for FrItemStruct {}
pub type FrItem = *const FrItemStruct;

pub type FrMessageFn = extern "C" fn (item: FrItem, msg: FrMsg, ml: FrMsgLength); 
pub type FrMessageFn2 = extern "C" fn (item: EntityPointer, ct: FrPropertyType, msg: FrMsg, ml: FrMsgLength); 



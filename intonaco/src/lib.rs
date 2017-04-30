//
//  Fresco Framework for Multi-Language Programming
//  Copyright 2015-2016 Peter Althainz
//    
//  Distributed under the Apache License, Version 2.0
//  (See attached file LICENSE or copy at 
//  http://www.apache.org/licenses/LICENSE-2.0)
// 
//  file: intonacto/src/lib.rs
//

extern crate cbor;
extern crate libc;
extern crate crossbeam;
extern crate libloading;
extern crate snowflake;
extern crate rustc_serialize;

pub mod ftypes;
pub mod component;
pub mod entity;
pub mod thread_guard;
pub mod system;
pub mod cif;

#[macro_use] 
extern crate lazy_static;


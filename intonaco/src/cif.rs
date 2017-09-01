//
//  Fresco Framework for Multi-Language Programming
//  Copyright 2015-2016 Peter Althainz
//    
//  Distributed under the Apache License, Version 2.0
//  (See attached file LICENSE or copy at 
//  http://www.apache.org/licenses/LICENSE-2.0)
// 
//  file: intonacto/src/cif.rs
//

use std;
use std::io::{Cursor};
use std::mem;
use libc;
use std::env::vars;
use std::ffi::CStr;
use snowflake::ProcessUniqueId;
use cbor::{Encoder};

#[cfg(unix)]
use libloading::os::unix::{Library, Symbol};
#[cfg(windows)]
use libloading::os::windows::{Library, Symbol};


use component;
use cbor::{Config, GenericDecoder, DecodeError, value};
use entity::{Entity, EntityPointer};
use system::{System, ObjectLibSystem, CallbackSystem};
use ftypes::{FrMsg, FrMsgLength, FrComponentType, FrItemType, FrPropertyType, FrItem, FrMessageFn, FrMessageFn2};


//
// C-Interface
//

/*
    void (*inEntityCreate) (FrMsg m, FrMsgLength l, FrEntity *e);                // Msg contains a CBOR array from arrays [u64, bs] 
    void (*inEntityReadComponent) (FrEntity e, FrComponentType ct, FrMessageFn f);               // also names are "Entity", read write works per component
    void (*inEntityWriteComponent) (FrEntity e, FrComponentType ct, FrMsg m, FrMsgLength l);
    void (*inEntityReadId) (FrEntity e, FrItem it, FrMessageFn f);  
    void (*inEntityDestroy) (FrEntity e);

    void (*inObjectLibSystemInit) (FrGiornataEnv g, msgPointer m, msgLength l, FrSystem *ps);       // Msg contains specific system creation parameters
    void (*inObjectLibSystemAddEntity) (FrSystem s, FrEntity e);
    void (*inObjectLibSystemRemoveEntity) (FrSystem s, FrEntity e);
    void (*inObjectLibSystemShutdown) (FrSystem s);
    void (*inObjectLibSystemStep) (FrSystem s);                                    // runs one cycle of system (control over Thread needed)

    void (*inCallbackSystemInit) (FrSystem *ps);
    void (*inCallbackSystemRegisterReceiver) (FrSystem s, FrEntity e, FrComponentType ct, FrMessageFn2 f);  
    void (*inCallbackSystemShutdown) (FrSystem s);
    void (*inCallbackSystemStep) (FrSystem s);

*/


// Entities

#[no_mangle] 
pub extern "C" fn inEntityCreate(data: *const u8, len: u32, pp: *mut EntityPointer) 
{
    let mut cts:Vec<(u64, Vec<u8>)> = Vec::new();
    if len > 0 {
        let dv = component::vec_from_c_char_p(data, len);
        let mut reader = Cursor::new(dv);
        let mut d = GenericDecoder::new(Config::default(), reader);

        loop {
            match d.value() {
                Ok(value::Value::U64(u)) => {
                    match d.value() {
                        Ok(value::Value::Bytes(v)) => match v {
                            value::Bytes::Bytes(v) => cts.push((u, v)),
                            value::Bytes::Chunks(l) => cts.push((u, l.front().unwrap().clone())),
                        },
                        Ok(other) => panic!("inEntityCreate, bytes not following u64: {:?}", other),
                        Err(err) => panic!("inEntityCreate, error after u64: {:?}", err),
                    }
                },
                Ok(value::Value::I64(u)) => panic!("inEntityCreate, found i64 instead of u64: {:?}", u),
                _ => break,
            }
        }     
    }

    let ep = Entity::to_ptr(Entity::new(cts));

    unsafe {
      *pp = ep;
    }
}


#[no_mangle] 
pub extern "C" fn inEntityDestroy(ep: EntityPointer)
{
    let e = Entity::from_ptr(ep);
    // e goes out of scope here and destroys, the box and e
}

#[no_mangle] 
pub extern "C" fn inEntityReadComponent(ep: EntityPointer, ct: u64, ip: FrItem, rcb: FrMessageFn) {
    let av = Entity::do_with(ep, (|en| {
        return en.get(ct);
    }));
    unsafe {
        rcb(ip, av.as_ptr(), av.len() as u32);
    }
}

#[no_mangle] 
pub extern "C" fn inEntityWriteComponent(ep: EntityPointer, ct: u64, p: *const u8, l: u32) {
    Entity::do_with(ep, (|en| {
        let v = component::vec_from_c_char_p(p, l);
        en.set(ct, v);    
    }));
}

#[no_mangle] 
pub extern "C" fn inEntityId(ep: EntityPointer, ip: FrItem, rcb: FrMessageFn) {

    let id_bs = Entity::do_with(ep, (|en| {
        return en.id();
    }));

    // encode to cbor as data, bytestring
    let id_u8 = unsafe { mem::transmute::<ProcessUniqueId, [u8; 16]>(id_bs) };
    let mut buf = vec![];      
    let mut enc = Encoder::new(buf);
    enc.bytes(&id_u8);
    let id_bs2 = &enc.into_writer();               

    unsafe {
        // rcb(ip, id_bs2.as_ptr(), id_bs2.len() as u32);
        rcb(ip, id_u8.as_ptr(), id_u8.len() as u32);
    }
}

// ObjectLibSystem

#[no_mangle] 
pub extern "C" fn inObjectLibSystemInit(env: *const libc::c_char, pp: *mut *mut ObjectLibSystem) {
    for (k, v) in vars() {
        if k.eq( unsafe { &CStr::from_ptr(env).to_string_lossy().into_owned() }) {
            let lib = Library::new(v).unwrap();
            let b = Box::new(ObjectLibSystem::new(&lib));
            unsafe {
                *pp = Box::into_raw(b);
            }
            mem::forget(lib); // do not drop library, this is an intended memory leak
            return;
        }
    };
    panic!("Intonaco: env variable GIORNATA not set!");
    return;
}

#[no_mangle] 
pub extern "C" fn inObjectLibSystemStep(ols: *mut ObjectLibSystem) {
    unsafe {
        let olsb = Box::from_raw(ols);
        olsb.step_system();
        std::mem::forget(olsb);
    }
}

#[no_mangle] 
pub extern "C" fn inObjectLibSystemShutdown(ols: *mut ObjectLibSystem) {
    // not implemented yet
}

#[no_mangle] 
pub extern "C" fn inObjectLibSystemAddEntity (ols: *mut ObjectLibSystem, ep: EntityPointer) {
    unsafe {
        let olsb = Box::from_raw(ols);
        olsb.add_entity(ep);
        std::mem::forget(olsb);
    }
}

#[no_mangle] 
pub extern "C" fn inObjectLibSystemRemoveEntity (ols: *mut ObjectLibSystem, ep: EntityPointer) {
    // to be done
}



// Callback System

#[no_mangle] 
pub extern "C" fn inCallbackSystemInit(pp: *mut *mut CallbackSystem) {
    let b = Box::new(CallbackSystem::new());
    unsafe {
        *pp = Box::into_raw(b);
    }
}

#[no_mangle] 
pub extern "C" fn inCallbackSystemRegisterReceiver (cbs: *mut CallbackSystem, ep: EntityPointer, ct: u64, mfp: FrMessageFn2) {
    unsafe {
        let cb = Box::from_raw(cbs);
        cb.register_callback(ep, ct, mfp);
        std::mem::forget(cb);
    }
}

#[no_mangle] 
pub extern "C" fn inCallbackSystemShutdown(cbs: *mut CallbackSystem) {
    // currently not implemented
}

#[no_mangle] 
pub extern "C" fn inCallbackSystemStep(cbs: *mut CallbackSystem) {
    unsafe {
        let cb = Box::from_raw(cbs);
        cb.step_system();
        std::mem::forget(cb);
    }
}


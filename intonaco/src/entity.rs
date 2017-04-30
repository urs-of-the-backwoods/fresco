//
//  Fresco Framework for Multi-Language Programming
//  Copyright 2015-2016 Peter Althainz
//    
//  Distributed under the Apache License, Version 2.0
//  (See attached file LICENSE or copy at 
//  http://www.apache.org/licenses/LICENSE-2.0)
// 
//  file: intonacto/src/entity.rs
//

use component;
use std;
use snowflake::ProcessUniqueId;
use std::collections::{BTreeMap};
use std::sync::Arc;
use libc;
use std::io::{Cursor};
use std::ptr::copy;
use std::mem;
use cbor::{Config, GenericDecoder, DecodeError, value};

pub struct Entity {
    id: ProcessUniqueId,    
    values: BTreeMap <u64, component::Component<Vec<u8>>>,
//    destroyers: Mutex<Vec<RemoveObjectLibItemFromSystemFunctionInfo>>,
}

impl std::cmp::PartialEq for Entity {
    fn eq(&self, other: &Entity) -> bool {
        return self.id == other.id;
    } 
}

#[derive(Copy, Clone)]
pub struct EntityPointer(*mut Entity);

unsafe impl Send for EntityPointer {}
unsafe impl Sync for EntityPointer {}

impl Entity {

    pub fn new(cts: Vec<(u64, Vec<u8>)>) -> Box<Entity> {
        let mut m = BTreeMap::new();
        for (k, v) in cts {
            m.insert(k, component::Component::new(v));
        };
        Box::new(
            Entity {
                id: ProcessUniqueId::new(),
                values: m,
            })
    }

    pub fn get(&self, ct: u64) -> Arc<Vec<u8>> {
        if self.values.contains_key(&ct) {
            return self.values[&ct].get();
        }
        else {
            panic!("Entity get: key not found: {:x}", ct);
        }
    }

    pub fn set(&self, ct: u64, cv: Vec<u8>) {
        if self.values.contains_key(&ct) {
            self.values[&ct].set(cv);
        }
        else {
            panic!("Entity set: key not found: {:x}", ct);
        }
    }

    pub fn values(&self) -> &BTreeMap <u64, component::Component<Vec<u8>>> {
        &self.values
    }

    pub fn id(&self) -> ProcessUniqueId {  
        self.id
    }

    pub fn to_ptr(b: Box<Entity>) -> EntityPointer {
        EntityPointer(Box::into_raw(b))
    }

    pub fn from_ptr(ep: EntityPointer) -> Box<Entity> {
        let EntityPointer(b) = ep;
        let e = unsafe { Box::from_raw(b) };
        e
    }

    pub fn do_with<F, G>(ep: EntityPointer, f: F) -> G where F: FnOnce(&Entity) -> G {
        let e = Entity::from_ptr(ep);
        let r = f(&*e);
        std::mem::forget(e);
        return r;
    }

}


/*
#[no_mangle] 
pub extern "C" fn entity_id(ep: EntityPointer) -> *mut ProcessUniqueId {
    let id = Entity::do_with(ep, (|en| {
            en.id()    
        }));
    let bid = Box::new(id);
    return Box::into_raw(bid);
}

#[no_mangle] 
pub extern "C" fn entity_id_free(bid: *mut ProcessUniqueId) {
    let bid = unsafe {Box::from_raw(bid)};
    // drops out of scope and deletes box
}


#[no_mangle] 
pub extern "C" fn entity_set(data: *const libc::c_char, len: libc::c_int, ep: EntityPointer) {
    if len > 0 {

        let dv = component::vec_from_c_char_p(data, len);
        let mut reader = Cursor::new(dv);
        let (ct, v) = component::read_component(&mut reader).unwrap();

        Entity::do_with(ep, (|en| {
            en.set(ct, v);    
        }));
    } else {
        println!("found entity_set with data len = 0");
    }
}

pub struct DataPointer(Arc<Vec<u8>>);

// reading of data

#[no_mangle] 
pub extern "C" fn entity_get_data(ep: EntityPointer, ct: u64, pp: *mut *mut DataPointer) {

    let av = Entity::do_with(ep, (|en| {
        return en.get(ct);
    }));

    unsafe {
        *pp = Box::into_raw(Box::new(DataPointer(av)));
    }
}

#[no_mangle] 
pub extern "C" fn entity_data_read(dp: *mut DataPointer, p_cp: *mut *const libc::c_char, p_len: *mut libc::c_int) {
    let bav = unsafe {Box::from_raw(dp)};
    unsafe {
        let DataPointer(ref av) = *bav;
        *p_len = av.len() as i32;
        *p_cp = (mem::transmute::<&[u8],&[i8]>(av)).as_ptr();
    }
    std::mem::forget(bav);
}

#[no_mangle] 
pub extern "C" fn entity_data_release(dp: *mut DataPointer) {
    let _ = unsafe {Box::from_raw(dp)};
}

*/

//
// Tests
//

#[cfg(test)]
mod tests {

    use std::mem::{size_of};
    use snowflake::{ProcessUniqueId};

    #[test]
    fn size_of_unique_id() {                            // important test to have correct conversion of UniqueId's to [u8;16]
         assert!(size_of::<ProcessUniqueId>() == 16);
    }
}

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

mod lockfree_value;
mod thread_guard;

extern crate rmp;
extern crate rmp_serialize;
extern crate rustc_serialize;
extern crate libc;
extern crate crossbeam;
extern crate snowflake;
extern crate libloading;

#[macro_use]
extern crate lazy_static;

// use std;
use std::mem;
use std::ffi::CStr;
use std::collections::{HashMap, BTreeMap};
use std::io::{Cursor, Seek, SeekFrom, Read};
use std::io::copy;
use std::result;
use std::error;
use std::process::{exit};


use rustc_serialize::Encodable;
use rustc_serialize::Decodable;
use rmp_serialize::decode;
use rmp_serialize::encode;
use rmp::value::{Integer, Value};

use rmp::decode::value::read_value;

use std::thread;
use std::sync::mpsc::{channel, Sender, Receiver};
use std::sync::{Arc, Mutex, Barrier};
use std::cell::{RefCell, RefMut};
use std::env::vars;

use std::ptr;
     
use crossbeam::mem::epoch::{self, Atomic, Owned};
use crossbeam::sync::MsQueue;
use snowflake::ProcessUniqueId;
use libloading::{Library, Symbol};

/*

    intonaco is an entity component system runtime, which decouples access to the entities from different
    programming languages and different threads. All important data access is lock-free, although multi-threading 
    safe. The runtime is composed from the following pieces.

    Reactive Values and Entities
    ----------------------------
    A reactive value is a lockfree-value, which also calls some callback functions, once a value is changed. Entities 
    are maps of reactive values, where the key is the component identity (an u64).

    Systems
    -------
    A system is defined as a running loop (attached to one and only thread) responding to commands. Those commands
    are send to the thread in a non-blocking messaging queue, the commands are:

    - add an entity to the system, obtaining system specific data, called entity item
    - destroy entity item data from the system, deleting the system specific data
    - set a value on a component within an entity for the system specific data

    There are two types of systems, one handling external object libraries (written in C++) and one handling
    entities itself and responding to external callbacks.


*/


//
// Reactive Values
//

struct ReactiveValue {
    data: lockfree_value::Value<Vec<u8>>,
    setter: lockfree_value::Value<Vec<Arc<Fn(Vec<u8>)>>>,
}

impl ReactiveValue {

    fn new(v: Vec<u8>) -> ReactiveValue {
        ReactiveValue {
            data: lockfree_value::Value::new(v),
            setter: lockfree_value::Value::new(Vec::new()),
        }
    }

    fn set(&self, v: Vec<u8>) {
        let av = self.data.set(v.clone());
        let mref = self.setter.snapshot(); 
        for s in  &*mref {
            s(v.clone());
        }
    }

    fn get(&self) -> Arc<Vec<u8>> {
        self.data.snapshot()
    }

    fn add_setter(&self, f: Arc<Fn(Vec<u8>)>) {         // this is not Sync safe, but currently, we only have one system!
        let mut v = (*self.setter.snapshot()).clone();
        v.push(f);
        self.setter.set(v);
    }

}


//
// Entities
//

#[derive(Clone)]
struct RemoveObjectLibItemFromSystemFunctionInfo {
    item: thread_guard::Value<ObjectLibItemPointer>,
    sender: Arc<MsQueue<SystemCommands>>,
} 

impl RemoveObjectLibItemFromSystemFunctionInfo {
    fn new(s: &Arc<MsQueue<SystemCommands>>, ip: ObjectLibItemPointer) -> Self {
        RemoveObjectLibItemFromSystemFunctionInfo {
            item: thread_guard::Value::new(ip),
            sender: s.clone(),
        }
    }
}

pub struct Entity {
    id: ProcessUniqueId,    
    values: BTreeMap <u64, ReactiveValue>,
    destroyers: Mutex<Vec<RemoveObjectLibItemFromSystemFunctionInfo>>,
}

// represents external unsafe pointer to external entity, 
// since Entity is completely threadsafe, this is most efficient representation, also internally

#[derive(Copy, Clone)]
pub struct EntityPointer(*mut Entity);

unsafe impl Send for EntityPointer {}
unsafe impl Sync for EntityPointer {}

impl Entity {

    pub fn new(cts: Vec<(u64, Vec<u8>)>) -> EntityPointer {
        let mut m: BTreeMap<u64, ReactiveValue> = BTreeMap::new();
        for (k, v) in cts {
            m.insert(k, ReactiveValue::new(v));
        };
        let eb = Box::new(
            Entity {
                id: ProcessUniqueId::new(),
                values: m,
                destroyers: Mutex::new(Vec::new()),
            }
        );
        EntityPointer(Box::into_raw(eb))
    }

    pub fn do_with<F, G>(ep: EntityPointer, f: F) -> G where F: FnOnce(&Entity) -> G {
        let EntityPointer(rp) = ep;
        let e = unsafe { Box::from_raw(rp) };
        let r = f(&*e);
        std::mem::forget(e);
        return r;
    }

    pub fn drop(&mut self) {
        for destroy_info in  &*self.destroyers.lock().unwrap() {
            println!("destroy message sent");
            destroy_info.sender.push(SystemCommands::RemoveObjectLibItemFromSystem(destroy_info.item.clone()));
        }
    }

    pub fn set(&self, ct: u64, cv: Vec<u8>) {
        if self.values.contains_key(&ct) {
            self.values[&ct].set(cv);
        }
        else {
            println!("Entity set: key not found: {:x}", ct);
        }
    }

    pub fn get(&self, ct: u64) -> Arc<Vec<u8>> {
        if self.values.contains_key(&ct) {
            self.values[&ct].get()
        }
        else {
            panic!("Entity get: key not found: {:x}", ct);
        }
    }

}


//
// SystemCommands
//

pub enum SystemCommands {
    AddEntityToSystem(EntityPointer, Arc<Barrier>),
    RemoveObjectLibItemFromSystem(thread_guard::Value<ObjectLibItemPointer>),
    SetValueOnObjectLibItem(thread_guard::Value<SetValueOnObjectLibItemCallback>, thread_guard::Value<ObjectLibItemPointer>, Arc<Vec<u8>>),
    SetValueOnEntityComponent(SetValueCallback, EntityPointer, u64, Arc<Vec<u8>>),
}

//
// system implementation for component libraries from C/C++
//

// represents opaque pointer to external entity
enum ObjectLibItemStruct {}
unsafe impl Send for ObjectLibItemStruct {}
pub type ObjectLibItemPointer = *const ObjectLibItemStruct;

pub type SetValueOnObjectLibItemCallback = extern "stdcall" fn (ObjectLibItemPointer, *const libc::c_char, libc::c_int) -> i32;
pub fn set_value_on_object_lib_item(fp: SetValueOnObjectLibItemCallback, ip: ObjectLibItemPointer, msg: &[u8]) -> i32 {
    unsafe {
        let rval = fp (ip, (mem::transmute::<&[u8],&[i8]>(msg)).as_ptr(), msg.len() as i32);
        rval
    }
}

// the interface to external libraries, exposing object lib interfaces
pub struct ObjectLibInterface<'a> {
    create_function: Symbol <'a, extern "C" fn (item_type: u64, item_init_data: *const libc::c_char, data_len: libc::c_int, pp: *mut ObjectLibItemPointer) -> libc::c_int>,
    destroy_function: Symbol <'a, extern "C" fn (item_type: u64, ip: ObjectLibItemPointer) -> libc::c_int>,
    get_msg_sender_function: Symbol <'a, extern "C" fn (item_type: u64, prop_type: u64, pp: *mut SetValueOnObjectLibItemCallback) -> libc::c_int>,
    register_msg_receiver_function: Symbol <'a, extern "C" fn (item_type: u64, evt_type: u64, ip: ObjectLibItemPointer, ep: EntityPointer, fp: SetValueCallback) -> libc::c_int>,
    error_msg_function:  Symbol <'a, extern "C" fn (error: i32) -> *const libc::c_char>,
}

impl<'a> ObjectLibInterface<'a> {

    pub fn create_item(&self, item_type: u64, item_init_data: &[u8]) -> (i32, ObjectLibItemPointer)
    {
        unsafe {
            let mut p: ObjectLibItemPointer = std::ptr::null();
            let rval = (self.create_function)(item_type, (mem::transmute::<&[u8],&[i8]>(item_init_data)).as_ptr(), item_init_data.len() as i32, (&mut p) as *mut ObjectLibItemPointer);
            (rval as i32, p)
        }
    }

    pub fn destroy_item(&self, item_type: u64, ip: ObjectLibItemPointer) -> i32
    {
        unsafe {
            let rval = (self.destroy_function)(item_type, ip);
            rval as i32
        }
    }

    pub fn get_message_sender(&self, item_type: u64, property_type: u64) -> (i32, SetValueOnObjectLibItemCallback)
    {
        unsafe {
            let mut p: SetValueOnObjectLibItemCallback = std::mem::uninitialized();
            let rval = (self.get_msg_sender_function)(item_type, property_type, &mut p as *mut SetValueOnObjectLibItemCallback);
            (rval as i32, p as SetValueOnObjectLibItemCallback)
        }
    }

    pub fn register_message_receiver(&self, item_type: u64, event_type: u64, ip: ObjectLibItemPointer, ep: EntityPointer, fp: SetValueCallback) -> i32
    {
        unsafe {
            let rval = (self.register_msg_receiver_function)(item_type, event_type, ip, ep, fp);
            rval
        }
    }

    pub fn error_message(&self, error: i32) -> String
    {
        unsafe {
            let rval = (self.error_msg_function)(error);
            let s = CStr::from_ptr(rval).to_string_lossy().into_owned();
            s
        }
    }
}

// creating a object lib interface from a dynamic loaded library (DLL, .so)
fn get_object_lib_interface<'a>(dynamic_lib: &'a Library) -> ObjectLibInterface<'a> {
    // find hgamer3d by dynamic loading

    let create_item: Symbol<extern "C" fn(item_type: u64, item_init_data: *const libc::c_char, data_len: libc::c_int, pp: *mut ObjectLibItemPointer) -> libc::c_int> = unsafe {
        dynamic_lib.get(b"create_item\0").unwrap()
    };  
    let destroy_item: Symbol<extern "C" fn(item_type: u64, ip: ObjectLibItemPointer) -> libc::c_int> = unsafe {
        dynamic_lib.get(b"destroy_item\0").unwrap()
    };  
    let get_msg_sender: Symbol<extern "C" fn(item_type: u64, prop_type: u64, pp: *mut SetValueOnObjectLibItemCallback) -> libc::c_int> = unsafe {
        dynamic_lib.get(b"get_msg_sender\0").unwrap()
    };  
    let register_msg_receiver: Symbol<extern "C" fn(item_type: u64, evt_type: u64, ip: ObjectLibItemPointer, ep: EntityPointer, fp: SetValueCallback) -> libc::c_int> = unsafe {
        dynamic_lib.get(b"register_msg_receiver\0").unwrap()
    };  
    let error_message: Symbol<extern "C" fn(error: i32) -> *const libc::c_char> = unsafe {
        dynamic_lib.get(b"error_message\0").unwrap()
    };  

    ObjectLibInterface {
        create_function: create_item,
        destroy_function: destroy_item,
        get_msg_sender_function: get_msg_sender,
        register_msg_receiver_function: register_msg_receiver,
        error_msg_function: error_message,
    }
}

// a callback from the object lib towards the entities
pub extern "stdcall" fn object_lib_callback(ep: EntityPointer, ct: u64, data: *const libc::c_char, len: libc::c_int) -> i32 {
    Entity::do_with(ep, (|e| {e.set(ct, vec_from_c_char_p(data, len));}));
    return 0;
}

// the system loop for bindings to object libraries
fn object_lib_system_loop(lib_name: String, queue: Arc<MsQueue<SystemCommands>>) {
    let dynamic_lib = Library::new(lib_name).unwrap();
    let lib = get_object_lib_interface(&dynamic_lib);
    loop {
        let cmd = queue.pop();
        match cmd {
            SystemCommands::AddEntityToSystem(ep, b) => {

                let EntityPointer(rp) = ep;
                let e = unsafe { Box::from_raw(rp) };


                for (k, v) in &e.values {

                    let (r, ip) = lib.create_item(*k, &v.get());

                    if r == 0
                    {
                        for (k2, v2) in &e.values {
                            // add sender
                            let (r2, mfp) = lib.get_message_sender(*k, *k2);
                            if r2 == 0 {
                                let queue2 = queue.clone();
                                let tgmfp = thread_guard::Value::new(mfp);
                                let tgip = thread_guard::Value::new(ip.clone());
                                v2.add_setter(
                                   Arc::new(move | av | { queue2.push(SystemCommands::SetValueOnObjectLibItem(tgmfp.clone(), tgip.clone(), Arc::new(av.clone()))); })
                                    );
                                set_value_on_object_lib_item(mfp, ip, &v2.get());                
                            }
                            // register object lib callbacks
                            let r3 = lib.register_message_receiver(*k, *k2, ip, ep, object_lib_callback);
                            //                    println!("low level register: {}", r3);
                        }
                        let mut v = e.destroyers.lock().unwrap();
                        v.push(RemoveObjectLibItemFromSystemFunctionInfo::new(&queue, ip));
                    }
                }

                std::mem::forget(e);
  
                b.wait(); // finalize wait, to free calling thread
            },
            SystemCommands::SetValueOnObjectLibItem(mfp, ip, val) => {
//                println!("{}", "system_loop SetValueCommand");
                set_value_on_object_lib_item(*(mfp.borrow_mut()), *(ip.borrow_mut()), &val);                
            },
            SystemCommands::RemoveObjectLibItemFromSystem(ip) => {
//                println!("{}", "system_loop DestroyCommand");
                lib.destroy_item(0, *(ip.borrow_mut()));          // to do: insert u64 type !!!
            },
            SystemCommands::SetValueOnEntityComponent(cb, ep, ct, av) => {
                
            },
        }
    }
}


// here the object libraries are found, created and the loops are started
lazy_static! {

    pub static ref OBJECT_LIBS: Mutex<Vec<Arc<MsQueue<SystemCommands>>>> = {
        let mut v = Vec::new();
        for (env, libf) in vars()
                        {   
                            if env == "GIORNATA" {
                                let queue = Arc::new(MsQueue::new());
                                {
                                    let queue1 = queue.clone();
                                    // double thread, if childthread is being destroyed, program terminates
                                    thread::spawn(move || { 
	                                    let child = thread::spawn(move || { 
        	                            	object_lib_system_loop(libf, queue1); 
    	                                });             
    	                                child.join();
    	                                exit(-10);
	                                });             
                                }
                                v.push(queue);
                            }
                        };
        Mutex::new(v)
    };
}



//
// system implemenation for callbacks
//

pub struct CallbackSystem {
    queue: Arc<MsQueue<SystemCommands>>,
}

pub type SetValueCallback = extern "stdcall" fn (ep: EntityPointer, ct: u64, *const libc::c_char, libc::c_int) -> i32;
pub fn set_value(fp: SetValueCallback, ep: EntityPointer, ct: u64, msg: &[u8]) -> i32 {
    unsafe {
        let rval = fp (ep, ct, (mem::transmute::<&[u8],&[i8]>(msg)).as_ptr(), msg.len() as i32);
        rval
    }
}

impl CallbackSystem {

    pub fn new() -> CallbackSystem {
        let cbs = CallbackSystem {
            queue: Arc::new(MsQueue::new()),
        };
        cbs
    }

    pub fn register_callback(&self, ep: EntityPointer, ct: u64, mfp: SetValueCallback) {
        Entity::do_with(ep, |e| {
            let queue2 = self.queue.clone();
            e.values[&ct].add_setter(
                    Arc::new(move | v | { queue2.push(SystemCommands::SetValueOnEntityComponent(mfp, ep, ct, Arc::new(v))); })
                );
        });
    }

    pub fn step(&self) {
        let cmd = self.queue.pop();
        match cmd {
            SystemCommands::AddEntityToSystem(e, b) => {
            },
            SystemCommands::SetValueOnObjectLibItem(mfp, ip, val) => {
            },
            SystemCommands::RemoveObjectLibItemFromSystem(ip) => {
            },
            SystemCommands::SetValueOnEntityComponent(cb, ep, ct, av) => {
                set_value(cb, ep, ct, &av);
            },
        }
    }
}


//
// external interface Rust
//

pub fn create_entity (cts: Vec<(u64, Vec<u8>)>) -> EntityPointer {
    let ep = Entity::new(cts);
    let libs = OBJECT_LIBS.lock().unwrap();
    for lib in &(*libs) {
        let b = Arc::new(Barrier::new(2));
        lib.push(SystemCommands::AddEntityToSystem(ep.clone(), b.clone()));
        b.wait();
    };
    ep
}

pub fn create_callback_system() -> Box<CallbackSystem> {
    Box::new(CallbackSystem::new())
}

pub fn register_receiver (cbs: Box<CallbackSystem>, ep: EntityPointer, ct: u64, mfp: SetValueCallback) {
    cbs.register_callback(ep, ct, mfp);
}

pub fn step_callback_system(cbs: Box<CallbackSystem>) {
    cbs.step();
}

//
// external interface C
//

// some helper functions

fn vec_from_c_char_p(data: *const libc::c_char, len: libc::c_int) -> Vec<u8> {
    let mut dv = Vec::with_capacity(len as usize);
    unsafe {
        for i in 0..len {
            dv.push(*data.offset(i as isize) as u8);
        }
    }
    dv
}

fn read_component(cur: &mut Cursor<Vec<u8>>) -> Option<(u64, Vec<u8>)> {
    let val = read_value(cur);
    match val {
        Ok(v) => match v {
                        rmp::value::Value::Integer(i) => {
                            let u = match i {
                                Integer::U64(u2) => u2,
                                Integer::I64(i2) => i2 as u64
                            };
                            let pos = cur.position();
                            let val2 = read_value(cur);
                            let pos2 = cur.position();
                            cur.seek(SeekFrom::Start(pos));
                            let mut v = Vec::with_capacity((pos2-pos) as usize);
//                            unsafe {v.set_len((pos2-pos) as usize);}
                            copy(&mut cur.take(pos2-pos), &mut v);
                            Some((u, v))
                        },
                        _ => panic!("read_component: no value read"),
                    },
        Err(e) => None, 
    }   
}

#[no_mangle] 
pub extern "C" fn entity_create(data: *const libc::c_char, len: libc::c_int, pp: *mut EntityPointer)
{
    let mut dv = vec_from_c_char_p(data, len);
    let mut reader = Cursor::new(dv);
    let mut cts = Vec::new();

    loop {
        let val = read_component(&mut &mut reader);
        match val {
            Some((u, v)) => cts.push((u, v)),
            None => { break; }
        }
    }

    let ep = create_entity(cts);
//    println!("created entity: {:p} - {:?}", e, e.id);
    unsafe {
//      let ep = mem::transmute::< &Arc<Entity>, &EntityPointer>(&e);
      *pp = ep;
    }
}

#[no_mangle] 
pub extern "C" fn entity_set(data: *const libc::c_char, len: libc::c_int, ep: EntityPointer) {
    let dv = vec_from_c_char_p(data, len);
    let mut reader = Cursor::new(dv);
    let (ct, v) = read_component(&mut reader).unwrap();

    Entity::do_with(ep, (|en| {
        en.set(ct, v);    
    }));

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
        *p_cp = unsafe { (mem::transmute::<&[u8],&[i8]>(av)).as_ptr() };
    }
    std::mem::forget(bav);
}

#[no_mangle] 
pub extern "C" fn entity_data_release(dp: *mut DataPointer) {
    let bav = unsafe {Box::from_raw(dp)};
}


#[no_mangle] 
pub extern "C" fn callback_system_create(pp: *mut *mut CallbackSystem) {
    unsafe {
        *pp = Box::into_raw(create_callback_system());
    }
}

// also need callback_system_destroy !

#[no_mangle] 
pub extern "C" fn callback_system_register_receiver (cbs: *mut CallbackSystem, ep: EntityPointer, ct: u64, mfp: SetValueCallback) {
    unsafe {
        let cbs = Box::from_raw(cbs);
        cbs.register_callback(ep, ct, mfp);
        std::mem::forget(cbs);
    }
}

#[no_mangle] 
pub extern "C" fn callback_system_step(cbs: *mut CallbackSystem) {
    unsafe {
        let cbs = Box::from_raw(cbs);
        cbs.step();
        std::mem::forget(cbs);
    }
}





//
// TESTING
//


#[test]
fn test0() {
    let buf = [0,0,0,0,0];
    let (rval, _ip) = get_object_lib_interface().create_item(0, &buf);
    let rs = get_object_lib_interface().error_message(rval);
    println!("return value from create: {} -- {}\n", rval, rs);
}

#[test]
fn test1() {
    let val = (42u8, "the Answer");

    // The encoder borrows the bytearray buffer.
    let mut buf = [0u8; 13];

    val.encode(&mut encode::Encoder::new(&mut &mut buf[..]))
    	.ok()
    	.unwrap();

    assert_eq!([0x92, 0x2a, 0xaa, 0x74, 0x68, 0x65, 0x20, 0x41, 0x6e, 0x73, 0x77, 0x65, 0x72], buf);
}

#[test]
fn test2() {
    let buf = [0x92, 0x2a, 0xaa, 0x74, 0x68, 0x65, 0x20, 0x41, 0x6e, 0x73, 0x77, 0x65, 0x72];

    let mut decoder = decode::Decoder::new(&buf[..]);

    let res: (u8, String) = Decodable::decode(&mut decoder).unwrap();

    assert_eq!((42u8, "the Answer".to_string()), res);
}

#[derive(RustcEncodable, RustcDecodable, PartialEq, Debug)]
struct Custom {
    id: u32,
    key: String,
}

#[test]
fn test3() {
    let val = Custom { id: 42u32, key: "the Answer".to_string() };

    let mut buf = [0u8; 13];

    val.encode(&mut encode::Encoder::new(&mut &mut buf[..]))
    	.ok()
    	.unwrap();

    assert_eq!([0x92, 0x2a, 0xaa, 0x74, 0x68, 0x65, 0x20, 0x41, 0x6e, 0x73, 0x77, 0x65, 0x72], buf);

    // Now try to unpack the buffer into the initial struct.
    let mut decoder = decode::Decoder::new(&buf[..]);
    let res: Custom = Decodable::decode(&mut decoder).ok().unwrap();

    assert_eq!(val, res);
}
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

//
// global static entities, ENTITIES, to keep them living, LIBS, loaded system runtimes
//

lazy_static! {
//    pub static ref ENTITIES: Mutex<HashMap<ProcessUniqueId, Arc<Entity>>> = Mutex::new(HashMap::new());

    pub static ref LIBS: Mutex<Vec<Arc<MsQueue<SystemCommands>>>> = {
        let mut v = Vec::new();
        for (env, libf) in vars()
                        {   
                            if env == "INTONACO" {
                                let queue = Arc::new(MsQueue::new());
                                {
                                    let queue1 = queue.clone();
                                    thread::spawn(move || { system_loop(libf, queue1); });             
                                }
                                v.push(queue);
                            }
                        };
        Mutex::new(v)
    };
}

//
// system implementation for component libraries from C/C++
//

pub enum SystemCommands {
    AddLibCommand(EntityPointer, Arc<Barrier>),
    SetValueCmd(thread_guard::Value<MessageFunctionPointer>, thread_guard::Value<ItemPointer>, Arc<Vec<u8>>),
    DestroyCmd(thread_guard::Value<ItemPointer>),
}

fn system_loop(lib_name: String, queue: Arc<MsQueue<SystemCommands>>) {
    let dynamic_lib = Library::new(lib_name).unwrap();
    let lib = get_object_lib_interface(&dynamic_lib);
    loop {
        let cmd = queue.pop();
        match cmd {
            SystemCommands::AddLibCommand(ep, b) => {
//                println!("{}", "system_loop AddLibCommand");
                Entity::add_lib(ep, &lib, &queue);
                b.wait(); // finalize wait, to free calling thread
            },
            SystemCommands::SetValueCmd(mfp, ip, val) => {
//                println!("{}", "system_loop SetValueCommand");
                send_message(*(mfp.borrow_mut()), *(ip.borrow_mut()), &val);                
            },
            SystemCommands::DestroyCmd(ip) => {
//                println!("{}", "system_loop DestroyCommand");
                lib.destroy_item(0, *(ip.borrow_mut()));          // here we need to insert u64 type !!!
            },
        }
    }
}

//
// system implemenation for callbacks
//

pub struct CallbackSystem {
    queue: Arc<MsQueue<SystemCommands>>,
}

impl CallbackSystem {

    pub fn new() -> CallbackSystem {
        let cbs = CallbackSystem {
            queue: Arc::new(MsQueue::new()),
        };
        cbs
    }

    pub fn register_cbs_callback(&self, ep: EntityPointer, ct: u64, mfp: MessageFunctionPointer) {
        Entity::do_with(ep, |e| {
            e.values[&ct].add_setter(SetValueCmdFunctionInfo::new(&self.queue, mfp, unsafe {std::mem::transmute(ep)}));
        });
    }

    pub fn step(&self) {
        let cmd = self.queue.pop();
        match cmd {
            SystemCommands::AddLibCommand(e, b) => {
            },
            SystemCommands::SetValueCmd(mfp, ip, val) => {
                send_message(*(mfp.borrow_mut()), *(ip.borrow_mut()), &val);                
            },
            SystemCommands::DestroyCmd(ip) => {
            },
        }
    }
}

// http://stackoverflow.com/questions/32270030/passing-rust-closures-to-c/32270215#32270215

pub extern "stdcall" fn cbs_callback(ip: ItemPointer, dp: *mut (EntityPointer, u64), data: *const libc::c_char, len: libc::c_int) -> i32 {

    let b = unsafe { Box::from_raw(dp) };
    let (ep, ct) = *b;

    Entity::do_with(ep, (|e| {e.set(ct, vec_from_c_char_p(data, len));}));
    std::mem::forget(b);
    return 0;
}

//
// Data Structures and Entity
//

// represents opaque pointer to external entity
enum ItemStruct {}
unsafe impl Send for ItemStruct {}
pub type ItemPointer = *const ItemStruct;

// messag function pointer
pub type MessageFunctionPointer = extern "stdcall" fn (ItemPointer, *const libc::c_char, libc::c_int) -> i32;
pub fn send_message(fp: MessageFunctionPointer, ip: ItemPointer, msg: &[u8]) -> i32 {
    unsafe {
        let rval = fp (ip, (mem::transmute::<&[u8],&[i8]>(msg)).as_ptr(), msg.len() as i32);
        rval
    }
}

pub type MessageFunctionPointer2 = extern "stdcall" fn (ItemPointer, *mut (EntityPointer, u64), *const libc::c_char, libc::c_int) -> i32;

#[derive(Clone)]
struct SetValueCmdFunctionInfo {
    mfp: thread_guard::Value<MessageFunctionPointer>,
    item: thread_guard::Value<ItemPointer>,
    sender: Arc<MsQueue<SystemCommands>>,
} 

impl SetValueCmdFunctionInfo {
    fn new(s: &Arc<MsQueue<SystemCommands>>, mfp: MessageFunctionPointer, ip: ItemPointer) -> Self {
        SetValueCmdFunctionInfo {
            mfp: thread_guard::Value::new(mfp),
            item: thread_guard::Value::new(ip),
            sender: s.clone(),
        }
    }
}

#[derive(Clone)]
struct DestroyCmdFunctionInfo {
    item: thread_guard::Value<ItemPointer>,
    sender: Arc<MsQueue<SystemCommands>>,
} 

impl DestroyCmdFunctionInfo {
    fn new(s: &Arc<MsQueue<SystemCommands>>, ip: ItemPointer) -> Self {
        DestroyCmdFunctionInfo {
            item: thread_guard::Value::new(ip),
            sender: s.clone(),
        }
    }
}

struct ReactiveValue {
    data: lockfree_value::Value<Vec<u8>>,
    setter: lockfree_value::Value<Vec<SetValueCmdFunctionInfo>>,
}

impl ReactiveValue {

    fn new(v: Vec<u8>) -> ReactiveValue {
        ReactiveValue {
            data: lockfree_value::Value::new(v),
            setter: lockfree_value::Value::new(Vec::new()),
        }
    }

    fn set(&self, v: Vec<u8>) {
        let av = self.data.set(v);
        let mref = self.setter.snapshot(); 
        for setter_info in  &*mref {
            setter_info.sender.push(SystemCommands::SetValueCmd(setter_info.mfp.clone(), setter_info.item.clone(), av.clone()));
        }
    }

    fn get(&self) -> Arc<Vec<u8>> {
        self.data.snapshot()
    }

    fn add_setter(&self, f: SetValueCmdFunctionInfo) {         // this is not Sync safe, but currently, we only have one system!
        let mut v = (*self.setter.snapshot()).clone();
        v.push(f);
        self.setter.set(v);
    }

}


pub struct Entity {
    id: ProcessUniqueId,    
    values: BTreeMap <u64, ReactiveValue>,
    destroyers: Mutex<Vec<DestroyCmdFunctionInfo>>,
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

    pub fn do_with<F>(ep: EntityPointer, f: F) where F: FnOnce(&Entity) {
        let EntityPointer(rp) = ep;
        let e = unsafe { Box::from_raw(rp) };
        f(&*e);
        std::mem::forget(e);
    }

    pub fn drop(&mut self) {
        for destroy_info in  &*self.destroyers.lock().unwrap() {
            println!("destroy message sent");
            destroy_info.sender.push(SystemCommands::DestroyCmd(destroy_info.item.clone()));
        }
    }

    pub fn add_lib(ep: EntityPointer, lib: &ObjectLibInterface, queue: &Arc<MsQueue<SystemCommands>>) {
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
                        v2.add_setter(SetValueCmdFunctionInfo::new(queue, mfp, ip));
                        send_message(mfp, ip, &v2.get());                
                    }
                    unsafe {
                        let dp = Box::into_raw(Box::new( (ep, *k2) ));
                        let r3 = lib.register_message_receiver(*k, *k2, ip, dp, cbs_callback);
                        if r3 != 0 {
                          let bx: Box<(EntityPointer, u64)> = Box::from_raw(dp);
                        }
                    }
//                    println!("low level register: {}", r3);
                }
                let mut v = e.destroyers.lock().unwrap();
                v.push(DestroyCmdFunctionInfo::new(queue, ip));
            }
        }

        std::mem::forget(e);
    }

    pub fn set(&self, ct: u64, cv: Vec<u8>) {
        if self.values.contains_key(&ct) {
            self.values[&ct].set(cv);
        }
        else {
            println!("key not found: {:x}", ct);
        }
    }

    pub fn get(&self, ct: u64) -> Arc<Vec<u8>> {
        self.values[&ct].get()
    }

}

//
// Interface to Component Libraries
//

pub struct ObjectLibInterface<'a> {
    create_function: Symbol <'a, extern "C" fn (item_type: u64, item_init_data: *const libc::c_char, data_len: libc::c_int, pp: *mut ItemPointer) -> libc::c_int>,
    destroy_function: Symbol <'a, extern "C" fn (item_type: u64, ip: ItemPointer) -> libc::c_int>,
    get_msg_sender_function: Symbol <'a, extern "C" fn (item_type: u64, prop_type: u64, pp: *mut MessageFunctionPointer) -> libc::c_int>,
    register_msg_receiver_function: Symbol <'a, extern "C" fn (item_type: u64, evt_type: u64, ip: ItemPointer, dp: *mut (EntityPointer, u64), fp: MessageFunctionPointer2) -> libc::c_int>,
    error_msg_function:  Symbol <'a, extern "C" fn (error: i32) -> *const libc::c_char>,
}

impl<'a> ObjectLibInterface<'a> {

    pub fn create_item(&self, item_type: u64, item_init_data: &[u8]) -> (i32, ItemPointer)
    {
        unsafe {
            let mut p: ItemPointer = std::ptr::null();
            let rval = (self.create_function)(item_type, (mem::transmute::<&[u8],&[i8]>(item_init_data)).as_ptr(), item_init_data.len() as i32, (&mut p) as *mut ItemPointer);
            (rval as i32, p)
        }
    }

    pub fn destroy_item(&self, item_type: u64, ip: ItemPointer) -> i32
    {
        unsafe {
            let rval = (self.destroy_function)(item_type, ip);
            rval as i32
        }
    }

    pub fn get_message_sender(&self, item_type: u64, property_type: u64) -> (i32, MessageFunctionPointer)
    {
        unsafe {
            let mut p: MessageFunctionPointer = std::mem::uninitialized();
            let rval = (self.get_msg_sender_function)(item_type, property_type, &mut p as *mut MessageFunctionPointer);
            (rval as i32, p as MessageFunctionPointer)
        }
    }

    pub fn register_message_receiver(&self, item_type: u64, event_type: u64, ip: ItemPointer, dp: *mut (EntityPointer, u64), fp: MessageFunctionPointer2) -> i32
    {
        unsafe {
            let rval = (self.register_msg_receiver_function)(item_type, event_type, ip, dp, fp);
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


fn get_object_lib_interface<'a>(dynamic_lib: &'a Library) -> ObjectLibInterface<'a> {
    // find hgamer3d by dynamic loading

    let create_item: Symbol<extern "C" fn(item_type: u64, item_init_data: *const libc::c_char, data_len: libc::c_int, pp: *mut ItemPointer) -> libc::c_int> = unsafe {
        dynamic_lib.get(b"create_item\0").unwrap()
    };  
    let destroy_item: Symbol<extern "C" fn(item_type: u64, ip: ItemPointer) -> libc::c_int> = unsafe {
        dynamic_lib.get(b"destroy_item\0").unwrap()
    };  
    let get_msg_sender: Symbol<extern "C" fn(item_type: u64, prop_type: u64, pp: *mut MessageFunctionPointer) -> libc::c_int> = unsafe {
        dynamic_lib.get(b"get_msg_sender\0").unwrap()
    };  
    let register_msg_receiver: Symbol<extern "C" fn(item_type: u64, evt_type: u64, ip: ItemPointer, dp: *mut (EntityPointer, u64), fp: MessageFunctionPointer2) -> libc::c_int> = unsafe {
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


//
// external interface Rust
//

pub fn create_entity (cts: Vec<(u64, Vec<u8>)>) -> EntityPointer {
    let ep = Entity::new(cts);
    let libs = LIBS.lock().unwrap();
    for lib in &(*libs) {
        let b = Arc::new(Barrier::new(2));
        lib.push(SystemCommands::AddLibCommand(ep.clone(), b.clone()));
        b.wait();
    };
    ep
}

pub fn create_callback_system() -> Box<CallbackSystem> {
    Box::new(CallbackSystem::new())
}

pub fn register_receiver (cbs: Box<CallbackSystem>, ep: EntityPointer, ct: u64, mfp: MessageFunctionPointer) {
    cbs.register_cbs_callback(ep, ct, mfp);
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

#[no_mangle] 
pub extern "C" fn callback_system_create(pp: *mut *mut CallbackSystem) {
    unsafe {
        *pp = Box::into_raw(create_callback_system());
    }
}

// also need callback_system_destroy !

#[no_mangle] 
pub extern "C" fn callback_system_register_receiver (cbs: *mut CallbackSystem, ep: EntityPointer, ct: u64, mfp: MessageFunctionPointer) {
    unsafe {
        let cbs = Box::from_raw(cbs);
        cbs.register_cbs_callback(ep, ct, mfp);
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
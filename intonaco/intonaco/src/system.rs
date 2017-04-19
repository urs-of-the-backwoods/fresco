//
//  Fresco Framework for Multi-Language Programming
//  Copyright 2015-2016 Peter Althainz
//    
//  Distributed under the Apache License, Version 2.0
//  (See attached file LICENSE or copy at 
//  http://www.apache.org/licenses/LICENSE-2.0)
// 
//  file: intonacto/src/system.rs
//

use std;
use libc;
use thread_guard;
use component;
use cbor::{Config, GenericDecoder, DecodeError, Encoder, value};
use rustc_serialize::{Encodable};

use std::mem;
use std::ffi::CString;
use std::io::{Cursor, Write};
use std::process::{exit};
use std::time::Duration;

use std::thread;
use std::sync::{Arc, Mutex, Barrier};

use crossbeam::sync::MsQueue;
use snowflake::ProcessUniqueId;

#[cfg(unix)]
use libloading::os::unix::{Library, Symbol};
#[cfg(windows)]
use libloading::os::windows::{Library, Symbol};

use entity::{Entity, EntityPointer};
use ftypes::{FrMsg, FrMsgLength, FrComponentType, FrItemType, FrPropertyType, FrItem, FrMessageFn, FrMessageFn2};


const CT_ENTITY_ID: u64 = 0x112cc0dc2647d39e;

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
// System trait
//

pub trait System {
    fn add_entity(&self, ep: EntityPointer);
    fn step_system(&self);
}

//
// ObjectLibSystem, System with Object realted libraries as implementation behind it
//

pub enum ObjectLibSystemCommands {
    AddEntityToOLS(EntityPointer, Arc<Barrier>),
    RemoveObjectLibItemFromOLS(u64, thread_guard::Value<FrItem>),
    SetValueOnObjectLibItem(thread_guard::Value<SetValueOnObjectLibItemCallback>, thread_guard::Value<FrItem>, Arc<Vec<u8>>),
}

pub struct ObjectLibSystem  {
    queue: Arc<MsQueue<ObjectLibSystemCommands>>,
    lib_if: ObjectLibInterface ,
}

impl  ObjectLibSystem  {

    pub fn new(lib: & Library) -> ObjectLibSystem {
        let li = get_object_lib_interface(lib);
        let q = Arc::new(MsQueue::new());
        ObjectLibSystem {
            queue: q, 
            lib_if: li
        }
    }

}

impl  System for ObjectLibSystem  {

    fn add_entity(&self, ep: EntityPointer) {
        let b = Arc::new(Barrier::new(2));
        self.queue.push(ObjectLibSystemCommands::AddEntityToOLS(ep.clone(), b.clone()));
        b.wait();
    }

    fn step_system(&self) {
        let cmd = self.queue.pop();
        match cmd {
            ObjectLibSystemCommands::AddEntityToOLS(ep, b) => {

                Entity::do_with(ep, |e| { 

                    for (k, v) in e.values() {

                        let ip = self.lib_if.create_item(*k, &v.get());
                        let tgip = thread_guard::Value::new(ip.clone());

                        // set entity id on item, if interested in it
                        match self.lib_if.get_message_sender(*k, CT_ENTITY_ID) 
                        {
                            Some(mfp_id) => {
                                // encode to cbor as data, bytestring
                                let id_u8 = unsafe { mem::transmute::<ProcessUniqueId, [u8; 16]>(e.id()) };
                                let mut buf = vec![];      
                                let mut enc = Encoder::new(buf);
                                enc.bytes(&id_u8);
                                set_c_value_on_object_lib_item(mfp_id, ip, &enc.into_writer());                
                            },
                            None => {}
                        }
                        // set attributes
                        for (k2, v2) in e.values() {
                            // add sender
                            match self.lib_if.get_message_sender(*k, *k2)
                            {
                                Some(mfp) => {
                                    let queue2 = self.queue.clone();
                                    let tgmfp2 = thread_guard::Value::new(mfp);
                                    let tgip2 = thread_guard::Value::new(ip.clone());
                                    v2.add_callback(
                                       Arc::new(move | av | { queue2.push(ObjectLibSystemCommands::SetValueOnObjectLibItem(tgmfp2.clone(), tgip2.clone(), av)); })
                                        );
                                    set_c_value_on_object_lib_item(mfp, ip, &v2.get());                
                                },
                                None => {}
                            }
                            // register object lib callbacks
                            self.lib_if.register_message_receiver(*k, *k2, ip, ep, object_lib_callback);
                        }
                        // drop logic
                        let queue1 = self.queue.clone();
                        let c = *k;
                        v.add_dropper(
                           Arc::new(move || { 
//                                let b2 = Arc::new(Barrier::new(2));
                            queue1.push(ObjectLibSystemCommands::RemoveObjectLibItemFromOLS(c, tgip.clone()));
//                                b2.wait();
                            })
                        );
                    }
                });

                b.wait(); // finalize wait, to free calling thread
            },
            ObjectLibSystemCommands::SetValueOnObjectLibItem(mfp, ip, val) => {
                set_c_value_on_object_lib_item(*(mfp.borrow_mut()), *(ip.borrow_mut()), &val);              
            },
            ObjectLibSystemCommands::RemoveObjectLibItemFromOLS(ct, ip) => {
                self.lib_if.destroy_item(ct, *(ip.borrow_mut()));          // to do: insert u64 type !!!
                // b.wait();
            },
        }

    }

}


//
// system implementation for component libraries from C/C++
//

pub type SetValueOnObjectLibItemCallback = FrMessageFn; 
pub fn set_c_value_on_object_lib_item(fp: SetValueOnObjectLibItemCallback, ip: FrItem, msg: &[u8]) {
    unsafe {
        fp (ip, msg.as_ptr(), msg.len() as FrMsgLength);
    }
}

// the interface to external libraries, exposing object lib interfaces
pub struct ObjectLibInterface {
    create_function: Symbol <extern "C" fn(FrItemType, FrMsg, FrMsgLength) -> FrItem>,
    destroy_function: Symbol <extern "C" fn(FrItemType, FrItem)>,
    get_msg_sender_function: Symbol <extern "C" fn(FrItemType, FrPropertyType) -> Option<FrMessageFn>>,
    register_msg_receiver_function: Symbol <extern "C" fn(FrItemType, FrPropertyType, FrItem, EntityPointer, FrMessageFn2)>,
}

impl ObjectLibInterface {

    pub fn create_item(&self, item_type: FrItemType, item_init_data: &[u8]) -> FrItem
    {
        unsafe {
            (self.create_function)(item_type, item_init_data.as_ptr(), item_init_data.len() as FrMsgLength)
        }
    }

    pub fn destroy_item(&self, item_type: FrItemType, ip: FrItem)
    {
        (self.destroy_function)(item_type, ip);
    }

    pub fn get_message_sender(&self, item_type: FrItemType, property_type: FrPropertyType) -> Option<FrMessageFn>
    {
        unsafe {
            (self.get_msg_sender_function)(item_type, property_type)
        }
    }

    pub fn register_message_receiver(&self, item_type: FrItemType, event_type: FrPropertyType, ip: FrItem, ep: EntityPointer, fp: FrMessageFn2)
    {
        (self.register_msg_receiver_function)(item_type, event_type, ip, ep, fp);
    }

}


// creating a object lib interface from a dynamic loaded library (DLL, .so)
fn get_object_lib_interface(dynamic_lib: & Library) -> ObjectLibInterface {
    // find hgamer3d by dynamic loading

    let create_item: Symbol<extern "C" fn(FrItemType, FrMsg, FrMsgLength) -> FrItem> = unsafe {
        dynamic_lib.get(b"gioCreateItem\0").unwrap()
    };  
    let destroy_item: Symbol<extern "C" fn(FrItemType, FrItem)> = unsafe {
        dynamic_lib.get(b"gioDestroyItem\0").unwrap()
    };  
    let get_msg_sender: Symbol<extern "C" fn(FrItemType, FrPropertyType) -> Option<FrMessageFn>> = unsafe {
        dynamic_lib.get(b"gioGetMsgSender\0").unwrap()
    };  
    let register_msg_receiver: Symbol<extern "C" fn(FrItemType, FrPropertyType, FrItem, EntityPointer, FrMessageFn2)> = unsafe {
        dynamic_lib.get(b"gioRegisterMsgReceiver\0").unwrap()
    };  

    ObjectLibInterface {
        create_function: create_item,
        destroy_function: destroy_item,
        get_msg_sender_function: get_msg_sender,
        register_msg_receiver_function: register_msg_receiver,
    }
}

// a callback from the object lib towards the entities
pub extern "C" fn object_lib_callback(ep: EntityPointer, ct: FrPropertyType, data: FrMsg, len: FrMsgLength) {
    Entity::do_with(ep, (|e| {e.set(ct, component::vec_from_c_char_p(data, len));}));
}





//
// CallbackSystem, System needed for callbacks, establish a routine, if value changes
//

// Rust implementation

pub enum CallbackSystemCommands {
    SetCValue(EntityPointer, u64, FrMessageFn2, Arc<Vec<u8>>),
}

pub struct CallbackSystem {
    queue: Arc<MsQueue<CallbackSystemCommands>>,
}

pub fn set_c_value(fp: FrMessageFn2, ep: EntityPointer, ct: u64, msg: &[u8]) {
    unsafe {
        fp (ep, ct, msg.as_ptr(), msg.len() as u32);
    }
}

impl CallbackSystem {
    pub fn new() -> CallbackSystem {
        let cbs = CallbackSystem {
            queue: Arc::new(MsQueue::new()),
        };
        cbs
    }

    pub fn register_callback(&self, ep: EntityPointer, ct: u64, cb: FrMessageFn2) {
        Entity::do_with(ep, |e| {
            let queue2 = self.queue.clone();
            e.values()[&ct].add_callback(
                    Arc::new(move | v | { queue2.push(CallbackSystemCommands::SetCValue(ep, ct, cb, v)); })
                );
        });
    }
}

impl  System for CallbackSystem  {

    fn add_entity(&self, ep: EntityPointer) {
    }

    fn step_system(&self) {
        let cmdo = self.queue.try_pop();
        match cmdo {
            None =>  thread::sleep(Duration::from_millis(20)),
            Some(cmd) => match cmd {
                CallbackSystemCommands::SetCValue(ep, ct, cb, v) => {
                    set_c_value(cb, ep, ct, &v);
                },
            },
        }
    }
}





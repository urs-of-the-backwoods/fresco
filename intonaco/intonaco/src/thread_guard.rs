//
//  Fresco Framework for Multi-Language Programming
//  Copyright 2015-2016 Peter Althainz
//    
//  Distributed under the Apache License, Version 2.0
//  (See attached file LICENSE or copy at 
//  http://www.apache.org/licenses/LICENSE-2.0)
// 
//  file: intonacto/src/thread_guard.rs
//

//
// thread_guard creates the possibility to hand out pointers to something to a different
// thread, which can be send to the original thread for usage only inside the original thread.
// This mechanism is guaranteed by a guard value, which allows to use the original pointer.
// The motiviation behind this structure is to allow foreign threads to store information
// about id's (entities) and send them with commands to the originator thread.
//

use std::cell::{RefCell, RefMut};
use std::sync::Arc;
use snowflake::ProcessUniqueId;
use std::marker::PhantomData;

// thread local storage, to support thread bound objects
thread_local!(static GUARD: Guard = Guard::new());

pub struct Value<T> {
    id: ProcessUniqueId,
    value: Arc<RefCell<T>>,
}

impl<T> Clone for Value<T> {
    fn clone(&self) -> Self {
        Value {
            id: self.id,
            value: self.value.clone(),
        }
    }
}
unsafe impl<T> Send for Value<T> {}
unsafe impl<T> Sync for Value<T> {}

impl<T> Value<T> {

    pub fn borrow_mut(&self) -> RefMut<T> {
        // panic, if guard id is not the same
        GUARD.with(|g| { assert_eq!(g.id, self.id) });
//            if self.id != guard.id {panic!("ecs-glue::thread_guard: id not the same, usage by foreign thread/guard no allowed!");};
        self.value.borrow_mut()
    }

    pub fn new(v: T) -> Value<T> {
//            let mut new_id = None;
        let new_id = GUARD.with(|g| { g.id });
        Value {
            id: new_id,  // take id of this thread (thread local storage)
            value: Arc::new(RefCell::new(v)),
        }
    }
}

pub struct Guard {
    id: ProcessUniqueId,
    m: PhantomData<*mut ()>,  // make Guard not Send, therefore also not Sync
}

impl Guard {
    pub fn new() -> Self {
        Guard {
            id: ProcessUniqueId::new(),
            m: PhantomData,
        }
    }
}

//
//  Fresco Framework for Multi-Language Programming
//  Copyright 2015-2016 Peter Althainz
//    
//  Distributed under the Apache License, Version 2.0
//  (See attached file LICENSE or copy at 
//  http://www.apache.org/licenses/LICENSE-2.0)
// 
//  file: intonacto/src/lockfree_value.rs
//


// lockfree_value allows to access a value from multiple threads simultaneously without lock.
// The snapshot function creates a snapshot of the current value, which might be not up to date, since
// synchrounous set operations may take place. Snapshot returns an Arc reference of the value.
// A lockfree_value is Send and Sync.
//

use std::sync::Arc;
use crossbeam::mem::epoch::{self, Atomic, Owned};
use std::sync::atomic::Ordering::{Acquire, Release, Relaxed};

pub struct Value<T> {
    data_ptr: Atomic<Arc<T>>,
}

impl<T> Value<T> {

    pub fn new(ival: T) -> Value<T> {
        let v = Value {
            data_ptr: Atomic::null()
        };
        let o = Owned::new(Arc::new(ival));
        v.data_ptr.store(Some(o), Relaxed);
        v
    }

    pub fn set(&self, ival: T) -> Arc<T> {
        // allocate new value
        let a = Arc::new(ival);
        let mut o = Owned::new(a.clone());
        // become active
        let guard = epoch::pin();

        // replace
        loop {
            // snapshot current value
            match self.data_ptr.load(Acquire, &guard) {
                Some (ptr) => {
                    // if snapshot is still good replace
                    match self.data_ptr.cas(Some(ptr), Some(o), Release)  {
                        Ok(_) => {
                            unsafe {
                                guard.unlinked(ptr);
                            };
                            return a;
                        },
                        Err(Some(owned)) => {
                            o = owned;
                        },
                        // only to satisfy ownership semantics, should never happen
                        Err(None) => return panic!("lockfree_value: Err(None) in Value::set!"),
                    }

                },
                None => return panic!("lockfree_value: empty pointer in Value::set!"),
            }
        }
    }

    // returns a snapshot of current value
    pub fn snapshot(&self) -> Arc<T> {
        // become active
        let guard = epoch::pin();

        // snapshot current value
        match self.data_ptr.load(Acquire, &guard) {
                Some (ptr) => (*ptr).clone(),
                None => panic!("lockfree_value: empty pointer in Value::snapshot!"),
            }
    }
}

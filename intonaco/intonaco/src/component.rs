//
//  Fresco Framework for Multi-Language Programming
//  Copyright 2015-2016 Peter Althainz
//    
//  Distributed under the Apache License, Version 2.0
//  (See attached file LICENSE or copy at 
//  http://www.apache.org/licenses/LICENSE-2.0)
// 
//  file: intonacto/src/component.rs
//

// The component inherits its name from the role it plays in the entity-component-system model, which is 
// the base of underlying ideas. In addition to isolate threading behaviour, our components have some
// useful properties, like being accessible from multiple threads without locking the data structure.
// This is achieved by leveraging crossbeam functionality, specifically ArcCell. In addition component
// is a "reactive" value, with the ability to register callbacks on value change.

use libc;
use std::io::{Cursor, Seek, SeekFrom, Read};
use std::sync::Arc;
use crossbeam::sync::ArcCell;
use std::io::copy;

//
// Reactive Component
//

pub struct Component<T> {
    data: ArcCell<T>,
    setter: ArcCell<Vec<Arc<Fn(Arc<T>)>>>,
    dropper: ArcCell<Vec<Arc<Fn()>>>,
}

impl<T> Component<T> {

    pub fn new(v: T) -> Component<T> {
        Component {
            data: ArcCell::new(Arc::new(v)),
            setter: ArcCell::new(Arc::new(Vec::new())),
            dropper: ArcCell::new(Arc::new(Vec::new())),
        }
    }

    pub fn set(&self, v: T) {
        let av = Arc::new(v);
        self.data.set(av.clone());
        let mref = self.setter.get(); 
        for s in  &*mref {
            s(av.clone());
        }
    }

    pub fn get(&self) -> Arc<T> {
        self.data.get()
    }

    // remark those functions are not logically threadsafe, although technically. Reading values during adding, setting setters is ok.
    // Having two threads simultaneously adding setters is not working, one thread might overwrite changes of other thread.
    // Therefore usage in system code should be guarantee that systems add entities sequentially. (Or we need an add/delete mutex).

    pub fn add_callback(&self, f: Arc<Fn(Arc<T>)>) {         
        let mut v = (*self.setter.get()).clone();
        v.push(f);
        self.setter.set(Arc::new(v));
    }

    pub fn add_dropper(&self, f: Arc<Fn()>) {         
        let mut v = (*self.dropper.get()).clone();
        v.push(f);
        self.dropper.set(Arc::new(v));
    }

}

impl<T> Drop for Component<T> {
    fn drop(&mut self) {
        let mref = self.dropper.get(); 
        for d in  &*mref {
            d();
        }
    }
}

// helper functions to translate data from C

pub fn vec_from_c_char_p(data: *const u8, len: u32) -> Vec<u8> {
    let mut dv = Vec::with_capacity(len as usize);
    unsafe {
        for i in 0..len {
            dv.push(*data.offset(i as isize) as u8);
        }
    }
    dv
}


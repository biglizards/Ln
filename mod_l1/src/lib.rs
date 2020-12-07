mod compiler;
mod interpreter;

extern crate l1_parser;

pub use compiler::l1_compiler;
pub use interpreter::l1_interpreter;
pub use l1_parser::L1;

use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Value {
    Bool(bool),
    Int(i64),
}

#[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
struct Location {
    id: i32,
}

trait Store {
    fn put(&mut self, k: Location, v: Value);
    fn get(&self, k: &Location) -> Option<&Value>;
    fn contains_key(&self, k: &Location) -> bool;
}

#[derive(Debug)]
struct HashStore {
    map: HashMap<Location, Value>,
}

impl Store for HashStore {
    fn put(&mut self, k: Location, v: Value) {
        self.map.insert(k, v);
    }

    fn get(&self, k: &Location) -> Option<&Value> {
        self.map.get(k)
    }

    fn contains_key(&self, k: &Location) -> bool {
        self.map.contains_key(k)
    }
}

#[derive(Debug)]
struct LinearStore {
    store: Vec<Value>,
}

impl Store for LinearStore {
    fn put(&mut self, k: Location, v: Value) {
        // panics if out of bounds
        // also this can only store 2**32 values, which is just awful
        self.store[k.id as usize] = v;
    }

    fn get(&self, k: &Location) -> Option<&Value> {
        self.store.get(k.id as usize)
    }

    fn contains_key(&self, k: &Location) -> bool {
        self.store.len() > k.id as usize
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

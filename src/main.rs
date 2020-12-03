extern crate parser;
use parser::L1;

mod compile_time;
mod interpreter;

use crate::compile_time::ct;
use crate::Expression::*;
use crate::Operation::*;
use crate::Value::*;
use std::collections::HashMap;
use std::env;
use std::time::Instant;

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

#[derive(Debug, Clone)]
enum Operation {
    ADD,
    GTE,
}

impl Operation {
    fn perform(&self, left: &Value, right: &Value) -> Option<Value> {
        match self {
            ADD => {
                if let (Int(a), Int(b)) = (left, right) {
                    Some(Int(a + b))
                } else {
                    None
                }
            }
            GTE => {
                if let (Int(a), Int(b)) = (left, right) {
                    Some(Bool(a >= b))
                } else {
                    None
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
enum Expression {
    VALUE(Value),
    OP(Box<Expression>, Operation, Box<Expression>),
    _IF {
        cond: Box<Expression>,
        then: Box<Expression>,
        else_: Box<Expression>,
    },
    ASSIGN(Location, Box<Expression>),
    DEREF(Location),
    SKIP,
    SEQ(Box<Expression>, Box<Expression>),
    WHILE {
        cond: Box<Expression>,
        do_: Box<Expression>,
    },
}

impl Expression {
    fn new_seq(e1: Expression, e2: Expression) -> Expression {
        SEQ(Box::new(e1), Box::new(e2))
    }

    fn new_assign(id: i32, e: Expression) -> Expression {
        ASSIGN(Location { id }, Box::new(e))
    }

    fn new_op(e1: Expression, op: Operation, e2: Expression) -> Expression {
        OP(Box::new(e1), op, Box::new(e2))
    }

    fn new_while(e1: Expression, e2: Expression) -> Expression {
        WHILE {
            cond: Box::new(e1),
            do_: Box::new(e2),
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let s: String = "1000000000".into();

    let now = Instant::now();
    let mut l1: i64 = args.get(1).unwrap_or(&s).parse().expect("int pls");
    let mut l2 = 0;
    while l1 >= 1 {
        l2 += l1;
        l1 -= 1;
    }
    println!("{}", l2);

    let time = (now.elapsed().as_nanos() as f64) / 1_000_000_000_f64;
    println!("{:?}", time);


    let now = Instant::now();
    ct::test(
        (&args.get(1).unwrap_or(&s))
            .parse()
            .expect("that's not an int, fool"),
    );
    let time = (now.elapsed().as_nanos() as f64) / 1_000_000_000_f64;
    println!("{:?}", time);
}

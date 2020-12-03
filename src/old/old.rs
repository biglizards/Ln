//
//
// use crate::Expression::*;
// use crate::Operation::*;
// use crate::Value::*;
// use std::collections::HashMap;
// use std::rc::Rc;
// use std::cell::{RefCell, RefMut, Cell};
// use std::iter::Skip;
//
// #[derive(Debug, Clone)]
// enum Value {
//     Bool(bool),
//     Int(i64),
// }
//
// #[derive(Debug, Hash, Eq, PartialEq, Clone)]
// struct Location {
//     id: i32,
// }
//
// #[derive(Debug)]
// struct Store {
//     map: HashMap<Location, Value>,
// }
//
// #[derive(Debug, Clone)]
// enum Operation {
//     ADD,
//     GTE,
// }
//
// impl Operation {
//     fn perform(&self, left: &Value, right: &Value) -> Option<Value> {
//         match self {
//             ADD => {
//                 if let (Int(a), Int(b)) = (left, right) {
//                     Some(Int(a + b))
//                 } else {
//                     None
//                 }
//             }
//             GTE => {
//                 if let (Int(a), Int(b)) = (left, right) {
//                     Some(Bool(a >= b))
//                 } else {
//                     None
//                 }
//             }
//         }
//     }
// }
//
// #[derive(Debug)]
// enum Expression {
//     VALUE(Value),
//     OP(Rc<RefCell<Expression>>, Operation, Rc<RefCell<Expression>>),
//     IF {
//         cond: Rc<RefCell<Expression>>,
//         then: Rc<RefCell<Expression>>,
//         else_: Rc<RefCell<Expression>>,
//     },
//     ASSIGN(Location, Rc<RefCell<Expression>>),
//     DEREF(Location),
//     SKIP,
//     SEQ(Rc<RefCell<Expression>>, Rc<RefCell<Expression>>),
//     WHILE {
//         cond: Rc<RefCell<Expression>>,
//         do_: Rc<RefCell<Expression>>,
//     },
// }
//
// impl Expression {
//     fn new_seq(e1: Expression, e2: Expression) -> Expression {
//         SEQ(Rc::from(RefCell::from(e1)), Rc::from(RefCell::from(e2)))
//     }
//
//     fn new_assign(id: i32, e: Expression) -> Expression {
//         ASSIGN(Location { id }, Rc::from(RefCell::from(e)))
//     }
//
//     fn new_op(e1: Expression, op: Operation, e2: Expression) -> Expression {
//         OP(Rc::from(RefCell::from(e1)), op, Rc::from(RefCell::from(e2)))
//     }
//
//     fn new_while(e1: Expression, e2: Expression) -> Expression {
//         WHILE {
//             cond: Rc::from(RefCell::from(e1)),
//             do_: Rc::from(RefCell::from(e2)),
//         }
//     }
// }
//
// fn to<T>(a: T) -> Option<Rc<RefCell<T>>> {
//     Some(Rc::from(RefCell::from(a)))
// }
//
// fn step(mut e: Rc<RefCell<Expression>>, s: &mut Store) -> Option<Rc<RefCell<Expression>>> {
//     //println!("borrowing {:?}", e);
//     let mut x = e.borrow_mut();
//     let mut y : Rc<RefCell<Expression>> = match &mut *x {
//         VALUE(_) => return None,
//         OP(ref mut e1, ref mut op, ref mut e2) => {
//             let mut to_step = 0;
//             {
//                 let x1 = e1.borrow_mut();
//                 let x2 = e2.borrow_mut();
//
//                 match (&*x1, &*x2) {
//                     (VALUE(v1), VALUE(v2)) => {
//                         return to(VALUE(op.perform(v1, v2)?));
//                     },
//                     (VALUE(_), _) => {
//                         to_step = 2
//                     },
//                     _ => {
//                         to_step = 1
//                     }
//                 }
//             }
//             match to_step {
//                 1 => *e1 = step(e1.clone(), s)?,
//                 2 => *e2 = step(e2.clone(), s)?,
//                 _ => panic!(":(")
//             }
//
//             e.clone()
//         },
//         IF { ref mut cond, ref mut then, ref mut else_ } => {
//             let mut next = None;
//             let rv = match &mut *cond.borrow_mut() {
//                 VALUE(v) => match v {
//                     Bool(b) => {
//                         if *b {
//                             then.clone()
//                         } else {
//                             else_.clone()
//                         }
//                     }
//                     _ => return None,
//                 },
//                 _ => {
//                     next = Some(cond.clone());
//                     e.clone()
//                 }
//             };
//             if let Some(e) = next {
//                 *cond = step(e, s)?;
//             }
//             rv
//
//         }
//         SKIP => return None,
//         ASSIGN(ref mut loc, ref mut e1) => {
//             let mut next = None;
//
//             if !s.map.contains_key(&loc) {
//                 return None;
//             };
//
//             let rv = match &*e1.borrow_mut() {
//                 VALUE(v) => {
//                     s.map.insert(loc.clone(), v.clone());
//                     return to(SKIP);
//                 }
//                 _ => {
//                     next = Some(e1.clone());
//                     e.clone()
//                 },
//             };
//             if let Some(a) = next {
//                 *e1 = step(a, s)?;
//             }
//             rv
//         }
//         DEREF(l) => return match s.map.get(&l) {
//             None => None,
//             Some(v) => to(VALUE(v.clone())),
//         },
//         SEQ(ref mut e1, ref mut e2) => {
//             if let SKIP = &*e1.borrow_mut() {
//                 return Some(e2.clone())
//             }
//             *e1 = step(e1.clone(), s)?;
//             e.clone()
//         }
//         WHILE { ref mut cond, ref mut do_ } => {
//             return to(
//                 IF {
//                     cond: (*cond).clone(),
//                     then: Rc::new(RefCell::new(SEQ(do_.clone(), e.clone()))),
//                     else_: Rc::new(RefCell::new(Expression::SKIP))
//                 }
//             );
//         }
//     };
//
//     Some(y)
// }
//
// fn main() {
//     let main = Expression::new_seq(
//         Expression::new_assign(2, VALUE(Int(0))),
//         Expression::new_while(
//             Expression::new_op(DEREF(Location { id: 1 }), GTE, VALUE(Int(1))),
//             Expression::new_seq(
//                 Expression::new_assign(
//                     2,
//                     Expression::new_op(DEREF(Location { id: 2 }), ADD, DEREF(Location { id: 1 })),
//                 ),
//                 Expression::new_assign(
//                     1,
//                     Expression::new_op(DEREF(Location { id: 1 }), ADD, VALUE(Int(-1))),
//                 ),
//             ),
//         ),
//     );
//
//     // let mut main = Expression::new_op(VALUE(Int(1)), ADD, VALUE(Int(2)));
//
//     let mut map = HashMap::new();
//     map.insert(Location { id: 1 }, Int(2));
//     map.insert(Location { id: 2 }, Int(0));
//     let mut store = Store { map };
//
//     let mut main2 = Rc::from(RefCell::from(main));
//
//     loop {
//         match step(main2, &mut store) {
//             None => break,
//             Some(e) => {
//                 println!("{:?}, {:?}", store, e);
//                 main2 = e
//             },
//         }
//     }
//
//     println!("{:?}", store);
//
//     // let data = Rc::new(RefCell::new(true));
//     // {
//     //     let mut reference = data.borrow_mut();
//     //     *reference = false;
//     // }
//     //
//     // let foo = Rc::from(RefCell::from(main));
//     // println!("{:?}", step(foo.clone(), &mut store))
// }

#[allow(dead_code)]
pub mod l2_interpreter {
    use self::Expression::*;
    use self::Value::{Bool, Int};

    #[derive(Debug, Clone)]
    pub(crate) enum Value {
        Bool(bool),
        Int(i64),
    }

    #[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
    pub(crate) struct Location {
        pub(crate) id: i32,
    }
    #[derive(Debug, Hash, Eq, PartialEq, Clone, Copy)]
    pub(crate) struct Variable {
        pub(crate) id: i32,
    }

    pub(crate) trait Store: Clone {
        fn put(&mut self, k: Location, v: i64);
        fn put_var(&mut self, k: Variable, v: i64);
        fn get(&self, k: &Location) -> Option<&i64>;
        fn get_var(&self, k: &Variable) -> Option<&i64>;
        fn contains_key(&self, k: &Location) -> bool;
        fn contains_var(&self, k: &Variable) -> bool;
    }

    #[derive(Debug, Clone)]
    pub(crate) struct LinearStore {
        pub(crate) store: Vec<i64>,
        pub(crate) var_store: Vec<i64>,
    }

    impl Store for LinearStore {
        fn put(&mut self, k: Location, v: i64) {
            // panics if out of bounds
            // also this can only store 2**32 values, which is just awful
            self.store[k.id as usize] = v;
        }

        fn put_var(&mut self, k: Variable, v: i64) {
            self.var_store[k.id as usize] = v;
        }

        fn get(&self, k: &Location) -> Option<&i64> {
            self.store.get(k.id as usize)
        }

        fn get_var(&self, k: &Variable) -> Option<&i64> {
            self.var_store.get(k.id as usize)
        }

        fn contains_key(&self, k: &Location) -> bool {
            self.store.len() > k.id as usize
        }

        fn contains_var(&self, k: &Variable) -> bool {
            self.var_store.len() > k.id as usize
        }
    }

    #[derive(Debug, Clone)]
    pub(crate) enum Operation {
        ADD,
        GTE,
    }

    impl Operation {
        fn perform(&self, left: &Value, right: &Value) -> Option<Value> {
            match self {
                Operation::ADD => {
                    if let (Int(a), Int(b)) = (left, right) {
                        Some(Int(a + b))
                    } else {
                        None
                    }
                }
                Operation::GTE => {
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
    pub(crate) enum Expression {
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
        FUNCTION(Variable, Box<Expression>),
        APPLICATION(Box<Expression>, Box<Expression>),
        VARIABLE(Variable),
    }

    pub(crate) fn step<S: Store>(e: Expression, s: &mut S) -> Option<Expression> {
        Some(match e {
            VALUE(_) => return None, //values don't reduce
            OP(e1, op, e2) => match (&*e1, &*e2) {
                (VALUE(v), VALUE(v2)) => VALUE(op.perform(v, v2)?),
                (VALUE(_), _) => OP(e1, op, Box::from(step(*e2, s)?)),
                _ => OP(Box::from(step(*e1, s)?), op, e2),
            },
            _IF { cond, then, else_ } => match &*cond {
                VALUE(v) => match v {
                    Bool(b) => {
                        if *b {
                            *then
                        } else {
                            *else_
                        }
                    }
                    _ => return None,
                },
                _ => _IF {
                    cond: Box::from(step(*cond, s)?),
                    then,
                    else_,
                },
            },
            ASSIGN(loc, e) => {
                if !s.contains_key(&loc) {
                    return None;
                };

                match *e {
                    VALUE(Int(v)) => {
                        s.put(loc, v);
                        SKIP
                    }
                    _ => ASSIGN(loc, Box::from(step(*e, s)?)),
                }
            }
            DEREF(l) => match s.get(&l) {
                None => return None,
                Some(v) => VALUE(Int(*v)),
            },
            SEQ(e1, e2) => match (&*e1, &*e2) {
                (SKIP, _) => step(*e2, s)?,
                _ => SEQ(Box::from(step(*e1, s)?), e2),
            },
            WHILE { cond, do_ } => _IF {
                cond: cond.clone(),
                then: Box::new(SEQ(do_.clone(), Box::new(WHILE { cond, do_ }))),
                else_: Box::new(Expression::SKIP),
            },
            APPLICATION(e1, e2) => match *e1 {
                FUNCTION(var, e) => *sub(var, *e, &*e2),
                _ => APPLICATION(Box::from(step(*e1, s)?), e2),
            },

            _ => return None,
        })
    }

    fn sub(var: Variable, e: Expression, new_expression: &Expression) -> Box<Expression> {
        Box::from(match e {
            OP(e1, op, e2) => OP(
                sub(var, *e1, new_expression),
                op,
                sub(var, *e2, new_expression),
            ),
            _IF { cond, then, else_ } => _IF {
                cond: sub(var, *cond, new_expression),
                then: sub(var, *then, new_expression),
                else_: sub(var, *else_, new_expression),
            },
            ASSIGN(l, e) => ASSIGN(l, sub(var, *e, new_expression)),
            SEQ(e1, e2) => SEQ(sub(var, *e1, new_expression), sub(var, *e2, new_expression)),
            WHILE { cond, do_ } => WHILE {
                cond: sub(var, *cond, new_expression),
                do_: sub(var, *do_, new_expression),
            },
            FUNCTION(var_2, e) => {
                if var == var_2 {
                    FUNCTION(var_2, e)
                } else {
                    FUNCTION(var_2, sub(var, *e, new_expression))
                }
            }
            APPLICATION(e1, e2) => {
                APPLICATION(sub(var, *e1, new_expression), sub(var, *e2, new_expression))
            }
            VARIABLE(v) => {
                if v == var {
                    new_expression.clone()
                } else {
                    VARIABLE(v)
                }
            }
            _ => e,
        })
    }
}

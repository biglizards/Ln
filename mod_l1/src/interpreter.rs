pub mod l1_interpreter {
    use self::Expression::*;
    use crate::Value::*;
    use crate::{Location, Store, Value};

    #[derive(Debug, Clone)]
    enum Operation {
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

    fn step<S: Store>(e: Expression, s: &mut S) -> Option<Expression> {
        Some(match e {
            VALUE(_) => e,
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
                    VALUE(v) => {
                        s.put(loc, v);
                        SKIP
                    }
                    _ => ASSIGN(loc, Box::from(step(*e, s)?)),
                }
            }
            DEREF(l) => match s.get(&l) {
                None => return None,
                Some(v) => VALUE(v.clone()),
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

            _ => return None,
        })
    }
}

mod interpreter {
    use crate::Expression::*;
    use crate::Value::*;
    use crate::{Expression, Store};

    fn big_step<S: Store>(mut e: Expression, s: &mut S) -> Option<Expression> {
        loop {
            if let Some(a) = step(e, s) {
                match &a {
                    VALUE(_) => return Some(a),
                    _ => e = a,
                }
            } else {
                return None;
            }
        }
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
            SEQ(e1, e2) => {
                big_step(*e1, s);
                big_step(*e2, s)?
                // match (&*e1, &*e2) {
                //     (SKIP, _) => step(*e2, s)?,
                //     _ => SEQ(Box::from(step(*e1, s)?), e2),
                // }
            }
            WHILE { cond, do_ } => {
                loop {
                    // evaluate the condition
                    match big_step(*cond.clone(), s)? {
                        VALUE(v) => match v {
                            Bool(b) => {
                                if b {
                                    big_step(*do_.clone(), s);
                                } else {
                                    break;
                                }
                            }
                            Int(_) => panic!("Condition was not a bool??"),
                        },
                        _ => panic!("big step returned non-value"),
                    };
                }
                SKIP
                //     IF {
                //         cond: cond.clone(),
                //         then: Box::new(SEQ(do_.clone(), Box::new(WHILE { cond, do_ }))),
                //         else_: Box::new(Expression::SKIP),
                //     }
            }

            _ => return None,
        })
    }
}

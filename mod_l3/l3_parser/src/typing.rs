use crate::generator::FreeVar;
use crate::generator::{Expression, Type};
use proc_macro2::{Span, TokenStream};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use syn::Error;

// Takes two types (which may be free, concrete, or some combination thereof (in the case of function))
// and, asserting that these types are equal, unifies them
// ie unifying (int->a) with (b->bool) will, where a and b are different FreeVars
//  1. set a to bool, and set b to int
//  2. return (int->bool)
fn unify_types(a: Type, b: Type, free_vars: &mut HashMap<String, FreeVar>) -> Result<Type, ()> {
    match (a, b) {
        (Type::FreeVar(f1), Type::FreeVar(f2)) => {
            let f = unify_vars(f1, f2, free_vars)?;
            Ok(Type::FreeVar(f))
        }
        (Type::FreeVar(f1), t) | (t, Type::FreeVar(f1)) => match &*f1.borrow() {
            None => {
                update_with_type(&f1, t.clone(), free_vars);
                Ok(t)
            }
            Some(t2) => {
                let unified_type = unify_types(t, (*t2).clone(), free_vars)?;
                if *t2 != unified_type {
                    update_with_type(&f1, unified_type.clone(), free_vars)
                }
                Ok(unified_type)
            }
        },

        (Type::Function(a1, b1), Type::Function(a2, b2)) => {
            let a = unify_types(*a1, *a2, free_vars)?;
            let b = unify_types(*b1, *b2, free_vars)?;
            Ok(Type::Function(Box::from(a), Box::from(b)))
        }

        (a, b) => {
            // we assert that:
            //  neither a nor b can contain a free variable, because
            //  they must be basic variants (and contain no types)
            if a == b {
                Ok(a)
            } else {
                Err(())
            }
        }
    }
}


// Takes two free variables, and makes them the same
// eg unifying Some(int) and None will mutably update the latter to point to the former
//
// after you unify two free vars, you MUST replace all free vars equal to a or b with the result
// this is (i think) unavoidably O(n)
// note from future self: why can't you just add another layer of indirection?
// i remember there was a good reason, but it escapes me now
fn unify_vars(
    a: FreeVar,
    b: FreeVar,
    free_vars: &mut HashMap<String, FreeVar>,
) -> Result<FreeVar, ()> {
    // todo: if possible, remove ugly hack to appease borrow checker
    enum RV {
        A,
        B,
    };

    let x = match (&*a.borrow(), &*b.borrow()) {
        (None, None) | (Some(_), None) => {
            // unify to the same pointer
            update_free_vars(&b, &a, free_vars);
            RV::A
        }
        (None, Some(_)) => {
            update_free_vars(&a, &b, free_vars);
            RV::B
        }
        (Some(_), Some(_)) => {
            let t1 = a.borrow().clone().unwrap();
            let t2 = b.borrow().clone().unwrap();
            let new_type = unify_types(t1, t2, free_vars)?;
            update_free_vars(&b, &a, free_vars);
            update_with_type(&a, new_type, free_vars);
            RV::A
        }
    };
    match x {
        RV::A => Ok(a),
        RV::B => Ok(b),
    }
}

fn update_with_type(old_var: &FreeVar, t: Type, free_vars: &mut HashMap<String, FreeVar>) {
    update_free_vars(old_var, &Rc::new(RefCell::new(Some(t))), free_vars)
}

fn update_free_vars(
    old_var: &FreeVar,
    new_var: &FreeVar,
    free_vars: &mut HashMap<String, FreeVar>,
) {
    free_vars.iter_mut().for_each(|(_, v)| {
        if Rc::ptr_eq(v, old_var) {
            *v = new_var.clone()
        }
    })
}

fn assert_type(
    e: &Expression,
    t: &Type,
    variables: HashMap<String, Type>,
    free_vars: &mut HashMap<String, FreeVar>,
) -> Result<Type, TokenStream> {
    let t1 = infer_type(e, variables, Some(t.clone()), free_vars)?;
    if let Ok(t) = unify_types(t1.clone(), t.clone(), free_vars) {
        Ok(t)
    } else {
        Err(
            Error::new(e.span(), format!("Expected `{:?}`, found `{:?}`", t, t1))
                .to_compile_error(),
        )
    }
}

/// oh no i actually need to do type inference
pub fn infer_type(
    e: &Expression,
    mut variables: HashMap<String, Type>,
    ret_type: Option<Type>,
    free_vars: &mut HashMap<String, FreeVar>,
) -> Result<Type, TokenStream> {
    Ok(match e {
        Expression::Int(_) => Type::Int,
        Expression::Deref(_, _) => {
            // we don't actually know right now
            // there's gonna need to be some map like `variables` in the future
            // so as to not break things we'll use Int for now
            // todo when you add variable type locations, fix this
            Type::Int
        }
        Expression::Add(e1, e2) => {
            assert_type(e1, &Type::Int, variables.clone(), free_vars)?;
            assert_type(e2, &Type::Int, variables, free_vars)?;
            Type::Int
        }
        Expression::Bool(_) => Type::Bool,
        Expression::GE(e1, e2) => {
            assert_type(e1, &Type::Int, variables.clone(), free_vars)?;
            assert_type(e2, &Type::Int, variables, free_vars)?;
            Type::Bool
        }
        Expression::Seq(e1, e2) => {
            assert_type(e1, &Type::Unit, variables.clone(), free_vars)?;
            infer_type(e2, variables, None, free_vars)?
        }
        Expression::If(e1, e2, e3, _) => {
            assert_type(e1, &Type::Bool, variables.clone(), free_vars)?;
            let t = infer_type(e2, variables.clone(), None, free_vars)?;
            assert_type(e3, &t, variables, free_vars)?;
            t
        }

        Expression::Skip(_) => Type::Unit,

        Expression::Assign(_l, e) => {
            // todo: assert that e is of type `type(l)`
            let _ = infer_type(e, variables, None, free_vars);
            Type::Unit
        }
        Expression::While(e1, e2, _) => {
            assert_type(e1, &Type::Bool, variables.clone(), free_vars)?;
            assert_type(e2, &Type::Unit, variables, free_vars)?;
            Type::Unit
        }

        Expression::Function(l, e, Some(t_in), Some(t_out), _) => {
            // honestly i dont think this assertion can ever fail
            variables.insert(l.to_string(), t_in.clone());
            assert_type(e, t_out, variables, free_vars)?;

            Type::Function(Box::from(t_in.clone()), Box::from(t_out.clone()))
        }
        Expression::Function(l, e, Some(t_in), None, _) => {
            variables.insert(l.to_string(), t_in.clone());
            let t_out = infer_type(&*e, variables, None, free_vars)?;
            Type::Function(Box::from(t_in.clone()), Box::from(t_out))
        }
        Expression::Function(_, _, _, _, _) => {
            panic!("function didn't have input type? This shouldn't be valid syntax")
        }

        Expression::Application(e1, e2) => {
            if let Some(t_out) = ret_type {
                let t_in = infer_type(&*e2, variables.clone(), None, free_vars)?;
                assert_type(
                    e1,
                    &Type::Function(Box::from(t_in), Box::from(t_out.clone())),
                    variables,
                    free_vars,
                )?;
                t_out
            } else {
                let t_func = infer_type(&*e1, variables.clone(), None, free_vars)?;
                // make sure the application is valid
                if let Type::Function(t_f_in, t_f_out) = t_func {
                    assert_type(&*e2, &t_f_in, variables, free_vars)?;
                    *t_f_out
                } else {
                    return Err(Error::new(
                        e1.span(),
                        format!("Expected function, found `{:?}`", t_func),
                    )
                    .to_compile_error());
                }
            }
        }

        Expression::Variable(v) => {
            // 1. a bound variable always has a fully quantified type
            // 2. if a variable is bound, ignore any free variables it may shadow
            // 3. the ret_type might not be fully quantified (eg if v is a function)

            let v_str = v.to_string();
            match (variables.get(&v_str), free_vars.get(&v_str), ret_type) {
                (Some(t1), _, None) => t1.clone(),
                (None, Some(t), None) => Type::FreeVar(t.clone()),

                (Some(t1), _, Some(t2)) => {
                    unify_types(t1.clone(), t2.clone(), free_vars).map_err(|_| {
                        Error::new(
                            v.span(),
                            format!("Expected `{:?}`, found variable `{:?}`", t2, t1),
                        )
                        .to_compile_error()
                    })?
                }
                (None, Some(t1), Some(t2)) => {
                    let t11 = (*t1).borrow().clone();
                    let t22 = t2.clone();
                    unify_types(Type::FreeVar(t1.clone()), t2, free_vars).map_err(|_| {
                        Error::new(
                            v.span(),
                            format!(
                                "Expected `{:?}`, found variable of type `{:?}`",
                                t22,
                                t11.unwrap()
                            ),
                        )
                        .to_compile_error()
                    })?
                }

                (None, None, _) => {
                    unreachable!(
                        "variables should be either free or bound -- {} is neither!",
                        v
                    );
                }
            }
        }

        Expression::Let(v, t, arg, e2, _) => {
            assert_type(arg, t, variables.clone(), free_vars)?;
            variables.insert(v.to_string(), t.clone());
            infer_type(&*e2, variables, None, free_vars)?
        }
        Expression::LetRec(x, t, y, e1, e2, _) => {
            variables.insert(x.to_string(), t.clone());
            let rv = infer_type(&*e2, variables.clone(), None, free_vars)?;
            if let Type::Function(t1, t2) = t {
                variables.insert(y.to_string(), *t1.clone());
                assert_type(e1, t2, variables, free_vars)?;
                rv
            } else {
                panic!("todo add error")
                // new_err(t.span(), &format!("Expected Function, found `{:?}`", t))
            }
        }
    })
}

fn new_err<T>(span: Span, message: &str) -> Result<T, TokenStream> {
    Err(Error::new(span, message).to_compile_error())
}

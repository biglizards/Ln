extern crate proc_macro;
extern crate proc_macro2;

use std::collections::{HashMap, HashSet};

use proc_macro::{TokenStream};
use quote::quote;
use syn::{
    ext::IdentExt,
    parenthesized,
    parse::{Lookahead1, Parse, ParseBuffer, ParseStream},
    parse_macro_input, token, Ident, LitBool, LitInt, Result, Token
};
// this import doesn't actually do anything, but it silences a false positive error in CLion
#[allow(unused_imports)]
use syn::token::Token;

use crate::rewrite::Reduce;
use proc_macro2::Span;

#[derive(Debug, Clone, Eq, PartialEq)]
enum Type {
    Int,
    Bool,
    Unit,
    Function(Box<Type>, Box<Type>),
}

#[derive(Debug)]
enum Expression {
    Int(LitInt),
    Bool(LitBool),
    Add(Box<Expression>, Box<Expression>),
    GE(Box<Expression>, Box<Expression>),
    Seq(Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Deref(Ident),
    Assign(Ident, Box<Expression>),
    Skip,
    While(Box<Expression>, Box<Expression>),
    Function(Ident, Box<Expression>, Option<Type>, Option<Type>),
    Application(Box<Expression>, Box<Expression>),
    Variable(Ident),
    Let(Ident, Type, Box<Expression>, Box<Expression>),
    LetRec(Ident, Type, Ident, Box<Expression>, Box<Expression>),
}

fn parse_type(input: ParseStream) -> Result<Type> {
    let mut lookahead = input.lookahead1();
    let mut types: Vec<Type> = vec![];
    loop {
        if lookahead.peek(Ident::peek_any) {
            types.push(token_to_type(
                &*input.parse::<Ident>()?.to_string(),
                lookahead,
            )?);
            lookahead = input.lookahead1();
            if lookahead.peek(Token![->]) {
                let _ = input.parse::<Token![->]>()?;
                lookahead = input.lookahead1();
                continue;
            }
        } else if lookahead.peek(token::Paren) {
            let content: ParseBuffer;
            parenthesized!(content in input);
            types.push(parse_type(&content)?);
            lookahead = input.lookahead1();
            if lookahead.peek(Token![->]) {
                let _ = input.parse::<Token![->]>()?;
                lookahead = input.lookahead1();
                continue;
            }
        }
        break;
    }

    if types.is_empty() {
        return Err(lookahead.error());
    }

    // a->b->c->d
    // a->(b->(c->d))

    // a, b, c @ d
    // a, b, @ F(c,d)
    // a @ F(b, F(c, d))
    // F(a, F(b, F(c, d)))

    let mut rv: Type = types.pop().unwrap();

    while let Some(t) = types.pop() {
        rv = Type::Function(Box::from(t), Box::from(rv));
    }

    return Ok(rv);
}

fn type_to_token(t: Type) -> proc_macro2::TokenStream {
    match t {
        Type::Int => quote! {i64},
        Type::Bool => quote! {bool},
        Type::Unit => quote! {Skip},
        Type::Function(t_in, t_out) => {
            let (ts_in, ts_out) = (type_to_token(*t_in), type_to_token(*t_out));
            quote! {std::rc::Rc<dyn Fn(#ts_in) -> #ts_out>}
        }
    }
}

fn token_to_type(t: &str, lookahead: Lookahead1) -> Result<Type> {
    match t {
        "int" | "Int" | "i64" => Ok(Type::Int),
        "unit" | "Unit" | "()" => Ok(Type::Unit),
        "bool" | "Bool" => Ok(Type::Bool),
        _ => Err(lookahead.error()),
    }
}

/// oh no i actually need to do type inference
/// somehow i manged to put that off until L2
/// and really i can mostly put it off until L3 actually
/// instead of doing it properly I just optimistically assume the expression is well-typed
/// this works since if it's not well typed, the rust compiler will catch it
fn infer_type(e: &Expression, mut variables: HashMap<String, Type>) -> Type {
    match e {
        Expression::Int(_) => Type::Int,
        Expression::Bool(_) => Type::Bool,
        Expression::Add(_, _) => Type::Int,
        Expression::GE(_, _) => Type::Bool,
        Expression::Seq(_, e) => infer_type(&*e, variables),
        Expression::If(_, e, _) => infer_type(&*e, variables),
        Expression::Deref(_) => Type::Int,
        Expression::Assign(_, _) => Type::Unit,
        Expression::Skip => Type::Unit,
        Expression::While(_, _) => Type::Unit,

        Expression::Function(_, _, Some(t_in), Some(t_out)) => {
            Type::Function(Box::from(t_in.clone()), Box::from(t_out.clone()))
        }
        Expression::Function(l, e, Some(t_in), None) => {
            variables.insert(l.to_string(), t_in.clone());
            let t_out = infer_type(&*e, variables);
            Type::Function(Box::from(t_in.clone()), Box::from(t_out))
        }
        Expression::Function(_, _, _, _) => {
            panic!("function didn't have input type? This shouldn't be valid syntax")
        }

        Expression::Application(e1, e2) => {
            let t_func = infer_type(&*e1, variables.clone());
            let t_in = infer_type(&*e2, variables);
            // make sure the application is valid
            if let Type::Function(t_f_in, t_f_out) = t_func {
                if *t_f_in != t_in {
                    // todo helpful error message with location
                    // panic!(format!("cannot apply {:?} to function expecting {:?}", t_in, t_f_in));
                    *t_f_out
                } else {
                    // valid, return this type
                    *t_f_out
                }
            } else {
                // todo helpful error message with location
                panic!(format!(
                    "LHS of application wasn't a function, it was {:?}",
                    t_func
                ))
            }
        }
        Expression::Variable(v) => {
            let v_str = &*v.to_string();
            match variables.get(v_str) {
                None => {
                    panic!(format!("Variable {} not found in scope!", v_str))
                }
                Some(t) => t.clone(),
            }
        }
        Expression::Let(v, t, _, e2) => {
            variables.insert(v.to_string(), t.clone());
            infer_type(&*e2, variables)
        }
        Expression::LetRec(x, t, _, _, e2) => {
            variables.insert(x.to_string(), t.clone());
            infer_type(&*e2, variables)
        }
    }
}

/// updates the vector with most* free** variables inside an expression which are non-copy types
/// used to clone non-copy variables into closures
/// *: variables inside functions aren't returned, since the clone only needs to happen in
///    the innermost call
/// **: by free I mean free in the Expression passed to us (ie not bound internally)
///     all variables should be bound in an outer scope (and therefore in `variables`)
/// `variables` is a mapping from variables to type of all variables bound in an outer scope
///             GUARANTEE: variables is always the same after calling (but is mutated while calling)
fn get_non_copy_free_variables(
    e: &mut Box<Expression>,
    variables: &mut HashMap<String, Type>,
    free_vars: &mut Vec<Ident>,
) {
    if let Expression::Variable(v) = &**e {
        let var_name = v.to_string();
        if let Some(t) = variables.get(&var_name) {
            // what? why?
            if let Type::Function(_, _) = t {
                free_vars.push(v.clone());
            }
        }
    } else {
        match &mut **e {
            Expression::Int(_) => (),
            Expression::Bool(_) => (),
            Expression::Add(ref mut e1, ref mut e2) => {
                get_non_copy_free_variables(e1, variables, free_vars);
                get_non_copy_free_variables(e2, variables, free_vars)
            }
            Expression::GE(ref mut e1, ref mut e2) => {
                get_non_copy_free_variables(e1, variables, free_vars);
                get_non_copy_free_variables(e2, variables, free_vars)
            }
            Expression::Seq(ref mut e1, ref mut e2) => {
                get_non_copy_free_variables(e1, variables, free_vars);
                get_non_copy_free_variables(e2, variables, free_vars)
            }
            Expression::If(ref mut e1, ref mut e2, ref mut e3) => {
                get_non_copy_free_variables(e1, variables, free_vars);
                get_non_copy_free_variables(e2, variables, free_vars);
                get_non_copy_free_variables(e3, variables, free_vars)
            }
            Expression::Deref(_) => (),
            Expression::Assign(_, ref mut e1) => {
                get_non_copy_free_variables(e1, variables, free_vars)
            }
            Expression::Skip => (),
            Expression::While(ref mut e1, ref mut e2) => {
                get_non_copy_free_variables(e1, variables, free_vars);
                get_non_copy_free_variables(e2, variables, free_vars)
            }
            // don't recurse into functions, they will call this function separately
            Expression::Function(_, _, _, _) => (),
            Expression::Application(ref mut e1, ref mut e2) => {
                get_non_copy_free_variables(e1, variables, free_vars);
                get_non_copy_free_variables(e2, variables, free_vars)
            }
            Expression::Variable(_) => unreachable!("we already checked for this case"),
            Expression::Let(v, t, e1, e2) => {
                get_non_copy_free_variables(e1, variables, free_vars);
                let old_value = variables.insert(v.to_string(), t.clone());
                get_non_copy_free_variables(e2, variables, free_vars);
                // put variables back how it was
                match old_value {
                    None => variables.remove(&*v.to_string()),
                    Some(old_value) => variables.insert(v.to_string(), old_value),
                };
            }
            Expression::LetRec(v, t, y, _e1, e2) => {
                let old_value = variables.insert(v.to_string(), t.clone());
                get_non_copy_free_variables(e2, variables, free_vars);

                if let Type::Function(t1, _) = t {
                    let old_value2 = variables.insert(y.to_string(), (**t1).clone());
                    get_non_copy_free_variables(e2, variables, free_vars);
                    // put variables back how it was
                    match old_value2 {
                        None => variables.remove(&*y.to_string()),
                        Some(old_value) => variables.insert(y.to_string(), old_value),
                    };
                } else {
                    // todo catch this earlier
                    panic!("let rec type wasn't a function!")
                }

                // put variables back how it was
                match old_value {
                    None => variables.remove(&*v.to_string()),
                    Some(old_value) => variables.insert(v.to_string(), old_value),
                };
            }
        }
    }
}

fn get_all_free_variables(
    e: &Box<Expression>,
    variables: &HashMap<String, Type>,
    ignore_set: &HashSet<String>,
    free_vars: &mut HashSet<String>,
    appears_not_on_lhs_of_apply: &mut bool,
    target: &String,
    is_on_lhs_of_apply: bool
) {
    match &**e {
        Expression::Int(_) => {}
        Expression::Bool(_) => {}
        Expression::Add(e1, e2) => {
            get_all_free_variables(e1, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
            get_all_free_variables(e2, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
        }
        Expression::GE(e1, e2) => {
            get_all_free_variables(e1, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
            get_all_free_variables(e2, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
        }
        Expression::Seq(e1, e2) => {
            get_all_free_variables(e1, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
            get_all_free_variables(e2, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
        }
        Expression::If(e1, e2, e3) => {
            get_all_free_variables(e1, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
            get_all_free_variables(e2, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
            get_all_free_variables(e3, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
        }
        Expression::Deref(_) => {}
        Expression::Assign(_, e1) => {
            get_all_free_variables(e1, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
        }
        Expression::Skip => {}
        Expression::While(e1, e2) => {
            get_all_free_variables(e1, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
            get_all_free_variables(e2, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
        }
        Expression::Function(v, e1, _, _) => {
            let mut ignore_set = ignore_set.clone();
            ignore_set.insert(v.to_string());
            get_all_free_variables(e1, variables, &ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
        }
        Expression::Application(e1, e2) => {
            get_all_free_variables(e1, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, true);
            get_all_free_variables(e2, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
        }
        Expression::Variable(v) => {
            if !is_on_lhs_of_apply && v.to_string() == *target {
                *appears_not_on_lhs_of_apply = true;
            }
            
            if ignore_set.contains(&*v.to_string()) {
                return;
            }

            if !variables.contains_key(&*v.to_string()) {
                // todo helpful error message
                panic!(format!(
                    "variable {} used without definition! (at least, we don't know the type of it)",
                    v.to_string()
                ))
            }


            free_vars.insert(v.to_string());
        }
        Expression::Let(v, _, e1, e2) => {
            let mut ignore_set_2 = ignore_set.clone();
            ignore_set_2.insert(v.to_string());
            get_all_free_variables(e1, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
            get_all_free_variables(e2, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
        }
        Expression::LetRec(x, _, y, e1, e2) => {
            let mut ignore_set_1 = ignore_set.clone();
            ignore_set_1.insert(x.to_string());
            get_all_free_variables(e2, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
            ignore_set_1.insert(y.to_string());
            get_all_free_variables(e1, variables, ignore_set, free_vars, appears_not_on_lhs_of_apply, target, false);
        }
    }
}

/// converts an expression (in our internal representation) back into a token stream
fn to_token_stream(
    e: Expression,
    mut variables: HashMap<String, Type>,
) -> proc_macro2::TokenStream {
    match e {
        Expression::Int(e) => quote! {#e},
        Expression::Bool(b) => quote! {#b},
        Expression::Add(e1, e2) => {
            let (ts1, ts2) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables),
            );
            quote! {mod_l2::l2_compiler::Add {e1: #ts1, e2: #ts2}}
        }
        Expression::GE(e1, e2) => {
            let (ts1, ts2) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables),
            );
            quote! {mod_l2::l2_compiler::GE {e1: #ts1, e2: #ts2}}
        }
        Expression::Seq(e1, e2) => {
            let (ts1, ts2) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables),
            );
            quote! {mod_l2::l2_compiler::Seq {e1: #ts1, e2: #ts2}}
        }
        Expression::If(e1, e2, e3) => {
            let (ts1, ts2, ts3) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables.clone()),
                to_token_stream(*e3, variables),
            );
            quote! {mod_l2::l2_compiler::If {e1: #ts1, e2: #ts2, e3: #ts3}}
        }
        Expression::Deref(l) => {
            quote! {mod_l2::l2_compiler::Deref {l: #l.clone()}}
        }
        Expression::Assign(l, e) => {
            let ts = to_token_stream(*e, variables);
            quote! {mod_l2::l2_compiler::Assign {l: #l.clone(), e1: #ts}}
        }
        Expression::Skip => quote! {mod_l2::l2_compiler::Skip {}},
        Expression::While(e1, e2) => {
            let (ts1, ts2) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables),
            );
            quote! {mod_l2::l2_compiler::While {e1: #ts1, e2: #ts2}}
        }
        Expression::Function(l, mut e, Some(t_in), Some(t_out)) => {
            // bugfix: if this function uses a variable f: Rc<_>, we need to clone it _before_
            // entering the closure, not after. We only need to create one copy per variable.
            // meta bugfix: don't try to clone the argument.
            variables.remove(&l.to_string());

            let mut count = vec![];
            get_non_copy_free_variables(&mut e, &mut variables, &mut count);
            let count = count.iter();

            variables.insert(l.to_string(), t_in.clone());
            let ts = to_token_stream(*e, variables);
            let (ts_in, ts_out) = (type_to_token(t_in), type_to_token(t_out));
            quote! {
                {
                    #(
                        let #count = #count.clone();
                    )*
                    (std::rc::Rc::from(move |#l : #ts_in| (#ts).step()) as std::rc::Rc<dyn Fn(#ts_in) -> #ts_out>)
                }
            }
        }
        Expression::Function(l, e, Some(t_in), None) => {
            variables.insert(l.to_string(), t_in.clone());
            let t_out = infer_type(&e, variables.clone());
            to_token_stream(
                Expression::Function(l, e, Some(t_in), Some(t_out)),
                variables,
            )
        }
        Expression::Function(_, _, _, _) => {
            panic!("Could not infer type of function!")
        }
        Expression::Application(e1, e2) => {
            let (ts1, ts2) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables),
            );
            quote! {mod_l2::l2_compiler::Apply {e1: #ts1, e2: #ts2, phantom: Default::default()}}
        }
        Expression::Variable(v) => {
            if let Some(t) = variables.get(&*v.to_string()) {
                if let Type::Function(_, _) = t {
                    // functions are represented by Rc<>
                    // we have to clone, since they're not copy
                    quote! {#v.clone()}
                } else {
                    quote! {#v}
                }
            } else {
                // todo have error message be helpful
                eprintln!(
                    "Warning: could not infer type for variable: {:?}",
                    v.to_string()
                );
                quote! {#v}
            }
        }
        Expression::Let(v, t, e1, e2) => {
            let ts1 = to_token_stream(*e1, variables.clone());
            let type_token = type_to_token(t.clone());
            variables.insert(v.to_string(), t);
            let ts2 = to_token_stream(*e2, variables);


            let rv = quote! {
                {let #v: #type_token = (#ts1).step(); (#ts2).step()}
            };

            rv
        }
        Expression::LetRec(x, t, y, e1, e2) => {
            // in rust we can't have a closure recurse into itself (without dyn)
            // but you can define functions inline
            // but you can't capture scope in functions (only in closures)
            // so what if you define a closure to capture the scope, which then passes it as
            // arguments to the recursive function

            // let s = 1;
            // let sum = move |x: i64| {
            //     fn sum(s: i64, x: i64) -> i64 {
            //         let sum = move |x| {sum(s,x)};
            //         // now we put the inner code in as normal
            //         if 0>=x {s} else {x+sum(x+-1)}
            //     }; sum(s, x)
            // };

            // pros: actually optimises well when all captured scope is copy
            // cons: when dealing with functions from outer scope, there's some overhead
            //       like, it goes from 2ns to 50us
            //       loops don't get unrolled so performance is O(n) instead of O(1)
            // if you leak memory (or if you're very careful, manage it manually)
            // you can regain O(1) time, so this line of optimisation might be worth looking into

            // specifically, you need to clone the arguments _every recursive call_
            // it's just an rc cloning, but still, what a pain

            // let add = Rc::from(|x,y| {x+y});
            // let sum = {
            //     let add = add.clone();
            //     move |x: i64| {
            //         fn sum(add: Rc<dyn Fn(i64, i64) -> i64>, x: i64) -> i64 {
            //             let sum = {
            //                 let add = add.clone();
            //                 move |x| {sum(add,x)}
            //             };
            //             // now we put the inner code in as normal
            //             if 0>=x {1} else {x+sum(add(x, -1))}
            //         }
            //         sum(add, x)
            //     };
            // };

            // so, to make this:
            // step 1: identify ALL variables from outer scope (ie free variables in e1)
            // step 2: identify separately ALL non-copy variables (ie functions)
            // step 3: construct closure out of token streams

            let mut free_vars = HashSet::new();
            let mut ignore_set = HashSet::new();

            variables.insert(x.to_string(), t.clone());
            ignore_set.insert(x.to_string());

            let e2_ts = to_token_stream(*e2, variables.clone());

            let (t1, t2) = if let Type::Function(t1, t2) = t {
                variables.insert(y.to_string(), *t2.clone());
                ignore_set.insert(y.to_string());
                (t1, t2)
            } else {
                unreachable!("i _know_ it's a function, we already checked")
            };

            let mut appears_not_on_lhs_of_apply = false;

            get_all_free_variables(&e1, &variables, &ignore_set, &mut free_vars, &mut appears_not_on_lhs_of_apply, &x.to_string(), false);
            let free_var_types: Vec<_> = free_vars.iter().map(|v| {
                type_to_token(
                    variables.get(v).expect("We already checked they were in here").clone()
                )
            }).collect();

            // step 2: get all non-copy variables
            let clone_vars2: Vec<Ident> = free_vars
                .clone()
                .into_iter()
                .filter_map(|v| {
                    if let Some(Type::Function(_, _)) = variables.get(&*v) {
                        Some(Ident::new(&*v, Span::call_site()))
                    } else {
                        None
                    }
                })
                .collect();

            let captures_no_scope = clone_vars2.is_empty();

            let clone_vars = clone_vars2.iter();
            let clone_vars2 = clone_vars2.iter();

            let (t1_ts, t2_ts) = (type_to_token(*t1), type_to_token(*t2.clone()));
            let e1_ts = to_token_stream(*e1, variables.clone());

            let free_vars: Vec<Ident> = free_vars.into_iter().map( |v|
                Ident::new(&*v, Span::call_site())
            ).collect();

            // cool optimisation you could do here:
            // if the inner closure only appears in the LHS of an Apply
            // (this is sadly a limitation of rust's type system)
            // AND the recursive function returns a primitive
            // (both are pretty common)
            // AND the closures doesn't capture any scope
            // (this one is less common)
            // then we can change that inner closure from an Rc<Fn()> to a fn()
            // this allows for better optimisation, for whatever reason.

            let only_appears_on_lhs_of_apply = !appears_not_on_lhs_of_apply;
            let returns_primitive = if let Type::Function(_, _) = *t2 {false} else {true};

            let inner_closure = if only_appears_on_lhs_of_apply && returns_primitive && captures_no_scope {
                quote! {
                    (move |#y: #t1_ts| {#x(#(#free_vars.clone() ,)* #y)}) as fn(#t1_ts) -> #t2_ts
                }
            } else {
                quote! {
                    #(let #clone_vars2 = #clone_vars2.clone();)*
                    std::rc::Rc::from(move |#y: #t1_ts| {#x(#(#free_vars.clone() ,)* #y)}) as std::rc::Rc<dyn Fn(#t1_ts) -> #t2_ts>
                }
            };

            quote! {
                    {
                        let #x = {
                            #(
                                let #clone_vars = #clone_vars.clone();
                            )*
                            std::rc::Rc::from(move |#y : #t1_ts| {
                                fn #x (#(#free_vars : #free_var_types ,)* #y: #t1_ts) -> #t2_ts {
                                    let #x = {
                                        #inner_closure
                                    };
                                    #e1_ts.step()
                                }
                                #x(#(#free_vars.clone() ,)* #y)
                            }) as std::rc::Rc<dyn Fn(#t1_ts) -> #t2_ts>
                        };
                        #e2_ts.step()
                    }
            }
        }
    }
}

/// converts an expression (in our internal representation) back into a token stream
/// for the interpreter module
fn to_interpreted_token_stream(e: Expression) -> proc_macro2::TokenStream {
    match e {
        Expression::Int(e) => {
            quote! {mod_l2::l2_interpreter::Expression::VALUE(mod_l2::l2_interpreter::Value::Int(#e))}
        }
        Expression::Bool(b) => {
            quote! {mod_l2::l2_interpreter::Expression::VALUE(mod_l2::l2_interpreter::Value::Bool(#b))}
        }
        Expression::Add(e1, e2) => {
            let (ts1, ts2) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
            );
            quote! {mod_l2::l2_interpreter::Expression::OP(Box::from(#ts1), mod_l2::l2_interpreter::Operation::ADD, Box::from(#ts2))}
        }
        Expression::GE(e1, e2) => {
            let (ts1, ts2) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
            );
            quote! {mod_l2::l2_interpreter::Expression::OP(Box::from(#ts1), mod_l2::l2_interpreter::Operation::GTE, Box::from(#ts2))}
        }
        Expression::Seq(e1, e2) => {
            let (ts1, ts2) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
            );
            quote! {mod_l2::l2_interpreter::Expression::SEQ(Box::from(#ts1), Box::from(#ts2))}
        }
        Expression::If(e1, e2, e3) => {
            let (ts1, ts2, ts3) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
                to_interpreted_token_stream(*e3),
            );
            quote! {mod_l2::l2_interpreter::Expression::IF {
                    cond: Box::from(#ts1),
                    then: Box::from(#ts2),
                    else_: Box::from(#ts3)
                }
            }
        }
        Expression::Deref(l) => {
            quote! {mod_l2::l2_interpreter::Expression::DEREF(#l)}
        }
        Expression::Assign(l, e) => {
            let ts = to_interpreted_token_stream(*e);
            quote! {mod_l2::l2_interpreter::Expression::ASSIGN(#l, Box::from(#ts))}
        }
        Expression::Skip => quote! {mod_l2::l2_interpreter::Expression::SKIP {}},
        Expression::While(e1, e2) => {
            let (ts1, ts2) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
            );
            quote! {mod_l2::l2_interpreter::Expression::WHILE {
                    cond: Box::from(#ts1),
                    do_: Box::from(#ts2),
                }
            }
        }
        Expression::Function(l, e, _, _) => {
            let ts = to_interpreted_token_stream(*e);
            quote! {mod_l2::l2_interpreter::Expression::FUNCTION(#l, Box::from(#ts))}
        }
        Expression::Application(e1, e2) => {
            let (ts1, ts2) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
            );
            quote! {mod_l2::l2_interpreter::Expression::APPLICATION(Box::from(#ts1), Box::from(#ts2))}
        }
        Expression::Variable(v) => {
            quote! {mod_l2::l2_interpreter::Expression::VARIABLE(#v)}
        }
        Expression::Let(_, _, _, _) => {
            unimplemented!("no interpreter for let bindings yet")
        }
        Expression::LetRec(_, _, _, _, _) => {
            unimplemented!("no interpreter for let bindings yet")
        }
    }
}

/// The original grammar is
/// e := n | b | x | (e) | e + e | e >= e | e ; e | e e | l := e | !l | skip
///    | if e then e else e | while e do e | fn x:T => e | let val x: t = e in e
///    | let
/// but now to add precedence i'm changing it to
/// e := As | As ; e | if e then e else e | fn x:T => e
/// As := T | l := As | T >= T
/// T := Ap | Ap + T
/// Ap := (e) | n | x | !l | b | skip | Ap Ap | let x: t = e in e end
///
/// I assert that these grammars accept the same language
/// todo: find a way to prove it
///
/// A few notes:
///   - I chose to make expressions like `if-then-else` and `while` greedy. This means that
///     `if true then skip;1 else 2; l:=1` is parsed like
///     `if true then skip;1 else (2; l:=1)` (which is a typing error)
///     This is because you can have `Seq` in the second expression, so it'd be weird to
///     not be able to have it in the third without parens
///
///
/// test cases:
/// (1 + 2) + 3
/// 1 + (2 + 3)
/// 1 + (1; 2)
/// l := (l := 1)
/// l := (1 ; 2)
///
/// 1 + f 2 + 3
///   should be add(1, add(app(f, 2),3))
mod rewrite {
    use super::*;

    mod kw {
        syn::custom_keyword!(skip);
        syn::custom_keyword!(then);
        syn::custom_keyword!(val);
        syn::custom_keyword!(end);
        syn::custom_keyword!(rec);
    }

    /// Checks if the next token is an identifier. Does not count custom keywords as identifiers.
    fn is_ident(lookahead: &Lookahead1) -> bool {
        if lookahead.peek(Ident) {
            !(lookahead.peek(kw::skip)
                || lookahead.peek(kw::then)
                || lookahead.peek(kw::val)
                || lookahead.peek(kw::end))
                || lookahead.peek(kw::rec)
        } else {
            false
        }
    }

    #[derive(Debug)]
    pub(crate) enum Expression {
        Assign(Assign),
        Seq(Assign, Box<Expression>),
        If(Box<Expression>, Box<Expression>, Box<Expression>),
        While(Box<Expression>, Box<Expression>),
        Function(Ident, Box<Expression>, Type),
    }

    #[derive(Debug)]
    pub(crate) enum Assign {
        Term(Term),
        Application(Ident, Box<Assign>),
        GE(Term, Term),
    }

    #[derive(Debug)]
    pub(crate) enum Term {
        App(App),
        Add(Box<Term>, Box<Term>),
    }

    #[derive(Debug)]
    pub(crate) enum App {
        Expr(Box<Expression>),
        Int(LitInt),
        Bool(LitBool),
        Var(Ident),
        Deref(Ident),
        Skip,
        Application(Box<App>, Box<App>),
        Let(Ident, Type, Box<Expression>, Box<Expression>),
        LetRec(Ident, Type, (Ident, Box<Expression>), Box<Expression>),
    }

    impl Parse for Expression {
        fn parse<'a>(input: &'a ParseBuffer<'a>) -> Result<Self> {
            let lookahead = input.lookahead1();

            Ok(if lookahead.peek(Token![if]) {
                input.parse::<Token![if]>()?;
                let e1 = input.parse::<Expression>()?;
                input.parse::<kw::then>()?;
                let e2 = input.parse::<Expression>()?;
                input.parse::<Token![else]>()?;
                let e3 = input.parse::<Expression>()?;
                Expression::If(Box::from(e1), Box::from(e2), Box::from(e3))
            } else if lookahead.peek(Token![while]) {
                input.parse::<Token![while]>()?;
                let e1 = input.parse::<Expression>()?;
                input.parse::<Token![do]>()?;
                let e2 = input.parse::<Expression>()?;

                Expression::While(Box::from(e1), Box::from(e2))
            } else if lookahead.peek(Token![fn]) {
                input.parse::<Token![fn]>()?;
                let ident = input.parse::<Ident>()?;
                input.parse::<Token![:]>()?;
                let in_type = parse_type(input)?;
                input.parse::<Token![=>]>()?;
                let e = input.parse::<Expression>()?;
                Expression::Function(ident, Box::from(e), in_type)
            } else {
                let first_term = input.parse::<Assign>()?;
                let lookahead = input.lookahead1();

                if lookahead.peek(Token![;]) {
                    input.parse::<Token![;]>()?;
                    Expression::Seq(first_term, Box::from(input.parse::<Expression>()?))
                } else {
                    Expression::Assign(first_term)
                }
            })
        }
    }

    impl Parse for Assign {
        fn parse<'a>(input: &'a ParseBuffer<'a>) -> Result<Self> {
            let lookahead = input.lookahead1();
            Ok(if is_ident(&lookahead) {
                if input.peek2(Token![:]) {
                    let ident = input.parse()?;

                    input.parse::<Token![:]>()?;
                    input.parse::<Token![=]>()?;

                    Assign::Application(ident, Box::from(input.parse::<Assign>()?))
                } else {
                    Assign::Term(input.parse::<Term>()?)
                }
            } else {
                let t1 = input.parse()?;
                let lookahead = input.lookahead1();
                if lookahead.peek(Token![>=]) {
                    input.parse::<Token![>=]>()?;
                    let t2 = input.parse()?;
                    Assign::GE(t1, t2)
                } else {
                    Assign::Term(t1)
                }
            })
        }
    }

    impl Parse for Term {
        fn parse<'a>(input: &'a ParseBuffer<'a>) -> Result<Self> {
            let app = Term::App(input.parse::<App>()?);

            // ok now we have app, let's check if anything is following it:
            let lookahead = input.lookahead1();
            let full_term = if lookahead.peek(Token![+]) {
                input.parse::<Token![+]>()?;
                Term::Add(Box::from(app), Box::from(input.parse::<Term>()?))
            } else {
                app
            };

            Ok(full_term)
        }
    }

    impl Parse for App {
        fn parse<'a>(input: &'a ParseBuffer<'a>) -> Result<Self> {
            let mut lookahead = input.lookahead1();

            let mut apps = vec![];

            loop {
                let e1 = if lookahead.peek(token::Paren) {
                    let content;
                    parenthesized!(content in input);
                    App::Expr(Box::from(content.parse::<Expression>()?))
                } else if lookahead.peek(LitInt) {
                    input.parse().map(App::Int)?
                } else if lookahead.peek(LitBool) {
                    input.parse().map(App::Bool)?
                } else if lookahead.peek(kw::skip) {
                    input.parse::<kw::skip>()?;
                    App::Skip
                } else if lookahead.peek(Token![!]) {
                    input.parse::<Token![!]>()?;
                    input.parse().map(App::Deref)?
                } else if lookahead.peek(Token![let]) {
                    input.parse::<Token![let]>()?;
                    input.parse::<kw::val>()?;

                    lookahead = input.lookahead1();
                    if lookahead.peek(kw::rec) {
                        // recursive function
                        input.parse::<kw::rec>()?;
                        let x: Ident = input.parse()?;
                        input.parse::<Token![:]>()?;
                        let t: Type = parse_type(input)?;
                        input.parse::<Token![=]>()?;

                        // read parens
                        let content;
                        parenthesized!(content in input);
                        let function_e = content.parse::<Expression>()?;
                        if let Expression::Function(y, e1, _) = function_e {
                            input.parse::<Token![in]>()?;
                            let e2: Expression = input.parse()?;
                            input.parse::<kw::end>()?;

                            App::LetRec(x, t, (y, e1), Box::from(e2))
                        } else {
                            // todo helpful error message
                            panic!("Expected `fn` in let rec")
                        }
                    } else {
                        // regular let binding
                        let x: Ident = input.parse()?;
                        input.parse::<Token![:]>()?;
                        let t: Type = parse_type(input)?;
                        input.parse::<Token![=]>()?;
                        let e1: Expression = input.parse()?;
                        input.parse::<Token![in]>()?;
                        let e2: Expression = input.parse()?;
                        input.parse::<kw::end>()?;
                        App::Let(x, t, Box::from(e1), Box::from(e2))
                    }
                } else if is_ident(&lookahead) {
                    input.parse().map(App::Var)?
                } else {
                    break;
                };
                apps.push(e1);
                lookahead = input.lookahead1();
            }

            // interestingly this code is the dual of the function type parsing code

            if apps.is_empty() {
                return Err(lookahead.error());
            }

            apps.reverse();

            let mut rv: App = apps.pop().unwrap();

            while let Some(t) = apps.pop() {
                rv = App::Application(Box::from(rv), Box::from(t));
            }

            return Ok(rv);
        }
    }

    pub(crate) trait Reduce {
        fn reduce(self) -> crate::Expression;
    }

    impl Reduce for Expression {
        fn reduce(self) -> crate::Expression {
            match self {
                Expression::Assign(a) => a.reduce(),
                Expression::Seq(e1, e2) => {
                    crate::Expression::Seq(Box::from(e1.reduce()), Box::from(e2.reduce()))
                }
                Expression::If(e1, e2, e3) => crate::Expression::If(
                    Box::from(e1.reduce()),
                    Box::from(e2.reduce()),
                    Box::from(e3.reduce()),
                ),
                Expression::While(e1, e2) => {
                    crate::Expression::While(Box::from(e1.reduce()), Box::from(e2.reduce()))
                }
                Expression::Function(x, e, t) => {
                    crate::Expression::Function(x, Box::from(e.reduce()), Some(t), None)
                }
            }
        }
    }

    impl Reduce for Assign {
        fn reduce(self) -> crate::Expression {
            match self {
                Assign::Term(t) => t.reduce(),
                Assign::Application(l, a) => crate::Expression::Assign(l, Box::from(a.reduce())),
                Assign::GE(t1, t2) => {
                    crate::Expression::GE(Box::from(t1.reduce()), Box::from(t2.reduce()))
                }
            }
        }
    }

    impl Reduce for Term {
        fn reduce(self) -> crate::Expression {
            match self {
                Term::Add(t1, t2) => {
                    crate::Expression::Add(Box::from(t1.reduce()), Box::from(t2.reduce()))
                }
                Term::App(app) => app.reduce(),
            }
        }
    }

    impl Reduce for App {
        fn reduce(self) -> crate::Expression {
            match self {
                App::Expr(e) => e.reduce(),
                App::Int(i) => crate::Expression::Int(i),
                App::Var(v) => crate::Expression::Variable(v),
                App::Application(a1, a2) => {
                    crate::Expression::Application(Box::from(a1.reduce()), Box::from(a2.reduce()))
                }
                App::Deref(l) => crate::Expression::Deref(l),
                App::Skip => crate::Expression::Skip,
                App::Bool(b) => crate::Expression::Bool(b),
                App::Let(x, t, e1, e2) => {
                    crate::Expression::Let(x, t, Box::from(e1.reduce()), Box::from(e2.reduce()))
                }
                App::LetRec(x, t, (y, e1), e2) => crate::Expression::LetRec(
                    x,
                    t,
                    y,
                    Box::from(e1.reduce()),
                    Box::from(e2.reduce()),
                ),
            }
        }
    }
}

#[allow(non_snake_case)]
#[proc_macro]
pub fn L2_interpreter(tokens: TokenStream) -> TokenStream {
    // this macro is the opposite of hygienic -- it actively requires that a load of types
    // are in scope. Not ideal, but hey, it's for internal use only.

    // parse input into one big expression tree
    let input = parse_macro_input!(tokens as rewrite::Expression);

    // convert that expression tree into rust code
    let expanded = to_interpreted_token_stream(input.reduce());
    expanded.into()
}

#[allow(non_snake_case)]
#[proc_macro]
pub fn L2(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as rewrite::Expression);

    let mut variables = HashMap::new();
    variables.insert("arg".into(), Type::Int);

    to_token_stream(input.reduce(), variables).into()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        super::new_l2! {
            1 + 2
        }
    }
}

use std::collections::{HashMap, HashSet};

use proc_macro2::Span;
use quote::quote;
use std::cell::RefCell;
use std::rc::Rc;
use syn::{Ident, LitBool, LitInt};

pub type FreeVar = Rc<RefCell<Option<Type>>>;

use crate::typing::infer_type;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Function(Box<Type>, Box<Type>),
    FreeVar(FreeVar),
}

#[derive(Debug)]
pub enum Expression {
    Int(LitInt),
    Bool(LitBool),
    Add(Box<Expression>, Box<Expression>),
    GE(Box<Expression>, Box<Expression>),
    Seq(Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>, Span),
    Deref(Ident, Span),
    Assign(Ident, Box<Expression>),
    Skip(Span),
    While(Box<Expression>, Box<Expression>, Span),
    Function(Ident, Box<Expression>, Option<Type>, Option<Type>, Span),
    Application(Box<Expression>, Box<Expression>),
    Variable(Ident),
    Let(Ident, Type, Box<Expression>, Box<Expression>, Span),
    LetRec(Ident, Type, Ident, Box<Expression>, Box<Expression>, Span),
}

impl Expression {
    pub fn span(&self) -> Span {
        use syn::spanned::Spanned;
        match self {
            Expression::Int(i) => i.span(),
            Expression::Bool(b) => b.span(),
            Expression::Variable(v) => v.span(),
            Expression::Assign(l, e) => l.span().join(e.span()).unwrap(),

            Expression::Add(e1, e2)
            | Expression::GE(e1, e2)
            | Expression::Seq(e1, e2)
            | Expression::Application(e1, e2) => e1.span().join(e2.span()).unwrap(),

            Expression::If(_, _, _, span)
            | Expression::Deref(_, span)
            | Expression::Skip(span)
            | Expression::While(_, _, span)
            | Expression::Function(_, _, _, _, span)
            | Expression::Let(_, _, _, _, span)
            | Expression::LetRec(_, _, _, _, _, span) => *span,
        }
    }
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
        Type::FreeVar(_) => {
            panic!("Could not deduce type of free variable! Can't say which though sorry")
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
            Expression::Int(_) | Expression::Bool(_) | Expression::Deref(_, _) | Expression::Skip(_)
            // don't recurse into functions, they will call this function separately
            | Expression::Function(_, _, _, _, _)
            => {}

            Expression::Assign(_, ref mut e1) => {
                get_non_copy_free_variables(e1, variables, free_vars)
            }
            Expression::Add(ref mut e1, ref mut e2)
            | Expression::GE(ref mut e1, ref mut e2)
            | Expression::Seq(ref mut e1, ref mut e2)
            | Expression::While(ref mut e1, ref mut e2, _)
            | Expression::Application(ref mut e1, ref mut e2) => {
                get_non_copy_free_variables(e1, variables, free_vars);
                get_non_copy_free_variables(e2, variables, free_vars)
            }
            Expression::If(ref mut e1, ref mut e2, ref mut e3, _) => {
                get_non_copy_free_variables(e1, variables, free_vars);
                get_non_copy_free_variables(e2, variables, free_vars);
                get_non_copy_free_variables(e3, variables, free_vars)
            }

            Expression::Variable(_) => unreachable!("we already checked for this case"),
            Expression::Let(v, t, e1, e2, _) => {
                get_non_copy_free_variables(e1, variables, free_vars);
                let old_value = variables.insert(v.to_string(), t.clone());
                get_non_copy_free_variables(e2, variables, free_vars);
                // put variables back how it was
                match old_value {
                    None => variables.remove(&*v.to_string()),
                    Some(old_value) => variables.insert(v.to_string(), old_value),
                };
            }
            Expression::LetRec(v, t, y, _e1, e2, _) => {
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

// pub fn get_free_vars() -> FreeVar {
//
// }

/// this function does a lot
/// too much, you could say
/// it gets all the free variables (but not the bound ones) and puts them in a set
/// it also tells you if a given variable appears anywhere other than the LHS of an application
/// these two things should really be separated
pub fn get_all_free_variables(
    e: &Expression,
    variables: &HashMap<String, Type>,
    ignore_set: &HashSet<String>,
    free_vars: &mut HashSet<String>,
    check_defined: bool,
) {
    match &*e {
        Expression::Variable(v) => {
            if ignore_set.contains(&*v.to_string()) {
                return;
            }

            if check_defined && !variables.contains_key(&*v.to_string()) {
                // todo helpful error message
                panic!(format!(
                    "variable {} used without definition! (at least, we don't know the type of it)",
                    v.to_string()
                ))
            }

            free_vars.insert(v.to_string());
        }

        Expression::Int(_)
        | Expression::Bool(_)
        | Expression::Skip(_)
        | Expression::Deref(_, _) => {}

        Expression::Assign(_, e1) => {
            get_all_free_variables(e1, variables, ignore_set, free_vars, check_defined);
        }
        Expression::Add(e1, e2)
        | Expression::GE(e1, e2)
        | Expression::Seq(e1, e2)
        | Expression::While(e1, e2, _)
        | Expression::Application(e1, e2) => {
            get_all_free_variables(e1, variables, ignore_set, free_vars, check_defined);
            get_all_free_variables(e2, variables, ignore_set, free_vars, check_defined);
        }
        Expression::If(e1, e2, e3, _) => {
            get_all_free_variables(e1, variables, ignore_set, free_vars, check_defined);
            get_all_free_variables(e2, variables, ignore_set, free_vars, check_defined);
            get_all_free_variables(e3, variables, ignore_set, free_vars, check_defined);
        }

        Expression::Function(v, e1, _, _, _) => {
            let mut ignore_set = ignore_set.clone();
            ignore_set.insert(v.to_string());
            get_all_free_variables(e1, variables, &ignore_set, free_vars, check_defined);
        }
        Expression::Let(v, _, e1, e2, _) => {
            let mut ignore_set_2 = ignore_set.clone();
            ignore_set_2.insert(v.to_string());
            get_all_free_variables(e1, variables, &ignore_set_2, free_vars, check_defined);
            get_all_free_variables(e2, variables, &ignore_set_2, free_vars, check_defined);
        }
        Expression::LetRec(x, _, y, e1, e2, _) => {
            let mut ignore_set_1 = ignore_set.clone();
            ignore_set_1.insert(x.to_string());
            get_all_free_variables(e2, variables, &ignore_set_1, free_vars, check_defined);
            ignore_set_1.insert(y.to_string());
            get_all_free_variables(e1, variables, &ignore_set_1, free_vars, check_defined);
        }
    }
}

fn appears_only_on_lhs_of_apply(e: &Expression, target: &str, is_on_lhs_of_apply: bool) -> bool {
    match &*e {
        Expression::Application(e1, e2) => {
            appears_only_on_lhs_of_apply(e1, target, true)
                && appears_only_on_lhs_of_apply(e2, target, false)
        }
        Expression::Variable(v) => is_on_lhs_of_apply || v != target,

        Expression::Int(_)
        | Expression::Bool(_)
        | Expression::Skip(_)
        | Expression::Deref(_, _) => true,

        Expression::Assign(_, e1) | Expression::Function(_, e1, _, _, _) => {
            appears_only_on_lhs_of_apply(e1, target, false)
        }

        Expression::Add(e1, e2)
        | Expression::GE(e1, e2)
        | Expression::Seq(e1, e2)
        | Expression::While(e1, e2, _)
        | Expression::Let(_, _, e1, e2, _)
        | Expression::LetRec(_, _, _, e1, e2, _) => {
            appears_only_on_lhs_of_apply(e1, target, false)
                && appears_only_on_lhs_of_apply(e2, target, false)
        }

        Expression::If(e1, e2, e3, _) => {
            appears_only_on_lhs_of_apply(e1, target, false)
                && appears_only_on_lhs_of_apply(e2, target, false)
                && appears_only_on_lhs_of_apply(e3, target, false)
        }
    }
}

/// converts an expression (in our internal representation) back into a token stream
pub fn to_token_stream(
    e: Expression,
    mut variables: HashMap<String, Type>,
) -> proc_macro2::TokenStream {
    match e {
        Expression::Int(lit) => quote! {#lit},
        Expression::Bool(lit) => quote! {#lit},
        Expression::Add(e1, e2) => {
            let (ts1, ts2) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables),
            );
            quote! {mod_l3::l3_compiler::Add {e1: #ts1, e2: #ts2}}
        }
        Expression::GE(e1, e2) => {
            let (ts1, ts2) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables),
            );
            quote! {mod_l3::l3_compiler::GE {e1: #ts1, e2: #ts2}}
        }
        Expression::Seq(e1, e2) => {
            let (ts1, ts2) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables),
            );
            quote! {mod_l3::l3_compiler::Seq {e1: #ts1, e2: #ts2}}
        }
        Expression::If(e1, e2, e3, _) => {
            let (ts1, ts2, ts3) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables.clone()),
                to_token_stream(*e3, variables),
            );
            quote! {mod_l3::l3_compiler::If {e1: #ts1, e2: #ts2, e3: #ts3}}
        }
        Expression::Deref(l, _) => {
            quote! {mod_l3::l3_compiler::Deref {l: #l.clone()}}
        }
        Expression::Assign(l, e) => {
            let ts = to_token_stream(*e, variables);
            quote! {mod_l3::l3_compiler::Assign {l: #l.clone(), e1: #ts}}
        }
        Expression::Skip(_) => quote! {mod_l3::l3_compiler::Skip {}},
        Expression::While(e1, e2, _) => {
            let (ts1, ts2) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables),
            );
            quote! {mod_l3::l3_compiler::While {e1: #ts1, e2: #ts2}}
        }
        Expression::Function(l, mut e, Some(t_in), Some(t_out), _) => {
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
        Expression::Function(l, e, Some(t_in), None, span) => {
            variables.insert(l.to_string(), t_in.clone());

            let mut free_vars = HashMap::new();
            let t_out = infer_type(&e, variables.clone(), None, &mut free_vars).unwrap();
            assert!(free_vars.is_empty());

            to_token_stream(
                Expression::Function(l, e, Some(t_in), Some(t_out), span),
                variables,
            )
        }
        Expression::Function(_, _, _, _, _) => {
            panic!("Could not infer type of function!")
        }
        Expression::Application(e1, e2) => {
            let (ts1, ts2) = (
                to_token_stream(*e1, variables.clone()),
                to_token_stream(*e2, variables),
            );
            quote! {mod_l3::l3_compiler::Apply {e1: #ts1, e2: #ts2, phantom: Default::default()}}
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
                // this is actually kinda hard since syn doesn't have warnings
                eprintln!(
                    "Warning: could not infer type for variable: {:?}",
                    v.to_string()
                );
                // Error::new(v.span(), "could not infer type for variable").to_compile_error()
                quote! {#v}
            }
        }
        Expression::Let(v, t, e1, e2, _) => {
            let ts1 = to_token_stream(*e1, variables.clone());
            let type_token = type_to_token(t.clone());
            variables.insert(v.to_string(), t);
            let ts2 = to_token_stream(*e2, variables);

            quote! {
                {let #v: #type_token = (#ts1).step(); (#ts2).step()}
            }
        }

        Expression::LetRec(x, t, y, e1, e2, _) => {
            let_rec_to_token_stream(variables, &x, t, &y, *e1, *e2)
        }
    }
}

fn let_rec_to_token_stream(
    mut variables: HashMap<String, Type>,
    x: &Ident,
    t: Type,
    y: &Ident,
    e1: Expression,
    e2: Expression,
) -> proc_macro2::TokenStream {
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

    // add x to the list of variables (so the checker knows it's valid)
    variables.insert(x.to_string(), t.clone());
    // before we add y as well, generate the token stream for e2 (since y isn't in that scope)
    let e2_ts = to_token_stream(e2, variables.clone());

    let mut free_vars = HashSet::new();
    let mut ignore_set = HashSet::new();
    ignore_set.insert(x.to_string());

    // add y to the list of variables -- we need to coerce it out of the enum first
    // the two types are extracted for later use
    let (t1, t2) = if let Type::Function(t1, t2) = t {
        variables.insert(y.to_string(), *t2.clone());
        ignore_set.insert(y.to_string());
        (t1, t2)
    } else {
        unreachable!("i _know_ it's a function, we already checked")
    };

    get_all_free_variables(&e1, &variables, &ignore_set, &mut free_vars, true);

    // this is used later but needs to be calculated before e1 is consumed
    let appears_only_on_lhs_of_apply = appears_only_on_lhs_of_apply(&e1, &x.to_string(), false);

    // get a list of the types of the variables, in the same order
    // this is used in the quote iterator later, where it's zipped with free_vars
    let free_var_types: Vec<_> = free_vars
        .iter()
        .map(|v| {
            type_to_token(
                variables
                    .get(v)
                    .expect("We already checked they were in here")
                    .clone(),
            )
        })
        .collect();

    // step 2: get all non-copy variables
    let clone_vars: Vec<Ident> = free_vars
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

    // step 3: convert to token stream
    let (t1_ts, t2_ts) = (type_to_token(*t1), type_to_token(*t2.clone()));
    let e1_ts = to_token_stream(e1, variables);

    // change String into Ident (quote doesn't accept Vec<String>)
    let free_vars: Vec<Ident> = free_vars
        .into_iter()
        .map(|v| Ident::new(&*v, Span::call_site()))
        .collect();

    // if the inner closure only appears in the LHS of an Apply
    //     (this is sadly a limitation of rust's type system, in theory it could be more general)
    // AND the recursive function returns a primitive
    // AND the closures doesn't capture any scope
    // then we can change that inner closure from an Rc<Fn()> to a fn()
    // this allows for better optimisation, for whatever reason.
    let inner_closure = if appears_only_on_lhs_of_apply
        && !matches!(*t2, Type::Function(_, _))
        && clone_vars.is_empty()
    {
        quote! {
            (move |#y: #t1_ts| {#x(#(#free_vars.clone() ,)* #y)}) as fn(#t1_ts) -> #t2_ts
        }
    } else {
        let clone_vars = clone_vars.iter();
        quote! {
            #(let #clone_vars = #clone_vars.clone();)*
            std::rc::Rc::from(move |#y: #t1_ts| {#x(#(#free_vars.clone() ,)* #y)}) as std::rc::Rc<dyn Fn(#t1_ts) -> #t2_ts>
        }
    };

    let clone_vars = clone_vars.iter();
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

/// converts an expression (in our internal representation) back into a token stream
/// for the interpreter module
pub fn to_interpreted_token_stream(e: Expression) -> proc_macro2::TokenStream {
    match e {
        Expression::Int(e) => {
            quote! {mod_l3::l3_interpreter::Expression::VALUE(mod_l3::l3_interpreter::Value::Int(#e))}
        }
        Expression::Bool(b) => {
            quote! {mod_l3::l3_interpreter::Expression::VALUE(mod_l3::l3_interpreter::Value::Bool(#b))}
        }
        Expression::Add(e1, e2) => {
            let (ts1, ts2) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
            );
            quote! {mod_l3::l3_interpreter::Expression::OP(Box::from(#ts1), mod_l3::l3_interpreter::Operation::ADD, Box::from(#ts2))}
        }
        Expression::GE(e1, e2) => {
            let (ts1, ts2) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
            );
            quote! {mod_l3::l3_interpreter::Expression::OP(Box::from(#ts1), mod_l3::l3_interpreter::Operation::GTE, Box::from(#ts2))}
        }
        Expression::Seq(e1, e2) => {
            let (ts1, ts2) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
            );
            quote! {mod_l3::l3_interpreter::Expression::SEQ(Box::from(#ts1), Box::from(#ts2))}
        }
        Expression::If(e1, e2, e3, _) => {
            let (ts1, ts2, ts3) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
                to_interpreted_token_stream(*e3),
            );
            quote! {mod_l3::l3_interpreter::Expression::IF {
                    cond: Box::from(#ts1),
                    then: Box::from(#ts2),
                    else_: Box::from(#ts3)
                }
            }
        }
        Expression::Deref(l, _) => {
            quote! {mod_l3::l3_interpreter::Expression::DEREF(#l)}
        }
        Expression::Assign(l, e) => {
            let ts = to_interpreted_token_stream(*e);
            quote! {mod_l3::l3_interpreter::Expression::ASSIGN(#l, Box::from(#ts))}
        }
        Expression::Skip(_) => quote! {mod_l3::l3_interpreter::Expression::SKIP {}},
        Expression::While(e1, e2, _) => {
            let (ts1, ts2) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
            );
            quote! {mod_l3::l3_interpreter::Expression::WHILE {
                    cond: Box::from(#ts1),
                    do_: Box::from(#ts2),
                }
            }
        }
        Expression::Function(l, e, _, _, _) => {
            let ts = to_interpreted_token_stream(*e);
            quote! {mod_l3::l3_interpreter::Expression::FUNCTION(#l, Box::from(#ts))}
        }
        Expression::Application(e1, e2) => {
            let (ts1, ts2) = (
                to_interpreted_token_stream(*e1),
                to_interpreted_token_stream(*e2),
            );
            quote! {mod_l3::l3_interpreter::Expression::APPLICATION(Box::from(#ts1), Box::from(#ts2))}
        }
        Expression::Variable(v) => {
            quote! {mod_l3::l3_interpreter::Expression::VARIABLE(#v)}
        }
        Expression::Let(_, _, _, _, _) | Expression::LetRec(_, _, _, _, _, _) => {
            unimplemented!("no interpreter for let bindings yet")
        }
    }
}

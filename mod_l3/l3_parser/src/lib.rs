#![warn(clippy::all, clippy::pedantic, clippy::correctness)]

extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use std::collections::{HashMap, HashSet};

use syn::parse_macro_input;

use crate::generator::Type;
use crate::parser::Reduce;
use std::cell::RefCell;
use std::rc::Rc;

mod generator;
mod parser;
mod typing;

#[allow(non_snake_case)]
#[proc_macro]
pub fn L3_interpreter(tokens: TokenStream) -> TokenStream {
    // this macro is the opposite of hygienic -- it actively requires that a load of types
    // are in scope. Not ideal, but hey, it's for internal use only.

    // parse input into one big expression tree
    let input = parse_macro_input!(tokens as parser::Expression);

    // convert that expression tree into rust code
    generator::to_interpreted_token_stream(input.reduce()).into()
}

#[allow(non_snake_case)]
#[proc_macro]
pub fn L3(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as parser::Expression).reduce();

    // variables.insert("arg".into(), Type::Int);

    // infer the types of all free variables (where possible)
    let variables = HashMap::new();
    let mut free_vars = HashSet::new();
    generator::get_all_free_variables(&input, &variables, &HashSet::new(), &mut free_vars, false);

    let mut free_vars: HashMap<String, _> = free_vars
        .into_iter()
        .map(|v| (v, Rc::new(RefCell::new(None))))
        .collect();

    match typing::infer_type(&input, variables, None, &mut free_vars) {
        Ok(_) => {}
        Err(ts) => return ts.into(),
    }

    let variables: HashMap<String, Type> = free_vars
        .into_iter()
        .map(|(k, v)| {
            if let Some(t) = (*v).clone().into_inner() {
                (k, t)
            } else {
                panic!("could not infer type of free variable fff {:?}", k)
            }
        })
        .collect();

    generator::to_token_stream(input, variables).into()
}

#![warn(clippy::all, clippy::pedantic, clippy::correctness)]

extern crate proc_macro;
extern crate proc_macro2;

use proc_macro::TokenStream;
use std::collections::HashMap;

use syn::parse_macro_input;

use crate::generator::Type;
use crate::parser::Reduce;

mod generator;
mod parser;

#[allow(non_snake_case)]
#[proc_macro]
pub fn L2_interpreter(tokens: TokenStream) -> TokenStream {
    // this macro is the opposite of hygienic -- it actively requires that a load of types
    // are in scope. Not ideal, but hey, it's for internal use only.

    // parse input into one big expression tree
    let input = parse_macro_input!(tokens as parser::Expression);

    // convert that expression tree into rust code
    generator::to_interpreted_token_stream(input.reduce()).into()
}

#[allow(non_snake_case)]
#[proc_macro]
pub fn L2(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as parser::Expression);

    let mut variables = HashMap::new();
    variables.insert("arg".into(), Type::Int);

    generator::to_token_stream(input.reduce(), variables).into()
}

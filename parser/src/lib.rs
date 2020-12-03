extern crate proc_macro;

use proc_macro::{TokenStream, Literal, TokenTree};
use syn::{braced, parse_macro_input, token, Field, Ident, Result, Token, ItemEnum, LitInt, LitBool, Expr, ExprParen, parenthesized, Error};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use std::process::id;
use syn::token::Token;
use quote::quote;


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
    // Group(Box<Expression>),   // this one is just here for precedence reasons
}

struct TaggedExpression(i32, Expression);


fn handle_op(e1: Expression, input: ParseStream) -> Result<Expression> {
    let lookahead = input.lookahead1();
    if lookahead.peek(Token![+]) {
        println!("it was a +");
        let _: Token![+] = input.parse()?;
        let e2: Expression = input.parse()?;
        Ok(Expression::Add(Box::from(e1), Box::from(e2)))
    } else if lookahead.peek(Token![;]) {
        let _: Token![;] = input.parse()?;
        let e2: Expression = input.parse()?;
        Ok(Expression::Seq(Box::from(e1), Box::from(e2)))
    } else if lookahead.peek(Token![>=]) {
        let _: Token![>=] = input.parse()?;
        let e2: Expression = input.parse()?;
        Ok(Expression::GE(Box::from(e1), Box::from(e2)))
    } else {
        Ok(e1)
    }
}

// impl Expression {
//     fn parse2(input: ParseStream, lhs: Expression, min_precedence: u32) -> Option<Expression> {
//
//     }
// }

impl Parse for Expression {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();

        let e1: Expression = if lookahead.peek(token::Paren) {
            // conjecture: anything inside parens is an expression
            let content;
            let _: token::Paren = parenthesized!(content in input);
            content.parse()?
        } else if lookahead.peek(LitInt) {
            let int: LitInt = input.parse()?;
            println!("got an int");
            Expression::Int(int)
        } else if lookahead.peek(LitBool) {
            let b: LitBool = input.parse()?;
            println!("got an int");
            Expression::Bool(b)
        } else if lookahead.peek(Token![if]) {
            // oh boy this is gonna be fun to parse
            let _: Token![if] = input.parse()?;
            let e1: Expression = input.parse()?;
            let then: Ident = input.parse()?;
            if then.to_string() != "then" { return Err(Error::new(then.span(), "Expected 'then'")) };
            let e2: Expression = input.parse()?;
            let else_: Token![else] = input.parse()?;
            let e3: Expression = input.parse()?;
            Expression::If(Box::from(e1), Box::from(e2), Box::from(e3))
        } else if lookahead.peek(Token![!]) {
            let _ : Token![!] = input.parse()?;
            let loc: Ident = input.parse()?;
            Expression::Deref(loc)
        } else if lookahead.peek(Token![while]) {
            let _ : Token![while] = input.parse()?;
            let e1: Expression = input.parse()?;
            let _: Token![do] = input.parse()?;
            let e2: Expression = input.parse()?;
            Expression::While(Box::from(e1), Box::from(e2))
        } else if lookahead.peek(Ident) {
            // this case is a bit more complex since it could be several things
            // strategy: if it's a keyword, parse the relevant tree.
            //           otherwise, it's probably a location
            let ident: Ident = input.parse()?;
            match ident.to_string().as_str() {
                "skip" => Expression::Skip,
                _ => {
                    // it must be an assign (for now at least)
                    let _: (Token![:], Token![=]) = (input.parse()?, input.parse()?);
                    let e: Expression = input.parse()?;
                    Expression::Assign(ident, Box::from(e))
                }
            }
        } else {
            return Err(lookahead.error())
        };

        // but what if e1 is just the first part of an op or a seq?
        Ok(handle_op(e1, input)?)
    }
}

fn to_token_stream(e: Expression) -> quote::__private::TokenStream {
    match e {
        Expression::Int(e) => quote!{#e},
        Expression::Bool(b) => quote!{#b},
        Expression::Add(e1, e2) => {
            let (ts1, ts2) = (to_token_stream(*e1), to_token_stream(*e2));
            quote! {Add {e1: #ts1, e2: #ts2}}
        }
        Expression::GE(e1, e2) => {
            let (ts1, ts2) = (to_token_stream(*e1), to_token_stream(*e2));
            quote! {GE {e1: #ts1, e2: #ts2}}
        }
        Expression::Seq(e1, e2) => {
            let (ts1, ts2) = (to_token_stream(*e1), to_token_stream(*e2));
            quote! {Seq {e1: #ts1, e2: #ts2}}
        }
        Expression::If(e1, e2, e3) => {
            let (ts1, ts2, ts3) =
                (to_token_stream(*e1), to_token_stream(*e2), to_token_stream(*e3));
            quote! {If {e1: #ts1, e2: #ts2, e3: #ts3}}
        }
        Expression::Deref(l) => {
            quote! {Deref {l: #l}}
        },
        Expression::Assign(l, e) => {
            let ts = to_token_stream(*e);
            quote! {Assign {l: #l, e1: #ts}}
        }
        Expression::Skip => quote! {Skip {}},
        Expression::While(e1, e2) => {
            let (ts1, ts2) = (to_token_stream(*e1), to_token_stream(*e2));
            quote! {While {e1: #ts1, e2: #ts2}}
        }
    }
}

#[allow(non_snake_case)]
#[proc_macro]
pub fn L1(tokens_: TokenStream) -> TokenStream {
    println!("{:#?}", tokens_);
    let input = parse_macro_input!(tokens_ as Expression);
    println!("{:#?}", input);

    // ok now we have to convert it into a tokenstream again
    let expanded = to_token_stream(input);

    expanded.into()
}
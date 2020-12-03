extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{parenthesized, parse_macro_input, token, Error, Ident, LitBool, LitInt, Result, Token};

// this import doesn't actually do anything, but it silences a false positive error in CLion
#[allow(unused_imports)]
use syn::token::Token;

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

impl Expression {
    fn parse_primary(input: ParseStream) -> Result<Expression> {
        let lookahead = input.lookahead1();
        let e1: Expression = if lookahead.peek(token::Paren) {
            // conjecture: anything inside parens is an expression
            let content;
            parenthesized!(content in input);
            content.parse()?
        } else if lookahead.peek(LitInt) {
            Expression::Int(input.parse()?)
        } else if lookahead.peek(LitBool) {
            Expression::Bool(input.parse()?)
        } else if lookahead.peek(Token![if]) {
            // although IF ends in an expression, it's allowed to be greedy
            // this isn't specified in the semantics, it's a parsing decision i'm allowed to make
            input.parse::<Token![if]>()?;
            let e1: Expression = input.parse()?;

            // todo syn custom keywords
            let then: Ident = input.parse()?;
            if then.to_string() != "then" {
                return Err(Error::new(then.span(), "Expected 'then'"));
            };

            let e2: Expression = input.parse()?;
            input.parse::<Token![else]>()?;
            let e3: Expression = input.parse()?;

            Expression::If(Box::from(e1), Box::from(e2), Box::from(e3))
        } else if lookahead.peek(Token![!]) {
            input.parse::<Token![!]>()?;
            let loc: Ident = input.parse()?;
            Expression::Deref(loc)
        } else if lookahead.peek(Token![while]) {
            // although WHILE ends in an expression, it's allowed to be greedy
            // this isn't specified in the semantics, it's a parsing decision i'm allowed to make
            input.parse::<Token![while]>()?;
            let e1: Expression = input.parse()?;
            input.parse::<Token![do]>()?;
            let e2: Expression = input.parse()?;
            Expression::While(Box::from(e1), Box::from(e2))
        } else if lookahead.peek(Ident) {
            // this case is a bit more complex since it could be several things
            // strategy: if it's a keyword, parse the relevant tree.
            //           otherwise, it's probably a location
            // luckily for me there's only one L1 keyword that isn't already a rust keyword
            // todo syn has some way of adding custom keywords?
            let ident: Ident = input.parse()?;
            match ident.to_string().as_str() {
                "skip" => Expression::Skip,
                _ => {
                    // it must be an assign (for now at least)
                    // consume the :=
                    input.parse::<Token![:]>()?;
                    input.parse::<Token![=]>()?;

                    // consume an expression up to the nearest ;
                    let e: Expression = Expression::partial_parse(input, None, 3)?;
                    Expression::Assign(ident, Box::from(e))
                }
            }
        } else {
            return Err(lookahead.error());
        };

        Ok(e1)
    }

    fn partial_parse(
        input: ParseStream,
        lhs: Option<Expression>,
        min_precedence: u32,
    ) -> Result<Expression> {
        let mut lhs = match lhs {
            None => Expression::parse_primary(input)?,
            Some(e) => e,
        };

        let mut lookahead = input.lookahead1();
        loop {
            let (op, op_prec): (&str, u32) = if lookahead.peek(Token![+]) && 4 >= min_precedence {
                input.parse::<Token![+]>()?;
                ("+", 4)
            } else if lookahead.peek(Token![;]) && 2 >= min_precedence {
                input.parse::<Token![;]>()?;
                (";", 2)
            } else if lookahead.peek(Token![>=]) && 2 >= min_precedence {
                input.parse::<Token![>=]>()?;
                (">=", 3)
            } else {
                break;
            };

            let mut rhs = Expression::parse_primary(input)?;
            lookahead = input.lookahead1();
            loop {
                rhs = if lookahead.peek(Token![+]) && 4 >= op_prec {
                    Expression::partial_parse(input, Some(rhs), 4)?
                } else if lookahead.peek(Token![;]) && 2 >= op_prec {
                    Expression::partial_parse(input, Some(rhs), 2)?
                } else if lookahead.peek(Token![>=]) && 3 >= op_prec {
                    Expression::partial_parse(input, Some(rhs), 3)?
                } else {
                    break;
                };
                lookahead = input.lookahead1();
            }

            // apply op to (lhs, rhs)
            lhs = match op {
                "+" => Expression::Add(Box::from(lhs), Box::from(rhs)),
                ";" => Expression::Seq(Box::from(lhs), Box::from(rhs)),
                ">=" => Expression::GE(Box::from(lhs), Box::from(rhs)),
                _ => unreachable!(),
            }
        }

        Ok(lhs)
    }
}

impl Parse for Expression {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Expression::partial_parse(
            input,
            Some(Expression::parse_primary(input)?),
            0,
        )?)
    }
}

fn to_token_stream(e: Expression) -> quote::__private::TokenStream {
    match e {
        Expression::Int(e) => quote! {#e},
        Expression::Bool(b) => quote! {#b},
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
            let (ts1, ts2, ts3) = (
                to_token_stream(*e1),
                to_token_stream(*e2),
                to_token_stream(*e3),
            );
            quote! {If {e1: #ts1, e2: #ts2, e3: #ts3}}
        }
        Expression::Deref(l) => {
            quote! {Deref {l: #l}}
        }
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
pub fn L1(tokens: TokenStream) -> TokenStream {
    // this macro is the opposite of hygienic -- it actively requires that a load of types
    // are in scope. Not ideal, but hey, it's for internal use only.

    // parse input into one big expression tree
    let input = parse_macro_input!(tokens as Expression);

    // convert that expression tree into rust code
    let expanded = to_token_stream(input);
    expanded.into()
}

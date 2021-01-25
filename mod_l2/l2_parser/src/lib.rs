extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream, ParseBuffer, Lookahead1};
use syn::{parenthesized, parse_macro_input, token, Error, Ident, LitBool, LitInt, Result, Token};

// this import doesn't actually do anything, but it silences a false positive error in CLion
#[allow(unused_imports)]
use syn::token::Token;
use syn::ext::IdentExt;

#[derive(Debug, Clone, Eq, PartialEq)]
enum Type {
    Int,
    Bool,
    Unit,
    Function(Box<Type>, Box<Type>)
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
    Variable(Ident)
}

fn parse_type(input: ParseStream) -> Result<Type> {
    let mut lookahead = input.lookahead1();
    let mut types: Vec<Type> = vec![];
    loop {
        if lookahead.peek(Ident::peek_any) {
            types.push(token_to_type(&*input.parse::<Ident>()?.to_string(), lookahead)?);
            lookahead = input.lookahead1();
            if lookahead.peek(Token![->]) {
                let _ = input.parse::<Token![->]>()?;
                lookahead = input.lookahead1();
                continue;
            }
        } else if lookahead.peek(token::Paren){
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
        println!("Error 2");
        return Err(lookahead.error())
    }

    // a->b->c->d
    // a->(b->(c->d))

    // a, b, c @ d
    // a, b, @ F(c,d)
    // a @ F(b, F(c, d))
    // F(a, F(b, F(c, d)))


    let mut rv: Type = types.pop().unwrap();

    while let Some(t) = types.pop() {
        rv = Type::Function(
            Box::from(t),
            Box::from(rv)
        );
        println!("BUILDING: {:?}", rv);
    }

    return Ok(rv);
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
            if then != "then" {
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
        } else if lookahead.peek(Token![fn]) {
            input.parse::<Token![fn]>()?;
            let var_name: Ident = input.parse()?;
            let _ = input.parse::<Token![:]>()?;

            // now we parse the type
            let type_: Type = parse_type(input)?;
            let _ = input.parse::<Token![=>]>()?;
            let e: Expression = input.parse()?;
            Expression::Function(var_name, Box::from(e), Some(type_), None)
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
                    // it's either a variable or a location
                    // if it starts with l, it's a location
                    // otherwise it's a variable

                    if ident.to_string().as_str().starts_with("l") {
                        // it must be an assign
                        // consume the :=
                        input.parse::<Token![:]>()?;
                        input.parse::<Token![=]>()?;

                        // consume an expression up to the nearest ;
                        let e: Expression = Expression::partial_parse(input, None, 3)?;
                        Expression::Assign(ident, Box::from(e))
                    } else {
                        Expression::Variable(ident)
                    }
                }
            }
        } else {
            println!("Error 3");
            return Err(lookahead.error());
        };

        Ok(e1)
    }

    fn partial_parse(
        input: ParseStream,
        lhs: Option<Expression>,
        min_precedence: u32,
    ) -> Result<Expression>
    {
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
        let e1 = Box::from(Self::partial_parse(
            input,
            Some(Self::parse_primary(input)?),
            0,
        )?);

        if let Ok(e) = Self::parse_primary(input) {
            return Ok(Expression::Application(
                e1, Box::from(Self::partial_parse(
                    input,
                    Some(e),
                    0,
                )?)
            ))
        } else {
            return Ok(*e1)
        }
    }

    // fn parse2(input: ParseStream) -> Result<Self> {
    //     // parse an expression. Then try and parse another. If we succeed, it's an application.
    //     // application is left associative, so a b c == (a b) c
    //     // and a b c d == ((a b) c) d
    //
    //     let mut expressions: Vec<Expression> = vec![];
    //
    //     while let Ok(e) = Self::partial_parse(
    //         input,
    //         Some(Self::parse_primary(input)?),
    //         0,
    //     ) {
    //         expressions.push(e)
    //     };
    //
    //     match expressions.len() {
    //         0 => panic!("Expected expression"),
    //         1 => Ok(expressions.pop().unwrap()),
    //         2 => {
    //             expressions.reverse();
    //             Ok(Application(Box::from(expressions.pop().unwrap()), Box::from(expressions.pop().unwrap())))
    //         },
    //         _ => unimplemented!("oops forgot to implement multi-argument functions")
    //     }
    // }
}

fn type_to_token(t: Type) -> quote::__private::TokenStream {
    match t {
        Type::Int => quote! {i64},
        Type::Bool => quote! {bool},
        Type::Unit => quote! {Skip},
        Type::Function(t_in, t_out) => {
            let (ts_in, ts_out) = (type_to_token(*t_in), type_to_token(*t_out));
            quote! {Rc<dyn Fn(#ts_in) -> #ts_out>}
        }
    }
}

fn token_to_type(t: &str, lookahead: Lookahead1) -> Result<Type> {
    match t {
        "int" | "Int" | "i64" => Ok(Type::Int),
        "unit" | "Unit" | "()" => Ok(Type::Unit),
        "bool" | "Bool" => Ok(Type::Bool),
        _ => {
            println!("Error 1");
            Err(lookahead.error())
        }
    }
}

/// oh no i actually need to do type inference
/// somehow i manged to put that off until L2
/// and really i can mostly put it off until L3 actually
/// instead of doing it properly I just optmisticlly assume the expression is well-typed
use std::collections::HashMap;
use syn::export::Span;

// todo: functions are not greedy enough
// todo: function parens are slightly off

fn infer_type(e: &Expression, mut variables: HashMap<String, Type>) -> Type {
    println!("STACK TRACE: {:?} @@@@@@ {:?}", variables, e);
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
                panic!(format!("LHS of application wasn't a function, it was {:?}", t_func))
            }
        }
        Expression::Variable(v) => {
            let v_str = &*v.to_string();
            match variables.get(v_str) {
                None => {
                    panic!(format!("Variable {} not found in scope!", v_str))
                }
                Some(t) => {
                    t.clone()
                }
            }
        }
    }
}

fn fix_vars(e: &mut Box<Expression>, variables: &HashMap<String, Type>, count: &mut Vec<Ident>) {
    if let Expression::Variable(v) = &**e {
        let var_name = v.to_string();
        if let Some(t) = variables.get(&var_name) {
            if let Type::Function(_, _) = t {
                let new_ident = Ident::new(&var_name, Span::call_site());
                count.push(new_ident.clone());
                **e = Expression::Variable(new_ident)
            }
        }
    } else {
        match &mut **e {
            Expression::Int(_) => (),
            Expression::Bool(_) => (),
            Expression::Add(ref mut e1, ref mut e2) => {
                fix_vars(e1, variables, count);
                fix_vars(e2, variables, count)
            }
            Expression::GE(ref mut e1, ref mut e2) => {
                fix_vars(e1, variables, count);
                fix_vars(e2, variables, count)
            }
            Expression::Seq(ref mut e1, ref mut e2) => {
                fix_vars(e1, variables, count);
                fix_vars(e2, variables, count)
            }
            Expression::If(ref mut e1, ref mut e2, ref mut e3) => {
                fix_vars(e1, variables, count);
                fix_vars(e2, variables, count);
                fix_vars(e3, variables, count)
            }
            Expression::Deref(_) => (),
            Expression::Assign(_, ref mut e1) => {
                fix_vars(e1, variables, count)
            }
            Expression::Skip => (),
            Expression::While(ref mut e1, ref mut e2) => {
                fix_vars(e1, variables, count);
                fix_vars(e2, variables, count)
            }
            // don't recurse into functions, they will call this function separately
            Expression::Function(_, _, _, _) => (),
            Expression::Application(ref mut e1, ref mut e2) => {
                fix_vars(e1, variables, count);
                fix_vars(e2, variables, count)
            }
            Expression::Variable(_) => unreachable!("we already checked for this case"),
        }

    }

}

/// converts an expression (in our internal representation) back into a token stream
/// now, i know what you're thinking. __private seems dodgy. I agree.
fn to_token_stream(e: Expression, mut variables: HashMap<String, Type>) -> quote::__private::TokenStream {
    println!("@@@ TTS STACK TRACE {:?}", &e);
    match e {
        Expression::Int(e) => quote! {#e},
        Expression::Bool(b) => quote! {#b},
        Expression::Add(e1, e2) => {
            let (ts1, ts2) = (to_token_stream(*e1, variables.clone()), to_token_stream(*e2, variables));
            quote! {mod_l2::l2_compiler::Add {e1: #ts1, e2: #ts2}}
        }
        Expression::GE(e1, e2) => {
            let (ts1, ts2) = (to_token_stream(*e1, variables.clone()), to_token_stream(*e2, variables));
            quote! {mod_l2::l2_compiler::GE {e1: #ts1, e2: #ts2}}
        }
        Expression::Seq(e1, e2) => {
            let (ts1, ts2) = (to_token_stream(*e1, variables.clone()), to_token_stream(*e2, variables));
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
            let (ts1, ts2) = (to_token_stream(*e1, variables.clone()), to_token_stream(*e2, variables));
            quote! {mod_l2::l2_compiler::While {e1: #ts1, e2: #ts2}}
        }
        Expression::Function(l, mut e, Some(t_in), Some(t_out)) => {
            // bugfix: if this function uses a variable f: Rc<_>, we need to clone it _before_
            // entering the closure, not after. We only need to create one copy per variable.
            // meta bugfix: don't try to clone the argument.
            variables.remove(&l.to_string());

            let mut count = vec![];
            fix_vars(&mut e, &variables, &mut count);
            let count = count.iter();

            variables.insert(l.to_string(), t_in.clone());
            let ts = to_token_stream(*e, variables);
            let (ts_in, ts_out) = (type_to_token(t_in), type_to_token(t_out));
            quote! {
                {
                    #(
                        let #count = #count.clone();
                    )*
                    (Rc::from(move |#l : #ts_in| (#ts).step()) as Rc<dyn Fn(#ts_in) -> #ts_out>)
                }
            }
        }
        Expression::Function(l, e, Some(t_in), None) => {
            variables.insert(l.to_string(), t_in.clone());
            let t_out = infer_type(&e, variables.clone());
            to_token_stream(Expression::Function(l, e, Some(t_in), Some(t_out)), variables)
        }

        Expression::Function(_, _, _, _) => {
            panic!("Could not infer type of function!")
        }

        Expression::Application(e1, e2) => {
            let (ts1, ts2) = (to_token_stream(*e1, variables.clone()), to_token_stream(*e2, variables));
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
                eprintln!("Warning: could not infer type for variable: {:?}", v.to_string());
                quote! {#v}
            }

        }
    }
}

/// converts an expression (in our internal representation) back into a token stream
/// for the interpreter module
fn to_interpreted_token_stream(e: Expression) -> quote::__private::TokenStream {
    match e {
        Expression::Int(e) => quote! {mod_l2::l2_interpreter::Expression::VALUE(mod_l2::l2_interpreter::Value::Int(#e))},
        Expression::Bool(b) => quote! {mod_l2::l2_interpreter::Expression::VALUE(mod_l2::l2_interpreter::Value::Bool(#b))},
        Expression::Add(e1, e2) => {
            let (ts1, ts2) = (to_interpreted_token_stream(*e1), to_interpreted_token_stream(*e2));
            quote! {mod_l2::l2_interpreter::Expression::OP(Box::from(#ts1), mod_l2::l2_interpreter::Operation::ADD, Box::from(#ts2))}
        }
        Expression::GE(e1, e2) => {
            let (ts1, ts2) = (to_interpreted_token_stream(*e1), to_interpreted_token_stream(*e2));
            quote! {mod_l2::l2_interpreter::Expression::OP(Box::from(#ts1), mod_l2::l2_interpreter::Operation::GTE, Box::from(#ts2))}
        }
        Expression::Seq(e1, e2) => {
            let (ts1, ts2) = (to_interpreted_token_stream(*e1), to_interpreted_token_stream(*e2));
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
            let (ts1, ts2) = (to_interpreted_token_stream(*e1), to_interpreted_token_stream(*e2));
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
            let (ts1, ts2) = (to_interpreted_token_stream(*e1), to_interpreted_token_stream(*e2));
            quote! {mod_l2::l2_interpreter::Expression::APPLICATION(Box::from(#ts1), Box::from(#ts2))}
        }
        Expression::Variable(v) => {
            quote! {mod_l2::l2_interpreter::Expression::VARIABLE(#v)}
        }
    }
}


#[allow(non_snake_case)]
#[proc_macro]
pub fn L2(tokens: TokenStream) -> TokenStream {
    println!("NEW");
    // this macro is the opposite of hygienic -- it actively requires that a load of types
    // are in scope. Not ideal, but hey, it's for internal use only.

    // parse input into one big expression tree
    let input = parse_macro_input!(tokens as Expression);

    // convert that expression tree into rust code
    let mut variables = HashMap::new();
    variables.insert("arg".into(), Type::Int);
    let expanded = to_token_stream(input, variables);
    let rv = expanded.into();
    return rv
}

#[allow(non_snake_case)]
#[proc_macro]
pub fn L2_interpreter(tokens: TokenStream) -> TokenStream {
    // this macro is the opposite of hygienic -- it actively requires that a load of types
    // are in scope. Not ideal, but hey, it's for internal use only.

    // parse input into one big expression tree
    let input = parse_macro_input!(tokens as Expression);

    // convert that expression tree into rust code
    let expanded = to_interpreted_token_stream(input);
    expanded.into()
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq;!(2 + 2, 4);
    }
}

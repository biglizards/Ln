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
use syn::{
    ext::IdentExt,
    parenthesized,
    parse::{Lookahead1, Parse, ParseBuffer, ParseStream},
    spanned::Spanned,
    token, Ident, LitBool, LitInt, Result, Token,
};
// this import doesn't actually do anything, but it silences a false positive error in CLion
#[allow(unused_imports)]
use syn::token::Token;

use crate::generator;
use crate::generator::Type;
use proc_macro2::Span;

fn token_to_type(t: &str, lookahead: Lookahead1) -> Result<Type> {
    match t {
        "int" | "Int" | "i64" => Ok(Type::Int),
        "unit" | "Unit" | "()" => Ok(Type::Unit),
        "bool" | "Bool" => Ok(Type::Bool),
        _ => Err(lookahead.error()),
    }
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

    Ok(rv)
}

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
    If(Box<Expression>, Box<Expression>, Box<Expression>, Span),
    While(Box<Expression>, Box<Expression>, Span),
    Function(Ident, Box<Expression>, Type, Span),
}

#[derive(Debug)]
pub(crate) enum Assign {
    Term(Term),
    Assign(Ident, Box<Assign>),
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
    Deref(Ident, Span),
    Skip(Span),
    Application(Box<App>, Box<App>),
    Let(Ident, Type, Box<Expression>, Box<Expression>, Span),
    LetRec(Ident, Type, (Ident, Box<Expression>), Box<Expression>, Span),
}

impl Expression {
    fn span(&self) -> Span {
        match self {
            Expression::Assign(a) => a.span(),

            Expression::If(_, _, _, span)
            | Expression::While(_, _, span)
            | Expression::Function(_, _, _, span) => *span,

            Expression::Seq(e1, e2) => e1.span().join(e2.span()).unwrap(),
        }
    }
}
impl Assign {
    fn span(&self) -> Span {
        match self {
            Assign::Term(t) => t.span(),
            Assign::Assign(l, e) => l.span().join(e.span()).unwrap(),
            Assign::GE(e1, e2) => e1.span().join(e2.span()).unwrap(),
        }
    }
}
impl Term {
    fn span(&self) -> Span {
        match self {
            Term::App(a) => a.span(),
            Term::Add(e1, e2) => e1.span().join(e2.span()).unwrap(),
        }
    }
}
impl App {
    fn span(&self) -> Span {
        match self {
            App::Expr(e) => e.span(),
            App::Int(i) => i.span(),
            App::Bool(b) => b.span(),
            App::Var(v) => v.span(),
            App::Application(e1, e2) => e1.span().join(e2.span()).unwrap(),

            App::Deref(_, span)
            | App::Skip(span)
            | App::Let(_, _, _, _, span)
            | App::LetRec(_, _, _, _, span) => *span,
        }
    }
}

impl Parse for Expression {
    fn parse<'a>(input: &'a ParseBuffer<'a>) -> Result<Self> {
        let lookahead = input.lookahead1();

        Ok(if lookahead.peek(Token![if]) {
            let start = input.parse::<Token![if]>()?.span();
            let e1 = input.parse::<Expression>()?;
            input.parse::<kw::then>()?;
            let e2 = input.parse::<Expression>()?;
            input.parse::<Token![else]>()?;
            let e3 = input.parse::<Expression>()?;
            let span = start.join(e3.span()).unwrap();
            Expression::If(Box::from(e1), Box::from(e2), Box::from(e3), span)
        } else if lookahead.peek(Token![while]) {
            let start = input.parse::<Token![while]>()?.span();
            let e1 = input.parse::<Expression>()?;
            input.parse::<Token![do]>()?;
            let e2 = input.parse::<Expression>()?;

            let span = start.join(e2.span()).unwrap();
            Expression::While(Box::from(e1), Box::from(e2), span)
        } else if lookahead.peek(Token![fn]) {
            let start = input.parse::<Token![fn]>()?.span();
            let ident = input.parse::<Ident>()?;
            input.parse::<Token![:]>()?;
            let in_type = parse_type(input)?;
            input.parse::<Token![=>]>()?;
            let e = input.parse::<Expression>()?;

            let span = start.join(e.span()).unwrap();
            Expression::Function(ident, Box::from(e), in_type, span)
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

                Assign::Assign(ident, Box::from(input.parse::<Assign>()?))
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
                let span = input.parse::<kw::skip>()?.span();
                App::Skip(span)
            } else if lookahead.peek(Token![!]) {
                let start = input.parse::<Token![!]>()?.span();
                let ident = input.parse::<Ident>()?;
                let span = start.join(ident.span()).unwrap();
                App::Deref(ident, span)
            } else if lookahead.peek(Token![let]) {
                let start = input.parse::<Token![let]>()?.span();
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
                    if let Expression::Function(y, e1, _, _) = function_e {
                        input.parse::<Token![in]>()?;
                        let e2: Expression = input.parse()?;
                        let end = input.parse::<kw::end>()?.span();

                        let span = start.join(end).unwrap();
                        App::LetRec(x, t, (y, e1), Box::from(e2), span)
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
                    let end = input.parse::<kw::end>()?.span();

                    let span = start.join(end).unwrap();
                    App::Let(x, t, Box::from(e1), Box::from(e2), span)
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

        Ok(rv)
    }
}

pub(crate) trait Reduce {
    fn reduce(self) -> generator::Expression;
}

impl Reduce for Expression {
    fn reduce(self) -> generator::Expression {
        match self {
            Expression::Assign(a) => a.reduce(),
            Expression::Seq(e1, e2) => {
                generator::Expression::Seq(Box::from(e1.reduce()), Box::from(e2.reduce()))
            }
            Expression::If(e1, e2, e3, span) => generator::Expression::If(
                Box::from(e1.reduce()),
                Box::from(e2.reduce()),
                Box::from(e3.reduce()),
                span,
            ),
            Expression::While(e1, e2, span) => {
                generator::Expression::While(Box::from(e1.reduce()), Box::from(e2.reduce()), span)
            }
            Expression::Function(x, e, t, span) => {
                generator::Expression::Function(x, Box::from(e.reduce()), Some(t), None, span)
            }
        }
    }
}

impl Reduce for Assign {
    fn reduce(self) -> generator::Expression {
        match self {
            Assign::Term(t) => t.reduce(),
            Assign::Assign(l, a) => generator::Expression::Assign(l, Box::from(a.reduce())),
            Assign::GE(t1, t2) => {
                generator::Expression::GE(Box::from(t1.reduce()), Box::from(t2.reduce()))
            }
        }
    }
}

impl Reduce for Term {
    fn reduce(self) -> generator::Expression {
        match self {
            Term::Add(t1, t2) => {
                generator::Expression::Add(Box::from(t1.reduce()), Box::from(t2.reduce()))
            }
            Term::App(app) => app.reduce(),
        }
    }
}

impl Reduce for App {
    fn reduce(self) -> generator::Expression {
        match self {
            App::Expr(e) => e.reduce(),
            App::Int(i) => generator::Expression::Int(i),
            App::Var(v) => generator::Expression::Variable(v),
            App::Application(a1, a2) => {
                generator::Expression::Application(Box::from(a1.reduce()), Box::from(a2.reduce()))
            }
            App::Deref(l, span) => generator::Expression::Deref(l, span),
            App::Skip(span) => generator::Expression::Skip(span),
            App::Bool(b) => generator::Expression::Bool(b),
            App::Let(x, t, e1, e2, span) => generator::Expression::Let(
                x,
                t,
                Box::from(e1.reduce()),
                Box::from(e2.reduce()),
                span,
            ),
            App::LetRec(x, t, (y, e1), e2, span) => generator::Expression::LetRec(
                x,
                t,
                y,
                Box::from(e1.reduce()),
                Box::from(e2.reduce()),
                span,
            ),
        }
    }
}

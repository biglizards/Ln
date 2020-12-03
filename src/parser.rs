pub mod parse {
    use crate::{Location, Operation, Value};

    #[derive(Debug, Clone)]
    enum Keyword {
        IF,
        THEN,
        ELSE,
        SKIP,
        WHILE,
        DO,
    }

    #[derive(Debug, Clone)]
    enum Token {
        Value(crate::Value),
        Loc(Location),
        Operator(Operation),
        Keyword(Keyword),
        ASSIGN,
        DEREF,
        SEMICOLON,
        SKIP,
    }

    fn tokenize(s: &str) {

    }

    // fn get_next_expression(tokens: &[Token], prior: Option<Box<dyn Step<LinearStore, Value>>>)
    //                        -> Option<Box<dyn Step<LinearStore, Value>>> {
    //     if tokens.len() == 0 {
    //         return None;
    //     }
    //     let first_token: Token = tokens[0].clone();
    //     let x: Box<dyn Step<LinearStore, Value>> = match first_token {
    //         Token::Bool(b) => Box::from(b),
    //         Token::Int(i) => Box::from(i),
    //         Token::Loc(_) => return None,
    //         Token::Operator(op) => {
    //             let next_expr: Box<dyn Step<LinearStore, Value>> =
    //                 get_next_expression(&tokens[1..], None)?;
    //             let prior_expr = prior?;
    //             println!("i'm {:?}", op);
    //             Box::from(Add { e1: next_expr.step(&mut LinearStore { store: vec![] }),
    //                 e2: prior_expr.step(&mut LinearStore { store: vec![] }) })
    //         },
    //         Token::Keyword(_) => return None,
    //         Token::ASSIGN => return None,
    //         Token::DEREF => return None,
    //         SEMICOLON => return None,
    //         Token::SKIP => return None,
    //     };
    //
    //     Some(x)
    // }
}

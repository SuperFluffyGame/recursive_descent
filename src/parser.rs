mod ast;

use crate::lexer::{Lexeme, Lexer, Token};
pub use ast::Expr;
pub type ParseResult = Result<Expr, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    ExpectedButGot(Vec<Lexeme>, Token),
}
impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedButGot(expected_vec, got) => {
                let mut expected_string = String::new();

                for (i, expected) in expected_vec.iter().enumerate() {
                    if expected_vec.len() > 1 && i == expected_vec.len() - 2 {
                        expected_string += &format!(", or {}", expected);
                        break;
                    } else if i == 0 {
                        expected_string += &format!("{}", expected);
                        continue;
                    }
                    expected_string += &format!(", {}", expected);
                }

                let string = format!(
                    "expected {}, but got {} at line {}, column {}",
                    expected_string,
                    got.lexeme,
                    got.line + 1,
                    got.column + 2,
                );

                writeln!(f, "{}", string)
            }
        }
    }
}

fn expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    assignment_expr(lexer)
}

fn assignment_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = add_expr(lexer)?;

    loop {
        if let Lexeme::Equal = lexer.next_token.lexeme {
            lexer.scan();
            let b = expr(lexer)?;
            a = Expr::Assign(Box::new(a), Box::new(b));
        } else {
            return Ok(a);
        }
    }
}

fn primary_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let expects: Vec<Lexeme> = vec![
        Lexeme::Let,
        Lexeme::Identifier(String::new()),
        Lexeme::String(String::new()),
        Lexeme::Return,
        Lexeme::Fn,
        Lexeme::LBracket,
        Lexeme::LBrace,
        Lexeme::Number(0.0),
        Lexeme::LParen,
    ];

    let tok = lexer.next_token.clone();
    let lexeme = tok.lexeme.clone();
    if let Lexeme::Let = lexeme {
        return let_expr(lexer);
    } else if let Lexeme::Identifier(i) = lexeme {
        lexer.scan();
        return Ok(Expr::Ident(i));
    } else if let Lexeme::String(s) = lexeme {
        lexer.scan();
        return Ok(Expr::String(s));
    } else if let Lexeme::Return = lexeme {
        return return_expr(lexer);
    } else if let Lexeme::Fn = lexeme {
        return function_declaration(lexer);
    } else if let Lexeme::LBracket = lexeme {
        return array(lexer);
    } else if let Lexeme::LBrace = lexeme {
        return block_expr(lexer);
    } else if let Lexeme::Number(n) = lexeme {
        lexer.scan();
        return Ok(Expr::Number(n));
    } else if let Lexeme::LParen = lexeme {
        lexer.scan();
        let expr = expr(lexer)?;
        if let Lexeme::RParen = lexer.next_token.lexeme {
            lexer.scan();
            return Ok(expr);
        } else {
            return Err(ParserError::ExpectedButGot(
                vec![Lexeme::RParen],
                lexer.next_token.clone(),
            ));
        }
    } else {
        return Err(ParserError::ExpectedButGot(
            expects,
            lexer.next_token.clone(),
        ));
    }
}

fn add_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = mul_expr(lexer)?;

    loop {
        if let Lexeme::Plus = lexer.next_token.lexeme {
            lexer.scan();
            let b = mul_expr(lexer)?;
            a = Expr::Add(Box::new(a), Box::new(b));
        } else if let Lexeme::Minus = lexer.next_token.lexeme {
            lexer.scan();
            let b = mul_expr(lexer)?;
            a = Expr::Sub(Box::new(a), Box::new(b));
        } else {
            return Ok(a);
        }
    }
}

fn mul_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = unary_expr(lexer)?;

    loop {
        if let Lexeme::Asterisk = lexer.next_token.lexeme {
            lexer.scan();
            let b = unary_expr(lexer)?;
            a = Expr::Mul(Box::new(a), Box::new(b));
        } else if let Lexeme::Slash = lexer.next_token.lexeme {
            lexer.scan();
            let b = unary_expr(lexer)?;
            a = Expr::Div(Box::new(a), Box::new(b));
        } else {
            return Ok(a);
        }
    }
}

fn unary_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Lexeme::Minus = lexer.next_token.lexeme {
        lexer.scan();
        return Ok(Expr::Neg(Box::new(exp_expr(lexer)?)));
    } else {
        return exp_expr(lexer);
    }
}

fn exp_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = function_call(lexer)?;

    loop {
        if let Lexeme::DoubleAsterisk = lexer.next_token.lexeme {
            lexer.scan();
            let b = unary_expr(lexer)?;
            a = Expr::Exp(Box::new(a), Box::new(b));
        } else {
            return Ok(a);
        }
    }
}

fn function_call(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = index_expr(lexer)?;

    loop {
        if let Lexeme::LParen = lexer.next_token.lexeme {
            lexer.scan();
            let exprs = expr_list(lexer)?;
            if let Lexeme::RParen = lexer.next_token.lexeme {
                lexer.scan();
                a = Expr::FunctionCall(Box::new(a), exprs);
            } else {
                return Err(ParserError::ExpectedButGot(
                    vec![Lexeme::RParen],
                    lexer.next_token.clone(),
                ));
            }
        } else {
            return Ok(a);
        }
    }
}

fn index_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = primary_expr(lexer)?;

    loop {
        if let Lexeme::LBracket = lexer.next_token.lexeme {
            lexer.scan();
            let expr = expr(lexer)?;
            if let Lexeme::RBracket = lexer.next_token.lexeme {
                lexer.scan();
                a = Expr::Index(Box::new(a), Box::new(expr));
            } else {
                return Err(ParserError::ExpectedButGot(
                    vec![Lexeme::RBracket],
                    lexer.next_token.clone(),
                ));
            }
        } else {
            return Ok(a);
        }
    }
}

fn let_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Lexeme::Let = lexer.next_token.lexeme {
        lexer.scan();
        if let Lexeme::Identifier(i) = lexer.next_token.lexeme.clone() {
            lexer.scan();
            if let Lexeme::Equal = lexer.next_token.lexeme {
                lexer.scan();
                let expr = expr(lexer)?;
                let stmt = Expr::Let(i, Box::new(expr));

                Ok(stmt)
            } else {
                Err(ParserError::ExpectedButGot(
                    vec![Lexeme::Equal],
                    lexer.next_token.clone(),
                ))
            }
        } else {
            Err(ParserError::ExpectedButGot(
                vec![Lexeme::Identifier(String::new())],
                lexer.next_token.clone(),
            ))
        }
    } else {
        Err(ParserError::ExpectedButGot(
            vec![Lexeme::Let],
            lexer.next_token.clone(),
        ))
    }
}

fn statement(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let expr = expr(lexer)?;

    if let Expr::Block(_) = &expr {
        if let Lexeme::SemiColon = lexer.next_token.lexeme {
            lexer.scan();
        }
        return Ok(expr);
    }
    if let Expr::FunctionDeclaration(..) = &expr {
        if let Lexeme::SemiColon = lexer.next_token.lexeme {
            lexer.scan();
        }
        return Ok(expr);
    }
    if let Lexeme::SemiColon = lexer.next_token.lexeme {
        lexer.scan();
        return Ok(expr);
    } else if let Expr::Return(..) = expr {
        return Ok(expr);
    } else {
        return Ok(Expr::Return(Box::new(expr)));
    }
}

pub fn program(lexer: &mut Lexer) -> Result<Vec<Expr>, ParserError> {
    let mut o = Vec::new();

    loop {
        if let Lexeme::EOF = lexer.next_token.lexeme {
            return Ok(o);
        } else {
            let stmt = statement(lexer)?;
            o.push(stmt);
        }
    }
}

fn expr_list(lexer: &mut Lexer) -> Result<Vec<Expr>, ParserError> {
    let mut exprs = Vec::new();

    let mut first_expr_lexer = lexer.clone();
    let first_expr = expr(&mut first_expr_lexer);
    if let Ok(expr) = first_expr {
        *lexer = first_expr_lexer;
        exprs.push(expr);
    }

    loop {
        if let Lexeme::Comma = lexer.next_token.lexeme {
            lexer.scan();
            exprs.push(expr(lexer)?);
        } else {
            break;
        }
    }

    Ok(exprs)
}

fn array(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Lexeme::LBracket = lexer.next_token.lexeme {
        lexer.scan();
        let exprs = expr_list(lexer)?;
        if let Lexeme::RBracket = lexer.next_token.lexeme {
            lexer.scan();
            return Ok(Expr::Array(exprs));
        } else {
            Err(ParserError::ExpectedButGot(
                vec![Lexeme::RBracket],
                lexer.next_token.clone(),
            ))
        }
    } else {
        Err(ParserError::ExpectedButGot(
            vec![Lexeme::LBracket],
            lexer.next_token.clone(),
        ))
    }
}

fn block_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Lexeme::LBrace = lexer.next_token.lexeme {
        lexer.scan();
        let mut o = Vec::new();

        loop {
            if let Lexeme::RBrace = lexer.next_token.lexeme {
                lexer.scan();
                return Ok(Expr::Block(o));
            }

            let stmt = statement(lexer)?;
            o.push(stmt);
        }
    } else {
        Err(ParserError::ExpectedButGot(
            vec![Lexeme::LBrace],
            lexer.next_token.clone(),
        ))
    }
}

fn return_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Lexeme::Return = lexer.next_token.lexeme {
        lexer.scan();
        let expr = expr(lexer)?;

        return Ok(Expr::Return(Box::new(expr)));
    } else {
        Err(ParserError::ExpectedButGot(
            vec![Lexeme::Return],
            lexer.next_token.clone(),
        ))
    }
}

fn identifier_list(lexer: &mut Lexer) -> Result<Vec<Expr>, ParserError> {
    let mut identifiers = Vec::new();

    if let Lexeme::Identifier(i) = lexer.next_token.lexeme.clone() {
        lexer.scan();
        identifiers.push(Expr::Ident(i))
    }

    loop {
        if let Lexeme::Comma = lexer.next_token.lexeme {
            lexer.scan();
            if let Lexeme::Identifier(i) = lexer.next_token.lexeme.clone() {
                lexer.scan();
                identifiers.push(Expr::Ident(i))
            }
        } else {
            break;
        }
    }

    Ok(identifiers)
}

fn function_declaration(lexer: &mut Lexer) -> ParseResult {
    if let Lexeme::Fn = lexer.next_token.lexeme {
        lexer.scan();
        let mut id = None;
        if let Lexeme::Identifier(i) = lexer.next_token.lexeme.clone() {
            lexer.scan();
            id = Some(i);
        }

        if let Lexeme::LParen = lexer.next_token.lexeme {
            lexer.scan();
            let list = identifier_list(lexer)?;

            if let Lexeme::RParen = lexer.next_token.lexeme {
                lexer.scan();

                let block = block_expr(lexer)?;

                return Ok(Expr::FunctionDeclaration(id, list, Box::new(block)));
            } else {
                Err(ParserError::ExpectedButGot(
                    vec![Lexeme::RParen],
                    lexer.next_token.clone(),
                ))
            }
        } else {
            Err(ParserError::ExpectedButGot(
                vec![Lexeme::LParen],
                lexer.next_token.clone(),
            ))
        }
    } else {
        Err(ParserError::ExpectedButGot(
            vec![Lexeme::Fn],
            lexer.next_token.clone(),
        ))
    }
}

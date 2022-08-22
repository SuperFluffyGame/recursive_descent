mod ast;

use crate::lexer::{Lexer, Token};
pub use ast::Expr;
pub type ParseResult = Result<Expr, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token),
    UnexpectedEOF,
}

fn expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    assignment_expr(lexer)
}

fn assignment_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = add_expr(lexer)?;

    loop {
        if let Token::Equal = lexer.next_token {
            lexer.scan();
            let b = expr(lexer)?;
            a = Expr::Assign(Box::new(a), Box::new(b));
        } else {
            return Ok(a);
        }
    }
}

fn primary_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut let_lexer = lexer.clone();
    let let_expr = let_expr(&mut let_lexer);
    if let Ok(expr) = let_expr {
        *lexer = let_lexer;
        return Ok(expr);
    }

    // let mut function_call_lexer = lexer.clone();
    // let function_call_expr = function_call(&mut function_call_lexer);
    // if let Ok(expr) = function_call_expr {
    //     *lexer = function_call_lexer;
    //     return Ok(expr);
    // }

    let tok = lexer.next_token.clone();
    if let Token::Identifier(i) = tok {
        lexer.scan();
        return Ok(Expr::Ident(i));
    } else if let Token::String(s) = tok {
        lexer.scan();
        return Ok(Expr::String(s));
    } else if let Token::Return = tok {
        return return_expr(lexer);
    } else if let Token::Fn = tok {
        return function_declaration(lexer);
    } else if let Token::LBracket = tok {
        return array(lexer);
    } else if let Token::LBrace = tok {
        return block_expr(lexer);
    } else if let Token::Number(n) = tok {
        lexer.scan();
        return Ok(Expr::Number(n));
    } else if let Token::LParen = tok {
        lexer.scan();
        let expr = expr(lexer)?;
        if let Token::RParen = lexer.next_token {
            lexer.scan();
            return Ok(expr);
        } else {
            return Err(ParserError::UnexpectedToken(lexer.next_token.clone()));
        }
    } else {
        return Err(ParserError::UnexpectedToken(tok));
    }
}

fn add_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = mul_expr(lexer)?;

    loop {
        if let Token::Plus = lexer.next_token {
            lexer.scan();
            let b = mul_expr(lexer)?;
            a = Expr::Add(Box::new(a), Box::new(b));
        } else if let Token::Minus = lexer.next_token {
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
        if let Token::Asterisk = lexer.next_token {
            lexer.scan();
            let b = unary_expr(lexer)?;
            a = Expr::Mul(Box::new(a), Box::new(b));
        } else if let Token::Slash = lexer.next_token {
            lexer.scan();
            let b = unary_expr(lexer)?;
            a = Expr::Div(Box::new(a), Box::new(b));
        } else {
            return Ok(a);
        }
    }
}

fn unary_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Token::Minus = lexer.next_token {
        lexer.scan();
        return Ok(Expr::Neg(Box::new(exp_expr(lexer)?)));
    } else {
        return exp_expr(lexer);
    }
}

fn exp_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = function_call(lexer)?;

    loop {
        if let Token::DoubleAsterisk = lexer.next_token {
            lexer.scan();
            let b = unary_expr(lexer)?;
            a = Expr::Exp(Box::new(a), Box::new(b));
        } else {
            return Ok(a);
        }
    }
}

fn function_call(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = primary_expr(lexer)?;

    loop {
        if let Token::LParen = lexer.next_token {
            lexer.scan();
            let exprs = expr_list(lexer)?;
            if let Token::RParen = lexer.next_token {
                lexer.scan();
                a = Expr::FunctionCall(Box::new(a), exprs);
            } else {
                return Err(ParserError::UnexpectedToken(lexer.next_token.clone()));
            }
        } else {
            return Ok(a);
        }
    }
}

fn let_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Token::Let = lexer.next_token {
        lexer.scan();
        if let Token::Identifier(i) = lexer.next_token.clone() {
            lexer.scan();
            if let Token::Equal = lexer.next_token {
                lexer.scan();
                let expr = expr(lexer)?;
                let stmt = Expr::Let(i, Box::new(expr));

                Ok(stmt)
            } else {
                Err(ParserError::UnexpectedToken(lexer.next_token.clone()))
            }
        } else {
            Err(ParserError::UnexpectedToken(lexer.next_token.clone()))
        }
    } else {
        Err(ParserError::UnexpectedToken(lexer.next_token.clone()))
    }
}

fn statement(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let expr = expr(lexer)?;

    if let Expr::Block(_) = &expr {
        if let Token::SemiColon = lexer.next_token {
            lexer.scan();
        }
        return Ok(expr);
    }
    if let Expr::FunctionDeclaration(..) = &expr {
        if let Token::SemiColon = lexer.next_token {
            lexer.scan();
        }
        return Ok(expr);
    }
    if let Token::SemiColon = lexer.next_token {
        lexer.scan();
        return Ok(expr);
    } else {
        return Ok(Expr::Return(Box::new(expr)));
    }
}

pub fn program(lexer: &mut Lexer) -> Result<Vec<Expr>, ParserError> {
    let mut o = Vec::new();

    loop {
        if let Token::EOF = lexer.next_token {
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
        if let Token::Comma = lexer.next_token {
            lexer.scan();
            exprs.push(expr(lexer)?);
        } else {
            break;
        }
    }

    Ok(exprs)
}

fn array(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Token::LBracket = lexer.next_token {
        lexer.scan();
        let exprs = expr_list(lexer)?;
        if let Token::RBracket = lexer.next_token {
            lexer.scan();
            return Ok(Expr::Array(exprs));
        } else {
            return Err(ParserError::UnexpectedToken(lexer.next_token.clone()));
        }
    } else {
        return Err(ParserError::UnexpectedToken(lexer.next_token.clone()));
    }
}

fn block_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Token::LBrace = lexer.next_token {
        lexer.scan();
        let mut o = Vec::new();

        loop {
            if let Token::RBrace = lexer.next_token {
                lexer.scan();
                return Ok(Expr::Block(o));
            }

            let stmt = statement(lexer)?;
            o.push(stmt);
        }
    } else {
        return Err(ParserError::UnexpectedToken(lexer.next_token.clone()));
    }
}

fn return_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Token::Return = lexer.next_token {
        lexer.scan();
        let expr = expr(lexer)?;

        return Ok(Expr::Return(Box::new(expr)));
    } else {
        return Err(ParserError::UnexpectedToken(lexer.next_token.clone()));
    }
}

fn identifier_list(lexer: &mut Lexer) -> Result<Vec<Expr>, ParserError> {
    let mut identifiers = Vec::new();

    if let Token::Identifier(i) = lexer.next_token.clone() {
        lexer.scan();
        identifiers.push(Expr::Ident(i))
    }

    loop {
        if let Token::Comma = lexer.next_token {
            lexer.scan();
            if let Token::Identifier(i) = lexer.next_token.clone() {
                lexer.scan();
                identifiers.push(Expr::Ident(i))
            }
        } else {
            break;
        }
    }

    Ok(identifiers)

    // todo!()
}

fn function_declaration(lexer: &mut Lexer) -> ParseResult {
    if let Token::Fn = lexer.next_token {
        lexer.scan();
        let mut id = None;
        if let Token::Identifier(i) = lexer.next_token.clone() {
            lexer.scan();
            id = Some(i);
        }

        if let Token::LParen = lexer.next_token {
            lexer.scan();
            let list = identifier_list(lexer)?;

            if let Token::RParen = lexer.next_token {
                lexer.scan();

                let block = block_expr(lexer)?;

                return Ok(Expr::FunctionDeclaration(id, list, Box::new(block)));
            } else {
                return Err(ParserError::UnexpectedToken(lexer.next_token.clone()));
            }
        } else {
            return Err(ParserError::UnexpectedToken(lexer.next_token.clone()));
        }
    } else {
        return Err(ParserError::UnexpectedToken(lexer.next_token.clone()));
    }
}

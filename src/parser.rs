mod ast;

use crate::lexer::{Lexer, Token};
pub use ast::Expr;
pub use ast::Statement;

#[derive(Debug)]
pub enum ParserError {
    UnexpectedToken(Token),
    UnexpectedEOF,
}

pub fn expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    assignment_expr(lexer)
}

pub fn assignment_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
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

pub fn primary_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let tok = lexer.next_token.clone();

    let mut function_call_lexer = lexer.clone();
    let function_call_expr = function_call(&mut function_call_lexer);
    if let Ok(expr) = function_call_expr {
        *lexer = function_call_lexer;
        return Ok(expr);
    }

    if let Token::Identifier(i) = tok {
        lexer.scan();
        return Ok(Expr::Ident(i));
    } else if let Token::String(s) = tok {
        lexer.scan();
        return Ok(Expr::String(s));
    } else if let Token::LBracket = tok {
        return Ok(array(lexer)?);
    } else if let Token::LBrace = tok {
        return Ok(block(lexer)?);
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

pub fn add_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
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

pub fn mul_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
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

pub fn unary_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Token::Minus = lexer.next_token {
        lexer.scan();
        return Ok(Expr::Neg(Box::new(exp_expr(lexer)?)));
    } else {
        return exp_expr(lexer);
    }
}

pub fn exp_expr(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    let mut a = primary_expr(lexer)?;

    loop {
        if let Token::Caret = lexer.next_token {
            lexer.scan();
            let b = unary_expr(lexer)?;
            a = Expr::Exp(Box::new(a), Box::new(b));
        } else {
            return Ok(a);
        }
    }
}

pub fn let_statement(lexer: &mut Lexer) -> Result<Statement, ParserError> {
    if let Token::Let = lexer.next_token {
        lexer.scan();
        if let Token::Identifier(i) = lexer.next_token.clone() {
            lexer.scan();
            if let Token::Equal = lexer.next_token {
                lexer.scan();
                let expr = expr(lexer)?;
                let stmt = Statement::Let(i, expr);

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

pub fn expr_statement(lexer: &mut Lexer) -> Result<Statement, ParserError> {
    let expr = expr(lexer);

    if let Ok(expr) = expr {
        return Ok(Statement::Expr(expr));
    } else {
        return Err(expr.unwrap_err());
    }
}

pub fn statement(lexer: &mut Lexer) -> Result<Statement, ParserError> {
    let mut o_stmt: Option<Statement> = None;

    let mut let_lexer = lexer.clone();
    let let_statement = let_statement(&mut let_lexer);
    if let Ok(stmt) = let_statement {
        *lexer = let_lexer;
        o_stmt = Some(stmt);
    }

    let mut expr_lexer = lexer.clone();
    let expr_statement = expr_statement(&mut expr_lexer);
    if let Ok(stmt) = expr_statement {
        *lexer = expr_lexer;
        o_stmt = Some(stmt);
    }

    if let None = o_stmt {
        Err(ParserError::UnexpectedToken(lexer.next_token.clone()))
    } else if let Token::SemiColon = lexer.next_token.clone() {
        lexer.scan();
        Ok(o_stmt.unwrap())
    } else {
        Err(ParserError::UnexpectedToken(lexer.next_token.clone()))
    }
}

pub fn program(lexer: &mut Lexer) -> Result<Vec<Statement>, ParserError> {
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

pub fn expr_list(lexer: &mut Lexer) -> Result<Vec<Expr>, ParserError> {
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

pub fn function_call(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Token::Identifier(i) = lexer.next_token.clone() {
        lexer.scan();
        if let Token::LParen = lexer.next_token {
            lexer.scan();
            let exprs = expr_list(lexer)?;
            if let Token::RParen = lexer.next_token {
                lexer.scan();
                return Ok(Expr::FunctionCall(i, exprs));
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

pub fn array(lexer: &mut Lexer) -> Result<Expr, ParserError> {
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

pub fn block(lexer: &mut Lexer) -> Result<Expr, ParserError> {
    if let Token::LBrace = lexer.next_token {
        lexer.scan();
        let mut o = Vec::new();

        loop {
            if let Token::EOF = lexer.next_token {
                return Ok(Expr::Block(o));
            } else {
                let stmt = statement(lexer)?;
                o.push(stmt);
            }
        }
    } else {
        return Err(ParserError::UnexpectedToken(lexer.next_token.clone()));
    }
}

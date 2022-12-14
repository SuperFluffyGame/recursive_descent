#[derive(Debug, Clone)]
pub enum Expr {
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Exp(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Neg(Box<Expr>),

    Assign(Box<Expr>, Box<Expr>),

    Number(f64),
    String(String),
    Ident(String),

    FunctionCall(Box<Expr>, Vec<Expr>),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),

    Block(Vec<Expr>),
    Let(String, Box<Expr>),

    Return(Box<Expr>),

    FunctionDeclaration(Option<String>, Vec<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum Lexeme {
    Identifier(String),
    Number(f64),
    String(String),

    Plus,
    Minus,
    Asterisk,
    Slash,
    Caret,
    DoubleAsterisk,

    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    Equal,
    SemiColon,
    Comma,

    Let,
    Return,
    Fn,

    EOF,
    Unexpected(char),
}
impl std::fmt::Display for Lexeme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lexeme::Asterisk => write!(f, r#""*""#),
            Lexeme::Caret => write!(f, r#""^""#),
            Lexeme::Comma => write!(f, r#"",""#),
            Lexeme::DoubleAsterisk => write!(f, r#""**""#),
            Lexeme::EOF => write!(f, "EOF"),
            Lexeme::Equal => write!(f, r#""=""#),
            Lexeme::Fn => write!(f, r#""fn""#),
            Lexeme::Identifier(_) => write!(f, r#"IDENTIFIER"#),
            Lexeme::LBrace => write!(f, r#""{{""#),
            Lexeme::LBracket => write!(f, r#""[""#),
            Lexeme::LParen => write!(f, r#""(""#),
            Lexeme::Let => write!(f, r#""let""#),
            Lexeme::Minus => write!(f, r#""-""#),
            Lexeme::Number(_) => write!(f, r#"NUMBER"#),
            Lexeme::Plus => write!(f, r#""+""#),
            Lexeme::RBrace => write!(f, r#""}}""#),
            Lexeme::RBracket => write!(f, r#""]""#),
            Lexeme::RParen => write!(f, r#"")""#),
            Lexeme::Return => write!(f, r#""return""#),
            Lexeme::SemiColon => write!(f, r#"";""#),
            Lexeme::Slash => write!(f, r#""/""#),
            Lexeme::String(_) => write!(f, r#"STRING"#),
            Lexeme::Unexpected(_) => write!(f, ""),
        }
    }
}
#[derive(Debug, Clone)]
pub struct Token {
    pub lexeme: Lexeme,
    pub line: usize,
    pub column: usize,
}

#[derive(Clone)]
pub struct Lexer {
    input: String,
    pub tokens: Vec<Token>,
    index: usize,
    pub next_token: Token,
    lines: usize,
    columns: usize,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            tokens: Vec::new(),
            index: 0,
            next_token: Token {
                lexeme: Lexeme::EOF,
                line: 0,
                column: 0,
            },
            lines: 0,
            columns: 0,
        };

        l.tokenize();
        l
    }

    fn tokenize(&mut self) {
        let mut line = 0;
        let mut column = 0;
        let mut chars = self.input.chars().peekable();

        'outer: while let Some(c) = chars.next() {
            column += 1;
            let lexeme = match c {
                '+' => Lexeme::Plus,
                '-' => Lexeme::Minus,
                '*' => {
                    let o = if let Some('*') = chars.peek() {
                        chars.next();
                        Lexeme::DoubleAsterisk
                    } else {
                        Lexeme::Asterisk
                    };
                    o
                }
                '/' => Lexeme::Slash,
                '^' => Lexeme::Caret,
                '(' => Lexeme::LParen,
                ')' => Lexeme::RParen,
                '[' => Lexeme::LBracket,
                ']' => Lexeme::RBracket,
                '=' => Lexeme::Equal,
                ';' => Lexeme::SemiColon,
                ',' => Lexeme::Comma,
                '{' => Lexeme::LBrace,
                '}' => Lexeme::RBrace,
                '#' => {
                    while let Some(c) = chars.peek() {
                        if c == &'\n' {
                            continue 'outer;
                        } else {
                            chars.next();
                        }
                    }
                    continue;
                }
                '"' => {
                    let mut s = String::new();
                    while let Some(c) = chars.next() {
                        if c == '"' {
                            break;
                        }
                        s.push(c);
                    }
                    Lexeme::String(s)
                }
                '\n' => {
                    line += 1;
                    column = 0;
                    continue;
                }
                _ => {
                    if c.is_alphabetic() {
                        let mut s = String::new();
                        s.push(c);
                        while let Some(c) = chars.peek() {
                            if c.is_alphanumeric() || c == &'_' {
                                s.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }

                        let mut kw_token: Option<Lexeme> = None;

                        match &s as &str {
                            "let" => kw_token = Some(Lexeme::Let),
                            "return" => kw_token = Some(Lexeme::Return),
                            "fn" => kw_token = Some(Lexeme::Fn),
                            _ => {}
                        }

                        if let None = kw_token {
                            kw_token = Some(Lexeme::Identifier(s));
                        }
                        kw_token.unwrap()
                    } else if c.is_numeric() {
                        let mut s = String::new();
                        s.push(c);
                        while let Some(c) = chars.peek() {
                            if c.is_numeric() {
                                s.push(chars.next().unwrap());
                            } else {
                                break;
                            }
                        }
                        Lexeme::Number(s.parse().unwrap())
                    } else if c.is_whitespace() {
                        continue;
                    } else {
                        Lexeme::Unexpected(c)
                    }
                }
            };

            let token = Token {
                lexeme,
                line,
                column,
            };
            self.tokens.push(token);
        }

        self.lines = line;
        self.columns = column;

        self.scan();
    }

    pub fn scan(&mut self) {
        let t = self.tokens.get(self.index);
        if let Some(t) = t {
            self.next_token = t.clone()
        } else {
            self.next_token = Token {
                lexeme: Lexeme::EOF,
                line: self.lines,
                column: self.columns,
            }
        }
        self.index += 1;
    }
}

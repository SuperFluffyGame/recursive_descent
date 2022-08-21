#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Identifier(String),
    Number(f64),
    String(String),

    Plus,
    Minus,
    Asterisk,
    Slash,
    Caret,

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

    EOF,
    Unexpected(char),
}

const KEYWORDS: [&str; 1] = ["let"];

#[derive(Clone)]
pub struct Lexer {
    input: String,
    tokens: Vec<Token>,
    index: usize,
    pub next_token: Token,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            tokens: Vec::new(),
            index: 0,
            next_token: Token::EOF,
        };

        l.tokenize();
        l
    }

    fn tokenize(&mut self) {
        let mut chars = self.input.chars().peekable();

        while let Some(c) = chars.next() {
            let t = match c {
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '^' => Token::Caret,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '[' => Token::LBracket,
                ']' => Token::RBracket,
                '=' => Token::Equal,
                ';' => Token::SemiColon,
                ',' => Token::Comma,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '"' => {
                    let mut s = String::new();
                    while let Some(c) = chars.next() {
                        if c == '"' {
                            break;
                        }
                        s.push(c);
                    }
                    Token::String(s)
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

                        let mut kw_token: Option<Token> = None;
                        for kw in KEYWORDS {
                            if s == kw {
                                kw_token = Some(Token::Let);
                            }
                        }
                        if let None = kw_token {
                            kw_token = Some(Token::Identifier(s));
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
                        Token::Number(s.parse().unwrap())
                    } else if c.is_whitespace() {
                        continue;
                    } else {
                        Token::Unexpected(c)
                    }
                }
            };
            self.tokens.push(t);
        }

        self.scan();
    }

    pub fn scan(&mut self) {
        let t = self.tokens.get(self.index);
        if let Some(t) = t {
            self.next_token = t.clone()
        } else {
            self.next_token = Token::EOF
        }
        self.index += 1;
    }
}

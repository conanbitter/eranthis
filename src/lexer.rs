use std::{char, collections::VecDeque, str::Chars};

use phf_macros::phf_map;

use crate::parser::Token;

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "and" => Token::KwAnd,
    "const" => Token::KwConst,
    "elif" => Token::KwElif,
    "else" => Token::KwElse,
    "false" => Token::KwFalse,
    "for" => Token::KwFor,
    "func" => Token::KwFunc,
    "if" => Token::KwIf,
    "in" => Token::KwIn,
    "not" => Token::KwNot,
    "or" => Token::KwOr,
    "ref" => Token::KwRef,
    "return" => Token::KwReturn,
    "step" => Token::KwStep,
    "struct" => Token::KwStruct,
    "then" => Token::KwThen,
    "true" => Token::KwTrue,
    "to" => Token::KwTo,
    "type" => Token::KwType,
    "var" => Token::KwVar,
    "while" => Token::KwWhile,
};

static SINGLE_CHAR_OPS: phf::Map<char, Token> = phf_map! {
    '=' => Token::Assign,
    '>' => Token::Greater,
    '<' => Token::Less,
    '+' => Token::Add,
    '-' => Token::Sub,
    '*' => Token::Mul,
    '/' => Token::Div,
    '%' => Token::Mod,
    '(' => Token::LParen,
    ')' => Token::RParen,
    '[' => Token::LSqBracket,
    ']' => Token::RSqBracket,
    ',' => Token::Comma,
    '.' => Token::Period,
    ':' => Token::Colon,
};

static DOUBLE_CHAR_OPS: phf::Map<char, Token> = phf_map! {
    '=' => Token::Eq,
    '>' => Token::GreaterOrEq,
    '<' => Token::LessOrEq,
    '+' => Token::AddAssign,
    '-' => Token::SubAssign,
    '*' => Token::MulAssign,
    '/' => Token::DivAssign,
    '%' => Token::ModAssign,
};

#[derive(Clone, Copy)]
pub struct FilePos {
    pub line: u32,
    pub col: u32,
}

#[derive(Clone)]
pub struct LexerResult {
    pub token: Token,
    pub pos: FilePos,
    pub indent: u32,
}

pub struct Lexer<'a> {
    data: Chars<'a>,
    cur_char: Option<char>,
    line: u32,
    col: u32,
    indent: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        let mut result = Lexer {
            data: source.chars(),
            cur_char: None,
            line: 1,
            col: 1,
            indent: 0,
        };
        result.forward();
        result.indent = result.skip_spaces();
        result
    }

    pub fn next(&mut self) -> anyhow::Result<LexerResult> {
        self.skip_spaces();
        if self.cur_char == Some('#') {
            self.skip_comments();
        }

        let result = LexerResult {
            token: Token::Eof,
            pos: FilePos {
                line: self.line,
                col: self.col - 1,
            },
            indent: self.indent,
        };

        if self.cur_char == Some('\n') {
            while self.cur_char == Some('\n') {
                self.forward();
                self.indent = self.skip_spaces();
                if self.cur_char == Some('#') {
                    self.skip_comments();
                }
            }
            Ok(LexerResult {
                token: Token::NewLine,
                ..result
            })
        } else if let Some(is_char) = self.cur_char {
            // Names and keywords
            if is_char.is_ascii_alphabetic() {
                Ok(LexerResult {
                    token: self.read_name()?,
                    ..result
                })
            // Number literals (int and float)
            } else if is_char.is_ascii_digit() {
                Ok(LexerResult {
                    token: self.read_number()?,
                    ..result
                })
            // String literals
            } else if is_char == '"' {
                Ok(LexerResult {
                    token: Token::Str(self.read_str()?),
                    ..result
                })
            // Not equal operator (special case)
            } else if is_char == '!' {
                self.forward();
                if self.cur_char == Some('=') {
                    self.forward();
                    Ok(LexerResult {
                        token: Token::NotEq,
                        ..result
                    })
                } else {
                    anyhow::bail!(
                        "[Ln {}, Col {}] ERROR: Unexpected symbol {:?}",
                        result.pos.line,
                        result.pos.col,
                        is_char
                    );
                }
            // Operators
            } else if SINGLE_CHAR_OPS.contains_key(&is_char) {
                let first = is_char;
                self.forward();
                if self.cur_char == Some('=')
                    && let Some(token) = DOUBLE_CHAR_OPS.get(&first).cloned()
                {
                    self.forward();
                    Ok(LexerResult { token, ..result })
                } else {
                    Ok(LexerResult {
                        token: SINGLE_CHAR_OPS.get(&first).cloned().unwrap(),
                        ..result
                    })
                }
            } else {
                anyhow::bail!(
                    "[Ln {}, Col {}] ERROR: Unexpected symbol {:?}",
                    result.pos.line,
                    result.pos.col,
                    is_char
                );
            }
        } else {
            Ok(LexerResult {
                token: Token::Eof,
                ..result
            })
        }
    }

    fn forward(&mut self) {
        if self.cur_char == Some('\n') {
            self.line += 1;
            self.col = 2;
        } else {
            self.col += 1;
        }
        self.cur_char = self.data.next();
    }

    fn is_separator(&self) -> bool {
        if let Some(is_char) = self.cur_char {
            is_char.is_whitespace() || is_char == '!' || SINGLE_CHAR_OPS.contains_key(&is_char)
        } else {
            true
        }
    }

    fn expext_separator(&self) -> anyhow::Result<()> {
        if !self.is_separator() {
            anyhow::bail!(
                "[Ln {}, Col {}] ERROR: Expecting space, operator or end of line before {:?}",
                self.line,
                self.col - 1,
                self.cur_char.unwrap_or(' ')
            );
        }
        Ok(())
    }

    fn skip_spaces(&mut self) -> u32 {
        let mut count = 0;
        while let Some(is_char) = self.cur_char
            && is_char.is_whitespace()
            && is_char != '\n'
        {
            self.forward();
            count += 1;
        }
        count
    }

    fn skip_comments(&mut self) {
        self.forward();
        while self.cur_char.is_some() && self.cur_char != Some('\n') {
            self.forward();
        }
    }

    fn read_name(&mut self) -> anyhow::Result<Token> {
        let mut result = String::new();
        while let Some(is_char) = self.cur_char
            && (is_char.is_ascii_alphanumeric() || is_char == '_')
        {
            result.push(is_char);
            self.forward();
        }
        if let Some(token) = KEYWORDS.get(&result).cloned() {
            self.expext_separator()?;
            Ok(token)
        } else {
            self.expext_separator()?;
            Ok(Token::Name(result))
        }
    }

    fn read_int(&mut self) -> (i64, usize) {
        let mut result = String::new();
        while let Some(is_char) = self.cur_char
            && is_char.is_ascii_digit()
        {
            result.push(is_char);
            self.forward();
        }
        (result.parse().unwrap_or(0), result.len())
    }

    fn read_hex(&mut self) -> anyhow::Result<i64> {
        let mut result = String::new();
        while let Some(is_char) = self.cur_char
            && is_char.is_ascii_hexdigit()
        {
            result.push(is_char);
            self.forward();
        }
        self.expext_separator()?;
        i64::from_str_radix(&result, 16)
            .map_err(|_| anyhow::format_err!("[Ln {}, Col {}] ERROR: Can't parse HEX value", self.line, self.col - 1))
    }

    fn read_number(&mut self) -> anyhow::Result<Token> {
        if self.cur_char == Some('0') {
            self.forward();
            if self.cur_char == Some('x') || self.cur_char == Some('X') {
                self.forward();
                return Ok(Token::Int(self.read_hex()?));
            }
        }
        let integer = self.read_int().0;
        let mut decimal: Option<f64> = None;
        if self.cur_char == Some('.') {
            self.forward();
            let (dec_number, length) = self.read_int();
            decimal = Some(dec_number as f64 / 10f64.powi(length as i32));
        }
        if self.cur_char == Some('e') || self.cur_char == Some('E') {
            self.forward();
            let exponent = if self.cur_char == Some('-') {
                self.forward();
                -self.read_int().0
            } else {
                self.read_int().0
            };

            self.expext_separator()?;
            Ok(Token::Float(
                (integer as f64 + decimal.unwrap_or(0.0)) * 10f64.powi(exponent as i32),
            ))
        } else if let Some(dm_part) = decimal {
            self.expext_separator()?;
            Ok(Token::Float(integer as f64 + dm_part))
        } else {
            self.expext_separator()?;
            Ok(Token::Int(integer))
        }
    }

    fn read_str(&mut self) -> anyhow::Result<String> {
        self.forward();
        let mut escaping = false;
        let mut result = String::new();
        while let Some(is_char) = self.cur_char
            && !is_char.is_control()
        {
            if escaping {
                result.push(is_char);
                self.forward();
                escaping = false;
            } else {
                match is_char {
                    '"' => break,
                    '\\' => escaping = true,
                    _ => result.push(is_char),
                }
                self.forward();
            }
        }
        if self.cur_char == Some('"') {
            self.forward();
            self.expext_separator()?;
            Ok(result)
        } else if let Some(is_char) = self.cur_char
            && is_char != '\n'
            && is_char != '\r'
        {
            anyhow::bail!(
                "[Ln {}, Col {}] ERROR: Unexpected symbol {:?}",
                self.line,
                self.col - 1,
                is_char,
            );
        } else {
            anyhow::bail!(
                "[Ln {}, Col {}] ERROR: Unexpected end of string",
                self.line,
                self.col - 1
            );
        }
    }
}

fn print_token(token: Token, pos: FilePos, indent: u32) {
    println!("{:3} {:3} {:2}  {:?}", pos.line, pos.col, indent, token);
}

pub fn debug_dump(lexer: &mut Lexer) -> anyhow::Result<()> {
    let mut indent_stack = VecDeque::new();
    indent_stack.push_front(0u32);

    let mut last_pos = FilePos { col: 0, line: 0 };

    loop {
        let LexerResult { token, pos, indent } = lexer.next()?;
        if token == Token::Eof {
            last_pos = pos;
            break;
        }

        if indent > *indent_stack.front().unwrap() {
            indent_stack.push_front(indent);
            print_token(Token::Indent, pos, indent);
        } else {
            while indent < *indent_stack.front().unwrap() {
                print_token(Token::Dedent, pos, indent);
                indent_stack.pop_front();
            }
        }
        print_token(token, pos, indent);
    }

    while let Some(indent) = indent_stack.pop_front()
        && indent > 0
    {
        print_token(Token::Dedent, last_pos, indent);
    }

    Ok(())
}

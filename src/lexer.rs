use std::{char, collections::VecDeque, str::Chars};

use phf_macros::phf_map;

use crate::parser::Token;

static KEYWORDS: phf::Map<&'static str, Token> = phf_map! {
    "and" => Token::KwAnd,
    "const" => Token::KwConst,
    "elif" => Token::KwElif,
    "else" => Token::KwElse,
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
    "to" => Token::KwTo,
    "type" => Token::KwType,
    "var" => Token::KwVar,
    "while" => Token::KwWhile,
};

// =  >  <  +  -  *  /  %     ( ) [ ] , . :
// == >= <= += -= *= /= %= !=

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

    pub fn next(&mut self) -> Option<(Token, u32, u32, u32)> {
        self.skip_spaces();
        if self.cur_char == Some('#') {
            self.skip_comments();
        }

        let line = self.line;
        let col = self.col - 1;

        if self.cur_char == Some('\n') {
            let old_indent = self.indent;
            while self.cur_char == Some('\n') {
                self.forward();
                self.indent = self.skip_spaces();
                if self.cur_char == Some('#') {
                    self.skip_comments();
                }
            }
            Some((Token::NewLine, line, col, old_indent))
        } else if let Some(is_char) = self.cur_char {
            if is_char.is_ascii_alphabetic() {
                Some((self.read_name(), line, col, self.indent))
            } else if is_char.is_ascii_digit() {
                Some((self.read_number(), line, col, self.indent))
            } else if is_char == '!' {
                self.forward();
                if self.cur_char == Some('=') {
                    self.forward();
                    Some((Token::NotEq, line, col, self.indent))
                } else {
                    None
                }
            } else if SINGLE_CHAR_OPS.contains_key(&is_char) {
                let first = is_char;
                self.forward();
                if self.cur_char == Some('=')
                    && let Some(token) = DOUBLE_CHAR_OPS.get(&first).cloned()
                {
                    self.forward();
                    Some((token, line, col, self.indent))
                } else {
                    Some((SINGLE_CHAR_OPS.get(&first).cloned().unwrap(), line, col, self.indent))
                }
            } else {
                None
            }
        } else {
            None
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

    fn read_name(&mut self) -> Token {
        let mut result = String::new();
        while let Some(is_char) = self.cur_char
            && (is_char.is_ascii_alphanumeric() || is_char == '_')
        {
            result.push(is_char);
            self.forward();
        }
        if let Some(token) = KEYWORDS.get(&result).cloned() {
            token
        } else {
            Token::Name(result)
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

    fn read_hex(&mut self) -> i64 {
        0
    }

    fn read_number(&mut self) -> Token {
        if self.cur_char == Some('0') {
            self.forward();
            if self.cur_char == Some('x') || self.cur_char == Some('X') {
                self.forward();
                return Token::Int(self.read_hex());
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

            Token::Float((integer as f64 + decimal.unwrap_or(0.0)) * 10f64.powi(exponent as i32))
        } else if let Some(dm_part) = decimal {
            Token::Float(integer as f64 + dm_part)
        } else {
            Token::Int(integer)
        }
    }
}

fn print_token(token: Token, line: u32, col: u32, indent: u32) {
    println!("{:3} {:3} {:2}  {:?}", line, col, indent, token);
}

pub fn debug_dump(lexer: &mut Lexer) {
    let mut indent_stack = VecDeque::new();
    indent_stack.push_front(0u32);

    while let Some((token, line, col, indent)) = lexer.next() {
        if indent > *indent_stack.front().unwrap() {
            indent_stack.push_front(indent);
            print_token(Token::Indent, line, col, indent);
        } else {
            while indent < *indent_stack.front().unwrap() {
                print_token(Token::Dedent, line, col, indent);
                indent_stack.pop_front();
            }
        }
        print_token(token, line, col, indent);
    }
}

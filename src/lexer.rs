use std::{char, collections::VecDeque, str::Chars};

use crate::parser::Token;

fn get_keyword(name: &str, pos: FilePos) -> Option<Token> {
    match name {
        "and" => Some(Token::KwAnd(pos)),
        "bool" => Some(Token::KwBool(pos)),
        "byte" => Some(Token::KwByte(pos)),
        "const" => Some(Token::KwConst(pos)),
        "do" => Some(Token::KwDo(pos)),
        "elif" => Some(Token::KwElif(pos)),
        "else" => Some(Token::KwElse(pos)),
        "false" => Some(Token::KwFalse(pos)),
        "fixed" => Some(Token::KwFixed(pos)),
        "float" => Some(Token::KwFloat(pos)),
        "for" => Some(Token::KwFor(pos)),
        "func" => Some(Token::KwFunc(pos)),
        "if" => Some(Token::KwIf(pos)),
        "in" => Some(Token::KwIn(pos)),
        "int" => Some(Token::KwInt(pos)),
        "not" => Some(Token::KwNot(pos)),
        "or" => Some(Token::KwOr(pos)),
        "pass" => Some(Token::KwPass(pos)),
        "ref" => Some(Token::KwRef(pos)),
        "return" => Some(Token::KwReturn(pos)),
        "step" => Some(Token::KwStep(pos)),
        "string" => Some(Token::KwString(pos)),
        "struct" => Some(Token::KwStruct(pos)),
        "then" => Some(Token::KwThen(pos)),
        "true" => Some(Token::KwTrue(pos)),
        "to" => Some(Token::KwTo(pos)),
        "type" => Some(Token::KwType(pos)),
        "var" => Some(Token::KwVar(pos)),
        "while" => Some(Token::KwWhile(pos)),
        _ => None,
    }
}

fn get_single_char_op(letter: char, pos: FilePos) -> Option<Token> {
    match letter {
        '=' => Some(Token::Assign(pos)),
        '>' => Some(Token::Greater(pos)),
        '<' => Some(Token::Less(pos)),
        '+' => Some(Token::Add(pos)),
        '-' => Some(Token::Sub(pos)),
        '*' => Some(Token::Mul(pos)),
        '/' => Some(Token::Div(pos)),
        '%' => Some(Token::Mod(pos)),
        '(' => Some(Token::LParen(pos)),
        ')' => Some(Token::RParen(pos)),
        '[' => Some(Token::LSqBracket(pos)),
        ']' => Some(Token::RSqBracket(pos)),
        ',' => Some(Token::Comma(pos)),
        '.' => Some(Token::Period(pos)),
        ':' => Some(Token::Colon(pos)),
        _ => None,
    }
}

fn get_double_char_op(letter: char, pos: FilePos) -> Option<Token> {
    match letter {
        '=' => Some(Token::Eq(pos)),
        '>' => Some(Token::GreaterOrEq(pos)),
        '<' => Some(Token::LessOrEq(pos)),
        '+' => Some(Token::AddAssign(pos)),
        '-' => Some(Token::SubAssign(pos)),
        '*' => Some(Token::MulAssign(pos)),
        '/' => Some(Token::DivAssign(pos)),
        '%' => Some(Token::ModAssign(pos)),
        _ => None,
    }
}

#[derive(Clone, Copy, Default, Debug)]
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

        let pos = FilePos {
            line: self.line,
            col: self.col - 1,
        };
        let result = LexerResult {
            token: Token::Eof(pos),
            pos,
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
                token: Token::NewLine(pos),
                ..result
            })
        } else if let Some(is_char) = self.cur_char {
            // Names and keywords
            if is_char.is_ascii_alphabetic() {
                Ok(LexerResult {
                    token: self.read_name(pos)?,
                    ..result
                })
            // Number literals (int and float)
            } else if is_char.is_ascii_digit() {
                Ok(LexerResult {
                    token: self.read_number(pos)?,
                    ..result
                })
            // String literals
            } else if is_char == '"' {
                Ok(LexerResult {
                    token: Token::Str((pos, self.read_str()?)),
                    ..result
                })
            // Not equal operator (special case)
            } else if is_char == '!' {
                self.forward();
                if self.cur_char == Some('=') {
                    self.forward();
                    Ok(LexerResult {
                        token: Token::NotEq(pos),
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
            } else if get_single_char_op(is_char, FilePos::default()).is_some() {
                let first = is_char;
                self.forward();
                if self.cur_char == Some('=')
                    && let Some(token) = get_double_char_op(first, pos)
                {
                    self.forward();
                    Ok(LexerResult { token, ..result })
                } else {
                    Ok(LexerResult {
                        token: get_single_char_op(is_char, pos).unwrap(),
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
                token: Token::Eof(pos),
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
            is_char.is_whitespace() || is_char == '!' || get_single_char_op(is_char, FilePos::default()).is_some()
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

    fn read_name(&mut self, pos: FilePos) -> anyhow::Result<Token> {
        let mut result = String::new();
        while let Some(is_char) = self.cur_char
            && (is_char.is_ascii_alphanumeric() || is_char == '_')
        {
            result.push(is_char);
            self.forward();
        }
        if let Some(token) = get_keyword(&result, pos) {
            self.expext_separator()?;
            Ok(token)
        } else {
            self.expext_separator()?;
            Ok(Token::Name((pos, result)))
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

    fn read_number(&mut self, pos: FilePos) -> anyhow::Result<Token> {
        if self.cur_char == Some('0') {
            self.forward();
            if self.cur_char == Some('x') || self.cur_char == Some('X') {
                self.forward();
                return Ok(Token::Int((pos, self.read_hex()?)));
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
            Ok(Token::Float((
                pos,
                (integer as f64 + decimal.unwrap_or(0.0)) * 10f64.powi(exponent as i32),
            )))
        } else if let Some(dm_part) = decimal {
            self.expext_separator()?;
            Ok(Token::Float((pos, integer as f64 + dm_part)))
        } else {
            self.expext_separator()?;
            Ok(Token::Int((pos, integer)))
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

#[allow(dead_code)]
pub fn debug_dump(lexer: &mut Lexer) -> anyhow::Result<()> {
    let mut indent_stack = VecDeque::new();
    indent_stack.push_front(0u32);

    let last_pos;

    loop {
        let LexerResult { token, pos, indent } = lexer.next()?;
        match token {
            Token::Eof(eofpos) => {
                last_pos = eofpos;
                break;
            }
            _ => {}
        }

        if indent > *indent_stack.front().unwrap() {
            indent_stack.push_front(indent);
            print_token(Token::Indent(pos), pos, indent);
        } else {
            while indent < *indent_stack.front().unwrap() {
                print_token(Token::Dedent(pos), pos, indent);
                indent_stack.pop_front();
            }
        }
        print_token(token, pos, indent);
    }

    while let Some(indent) = indent_stack.pop_front()
        && indent > 0
    {
        print_token(Token::Dedent(last_pos), last_pos, indent);
    }

    Ok(())
}

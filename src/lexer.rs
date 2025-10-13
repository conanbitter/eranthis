use std::{char, collections::VecDeque, str::Chars};

use miette::{SourceOffset, SourceSpan};

use crate::{errors::LexicalError, parser::Token};

fn get_keyword(name: &str, pos: SourceOffset) -> Option<Token> {
    let span = SourceSpan::new(pos, name.len());
    match name {
        "and" => Some(Token::KwAnd(span)),
        "bool" => Some(Token::KwBool(span)),
        "byte" => Some(Token::KwByte(span)),
        "const" => Some(Token::KwConst(span)),
        "do" => Some(Token::KwDo(span)),
        "elif" => Some(Token::KwElif(span)),
        "else" => Some(Token::KwElse(span)),
        "false" => Some(Token::KwFalse(span)),
        "fixed" => Some(Token::KwFixed(span)),
        "float" => Some(Token::KwFloat(span)),
        "for" => Some(Token::KwFor(span)),
        "func" => Some(Token::KwFunc(span)),
        "if" => Some(Token::KwIf(span)),
        "in" => Some(Token::KwIn(span)),
        "int" => Some(Token::KwInt(span)),
        "not" => Some(Token::KwNot(span)),
        "or" => Some(Token::KwOr(span)),
        "pass" => Some(Token::KwPass(span)),
        "ref" => Some(Token::KwRef(span)),
        "return" => Some(Token::KwReturn(span)),
        "step" => Some(Token::KwStep(span)),
        "string" => Some(Token::KwString(span)),
        "struct" => Some(Token::KwStruct(span)),
        "then" => Some(Token::KwThen(span)),
        "true" => Some(Token::KwTrue(span)),
        "to" => Some(Token::KwTo(span)),
        "type" => Some(Token::KwType(span)),
        "var" => Some(Token::KwVar(span)),
        "while" => Some(Token::KwWhile(span)),
        _ => None,
    }
}

fn get_single_char_op(letter: char, pos: SourceOffset) -> Option<Token> {
    let span = SourceSpan::new(pos, 1);
    match letter {
        '=' => Some(Token::Assign(span)),
        '>' => Some(Token::Greater(span)),
        '<' => Some(Token::Less(span)),
        '+' => Some(Token::Add(span)),
        '-' => Some(Token::Sub(span)),
        '*' => Some(Token::Mul(span)),
        '/' => Some(Token::Div(span)),
        '%' => Some(Token::Mod(span)),
        '(' => Some(Token::LParen(span)),
        ')' => Some(Token::RParen(span)),
        '[' => Some(Token::LSqBracket(span)),
        ']' => Some(Token::RSqBracket(span)),
        ',' => Some(Token::Comma(span)),
        '.' => Some(Token::Period(span)),
        ':' => Some(Token::Colon(span)),
        _ => None,
    }
}

fn get_double_char_op(letter: char, pos: SourceOffset) -> Option<Token> {
    let span = SourceSpan::new(pos, 2);
    match letter {
        '=' => Some(Token::Eq(span)),
        '>' => Some(Token::GreaterOrEq(span)),
        '<' => Some(Token::LessOrEq(span)),
        '+' => Some(Token::AddAssign(span)),
        '-' => Some(Token::SubAssign(span)),
        '*' => Some(Token::MulAssign(span)),
        '/' => Some(Token::DivAssign(span)),
        '%' => Some(Token::ModAssign(span)),
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
    pub pos: SourceOffset,
    pub indent: u32,
}

pub struct Lexer<'a> {
    data: Chars<'a>,
    cur_char: Option<char>,
    line: u32,
    col: u32,
    indent: u32,
    offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        let mut result = Lexer {
            data: source.chars(),
            cur_char: None,
            line: 1,
            col: 1,
            indent: 0,
            offset: 0,
        };
        result.forward();
        result.indent = result.skip_spaces();
        result
    }

    pub fn next(&mut self) -> miette::Result<LexerResult> {
        self.skip_spaces();
        if self.cur_char == Some('#') {
            self.skip_comments();
        }

        let pos: SourceOffset = (self.offset - 1).into();
        let result = LexerResult {
            token: Token::Eof(pos.into()),
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
                token: Token::NewLine(pos.into()),
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
                let value = self.read_str()?;
                Ok(LexerResult {
                    token: Token::Str((SourceSpan::new(pos, value.len() + 2), value)),
                    ..result
                })
            // Not equal operator (special case)
            } else if is_char == '!' {
                self.forward();
                if self.cur_char == Some('=') {
                    self.forward();
                    Ok(LexerResult {
                        token: Token::NotEq(SourceSpan::new(pos, 2)),
                        ..result
                    })
                } else {
                    miette::bail!(LexicalError::UnexpectedSymbol { symbol: is_char, pos });
                }
            // Operators
            } else if get_single_char_op(is_char, 0.into()).is_some() {
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
                miette::bail!(LexicalError::UnexpectedSymbol { symbol: is_char, pos });
            }
        } else {
            Ok(LexerResult {
                token: Token::Eof(pos.into()),
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
        self.offset += 1;
        self.cur_char = self.data.next();
    }

    fn is_separator(&self) -> bool {
        if let Some(is_char) = self.cur_char {
            is_char.is_whitespace() || is_char == '!' || get_single_char_op(is_char, 0.into()).is_some()
        } else {
            true
        }
    }

    fn expext_separator(&self) -> miette::Result<()> {
        if !self.is_separator() {
            miette::bail!(LexicalError::ExpectingSeparator {
                symbol: self.cur_char.unwrap_or(' '),
                pos: (self.offset - 1).into()
            });
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

    fn read_name(&mut self, pos: SourceOffset) -> miette::Result<Token> {
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
            Ok(Token::Name((SourceSpan::new(pos, result.len()), result)))
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

    fn read_hex(&mut self) -> miette::Result<i64> {
        let start = SourceOffset::from(self.offset - 3);
        let mut result = String::new();
        while let Some(is_char) = self.cur_char
            && is_char.is_ascii_hexdigit()
        {
            result.push(is_char);
            self.forward();
        }
        self.expext_separator()?; //anyhow::format_err!("[Ln {}, Col {}] ERROR: Can't parse HEX value", self.line, self.col - 1)
        let result = i64::from_str_radix(&result, 16).map_err(|_| LexicalError::HexParse {
            pos: SourceSpan::new(start, result.len() + 2),
        })?;
        Ok(result)
    }

    fn read_number(&mut self, pos: SourceOffset) -> miette::Result<Token> {
        let start = self.offset;
        if self.cur_char == Some('0') {
            self.forward();
            if self.cur_char == Some('x') || self.cur_char == Some('X') {
                self.forward();
                let result = self.read_hex()?;
                return Ok(Token::Int((SourceSpan::new(pos, self.offset - start), result)));
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
                SourceSpan::new(pos, self.offset - start),
                (integer as f64 + decimal.unwrap_or(0.0)) * 10f64.powi(exponent as i32),
            )))
        } else if let Some(dm_part) = decimal {
            self.expext_separator()?;
            Ok(Token::Float((
                SourceSpan::new(pos, self.offset - start),
                integer as f64 + dm_part,
            )))
        } else {
            self.expext_separator()?;
            Ok(Token::Int((SourceSpan::new(pos, self.offset - start), integer)))
        }
    }

    fn read_str(&mut self) -> miette::Result<String> {
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
            miette::bail!(LexicalError::UnexpectedSymbol {
                symbol: is_char,
                pos: (self.offset - 1).into()
            });
        } else {
            miette::bail!(LexicalError::UnexpectedEos {
                pos: (self.offset - 1).into()
            });
        }
    }
}

pub fn get_overall_span(from: SourceSpan, to: SourceSpan) -> SourceSpan {
    let length = to.offset() + to.len() - from.offset();
    SourceSpan::new(from.offset().into(), length)
}

fn print_token(token: Token, pos: SourceOffset, indent: u32) {
    println!("{:5} {:2}  {:?}", pos.offset(), indent, token);
}

#[allow(dead_code)]
pub fn debug_dump(lexer: &mut Lexer) -> miette::Result<()> {
    let mut indent_stack = VecDeque::new();
    indent_stack.push_front(0u32);

    let last_pos;

    loop {
        let LexerResult { token, pos, indent } = lexer.next()?;
        if let Token::Eof(eofpos) = token {
            last_pos = eofpos;
            break;
        }

        if indent > *indent_stack.front().unwrap() {
            indent_stack.push_front(indent);
            print_token(Token::Indent(SourceSpan::from(pos)), pos, indent);
        } else {
            while indent < *indent_stack.front().unwrap() {
                print_token(Token::Dedent(SourceSpan::from(pos)), pos, indent);
                indent_stack.pop_front();
            }
        }
        print_token(token, pos, indent);
    }

    while let Some(indent) = indent_stack.pop_front()
        && indent > 0
    {
        print_token(Token::Dedent(last_pos), last_pos.offset().into(), indent);
    }

    Ok(())
}

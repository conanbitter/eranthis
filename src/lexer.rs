use std::str::Chars;

use crate::parser::Token;

pub struct Lexer<'a> {
    data: Chars<'a>,
    cur_char: char,
    eof: bool,
    line: u32,
    col: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        let mut result = Lexer {
            data: source.chars(),
            cur_char: '\0',
            eof: false,
            line: 1,
            col: 1,
        };
        result.forward();
        result
    }

    pub fn next(&mut self) -> Option<(Token, u32, u32)> {
        self.skip_spaces();
        if self.eof {
            return None;
        }
        while self.cur_char == '#' {
            self.skip_comments();
            self.skip_spaces();
            if self.eof {
                return None;
            }
        }

        let line = self.line;
        let col = self.col - 1;

        match self.cur_char {
            _ => {
                if self.cur_char.is_ascii_alphabetic() {
                    let name = self.read_name();
                    if name == "test" {
                        Some((Token::KwTest, line, col))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    fn forward(&mut self) {
        if let Some(next_char) = self.data.next() {
            self.cur_char = next_char;
            if next_char == '\n' {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
        } else {
            self.eof = true;
        }
    }

    fn skip_spaces(&mut self) {
        while !self.eof && self.cur_char.is_whitespace() {
            self.forward();
        }
    }

    fn skip_comments(&mut self) {
        while !self.eof && self.cur_char != '\n' {
            self.forward();
        }
        self.forward();
    }

    fn read_name(&mut self) -> String {
        let mut result = String::new();
        while !self.eof && (self.cur_char.is_ascii_alphanumeric() || self.cur_char == '_') {
            result.push(self.cur_char);
            self.forward();
        }
        result
    }
}

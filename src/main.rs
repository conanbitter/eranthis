use std::{collections::VecDeque, fs, path::Path};

use crate::{
    ast::Node,
    lexer::{FilePos, Lexer, LexerResult},
    parser::{Parser, Token},
};

mod ast;
mod lexer;
mod parser;

fn parse_file<P: AsRef<Path>>(source_file: P) -> anyhow::Result<Node> {
    let source = fs::read_to_string(source_file)?;

    let mut lex = Lexer::new(&source);
    let mut par = Parser::new(FilePos { col: 0, line: 0 });

    let mut indent_stack = VecDeque::new();
    indent_stack.push_front(0u32);
    let mut new_line = false;
    loop {
        let LexerResult { token, pos, indent } = lex.next()?;
        if token == Token::Eof {
            break;
        }

        *par.extra_mut() = pos;

        if indent > *indent_stack.front().unwrap() {
            indent_stack.push_front(indent);
            par.parse(Token::Indent)?;
        } else {
            while indent < *indent_stack.front().unwrap() {
                par.parse(Token::Dedent)?;
                indent_stack.pop_front();
            }
        }
        new_line = token == Token::NewLine;
        par.parse(token)?;
    }
    if !new_line {
        par.parse(Token::NewLine)?
    }
    Ok(par.end_of_input().unwrap().0)

    //lexer::debug_dump(&mut lex)?;
}

fn main() -> anyhow::Result<()> {
    let root = parse_file("test2.txt")?;
    println!("{:?}", root);
    Ok(())
}

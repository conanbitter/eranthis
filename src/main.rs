use std::{collections::VecDeque, fs, path::Path};

use crate::{
    ast::Node,
    lexer::{Lexer, LexerResult},
    parser::{Parser, Token},
};

mod ast;
mod lexer;
mod parser;

fn parse_file<P: AsRef<Path>>(source_file: P) -> anyhow::Result<Node> {
    let source = fs::read_to_string(source_file)?;

    let mut lex = Lexer::new(&source);
    let mut par = Parser::new();

    let mut indent_stack = VecDeque::new();
    indent_stack.push_front(0u32);
    loop {
        let LexerResult { token, pos, indent } = lex.next()?;
        if token == Token::Eof {
            break;
        }

        if indent > *indent_stack.front().unwrap() {
            indent_stack.push_front(indent);
            par.parse(Token::Indent).unwrap();
        } else {
            while indent < *indent_stack.front().unwrap() {
                par.parse(Token::Dedent).unwrap();
                indent_stack.pop_front();
            }
        }

        par.parse(token).unwrap();
    }

    //lexer::debug_dump(&mut lex)?;
    Ok(par.end_of_input().unwrap())
}

fn main() -> anyhow::Result<()> {
    let root = parse_file("test2.txt")?;
    println!("{:?}", root);
    Ok(())
}

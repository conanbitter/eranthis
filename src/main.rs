use std::{collections::VecDeque, fs, path::Path};

use crate::{lexer::Lexer, parser::Token};

mod ast;
mod lexer;
mod parser;

fn parse_file<P: AsRef<Path>>(source_file: P) -> anyhow::Result<()> {
    let source = fs::read_to_string(source_file)?;

    let mut lex = Lexer::new(&source);
    let mut indent_stack = VecDeque::new();
    indent_stack.push_front(0u32);

    while let Some((token, line, col, indent)) = lex.next() {
        if indent > *indent_stack.front().unwrap() {
            indent_stack.push_front(indent);
            println!("{:3} {:3} {:2}  {:?}", line, col, indent, Token::Indent);
        } else {
            while indent < *indent_stack.front().unwrap() {
                println!("{:3} {:3} {:2}  {:?}", line, col, indent, Token::Dedent);
                indent_stack.pop_front();
            }
        }
        println!("{:3} {:3} {:2}  {:?}", line, col, indent, token);
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
    parse_file("test.txt")?;
    Ok(())
}

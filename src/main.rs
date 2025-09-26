use std::{fs, path::Path};

use crate::lexer::Lexer;

mod ast;
mod lexer;
mod parser;

fn parse_file<P: AsRef<Path>>(source_file: P) -> anyhow::Result<()> {
    let source = fs::read_to_string(source_file)?;

    let mut lex = Lexer::new(&source);

    while let Some((token, line, col)) = lex.next() {
        println!("{}:{} {:?}", line, col, token);
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
    parse_file("test.txt")?;
    Ok(())
}

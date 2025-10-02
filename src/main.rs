use std::{collections::VecDeque, fs, path::Path};

use crate::{
    ast::Node,
    bytecode::FrameAllocator,
    lexer::{FilePos, Lexer, LexerResult},
    parser::{Parser, Token},
};

mod ast;
mod bytecode;
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
        par.parse(Token::NewLine)?;
    }

    while let Some(indent) = indent_stack.pop_front()
        && indent > 0
    {
        par.parse(Token::Dedent)?;
    }

    Ok(par.end_of_input()?.0)
    //lexer::debug_dump(&mut lex)?;
    //Ok(Node::Dummy)
}

fn main() -> anyhow::Result<()> {
    let root = parse_file("test2.txt")?;
    ast::debug_dump(&root, "test2_result.txt")?;
    //println!("{:?}", root);
    /*let mut frame = FrameAllocator::new();
    frame.alloc(10); // 1
    let f1 = frame.alloc(20); // 2
    frame.alloc(10); // 3
    let f2 = frame.alloc(10); // 4
    frame.alloc(10); // 5
    frame.free(f1); // 6
    frame.free(f2); // 7
    frame.alloc(10); // 8
    frame.alloc(20); // 9

    frame.calculate();

    println!("Frame size {}", frame.get_frame_size());
    for i in 0..7 {
        println!("Offset {} = {}", i, frame.get_offset(i));
    }*/

    Ok(())
}

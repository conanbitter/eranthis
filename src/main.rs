use std::{collections::VecDeque, fs, path::Path};

use crate::{
    ast::{CodeNodeData, ExprNode, ExprNodeData, ModNode, ModNodeData},
    bytecode::FrameAllocator,
    lexer::{FilePos, Lexer, LexerResult},
    parser::{Parser, Token},
};

mod ast;
mod bytecode;
mod lexer;
mod parser;
mod semantic;

fn parse_file<P: AsRef<Path>>(source_file: P) -> anyhow::Result<Vec<ModNode>> {
    let source = fs::read_to_string(source_file)?;

    let mut lex = Lexer::new(&source);
    let mut par = Parser::new(FilePos { col: 0, line: 0 });

    let mut indent_stack = VecDeque::new();
    indent_stack.push_front(0u32);
    let mut new_line = false;
    let last_pos: FilePos;
    loop {
        let LexerResult { token, pos, indent } = lex.next()?;
        if let Token::Eof(eofpos) = token {
            last_pos = eofpos;
            break;
        }

        *par.extra_mut() = pos;

        if indent > *indent_stack.front().unwrap() {
            indent_stack.push_front(indent);
            par.parse(Token::Indent(pos))?;
        } else {
            while indent < *indent_stack.front().unwrap() {
                par.parse(Token::Dedent(pos))?;
                indent_stack.pop_front();
            }
        }

        new_line = matches!(token, Token::NewLine(_));
        par.parse(token)?;
    }

    if !new_line {
        par.parse(Token::NewLine(last_pos))?;
    }

    while let Some(indent) = indent_stack.pop_front()
        && indent > 0
    {
        par.parse(Token::Dedent(last_pos))?;
    }

    Ok(par.end_of_input()?.0)
    //lexer::debug_dump(&mut lex)?;
    //Ok(Node::Dummy)
}

fn collapse_consts(node: &mut ExprNode) {
    if let ExprNodeData::BinOp(_, left, right) = &mut node.data {
        collapse_consts(left);
        collapse_consts(right);
        if let ExprNodeData::IntLiteral(left_part) = left.data
            && let ExprNodeData::IntLiteral(right_part) = right.data
        {
            *node = ExprNode {
                datatype: ast::ExprType::IntLiteral,
                data: ExprNodeData::IntLiteral(left_part + right_part),
                pos: node.pos,
            };
        }
    }
}

fn opt_test(root: &mut Vec<ModNode>) {
    for ModNode { data: item, .. } in root {
        if let ModNodeData::FuncDecl(_, _, _, block) = item {
            for stmt in &mut block.stmts {
                if let CodeNodeData::Assign(_, expr) = &mut stmt.data {
                    collapse_consts(expr);
                }
            }
        }
    }
}

fn main() -> anyhow::Result<()> {
    let mut root = parse_file("test2.txt")?;
    opt_test(&mut root);
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

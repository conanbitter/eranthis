use std::{collections::VecDeque, fs, path::Path};

use anyhow::anyhow;
use miette::IntoDiagnostic;

use crate::{
    ast::{CodeNodeData, ExprNode, ExprNodeData, ModNode, ModNodeData},
    bytecode::FrameAllocator,
    fixedpoint::FixedPoint,
    lexer::{FilePos, Lexer, LexerResult},
    parser::{Parser, Token},
    semantic::Module,
};

mod ast;
mod bytecode;
mod errors;
mod fixedpoint;
mod lexer;
mod parser;
mod semantic;

fn parse_string(source: &String) -> miette::Result<Vec<ModNode>> {
    let mut lex = Lexer::new(source);
    //lexer::debug_dump(&mut lex)?;
    //return Ok(vec![]);
    let mut par = Parser::new(0.into());

    let mut indent_stack = VecDeque::new();
    indent_stack.push_front(0u32);
    let mut new_line = false;
    let last_pos;
    loop {
        let LexerResult { token, pos, indent } = lex.next()?;
        if let Token::Eof(eofpos) = token {
            last_pos = eofpos;
            break;
        }

        *par.extra_mut() = pos;

        if indent > *indent_stack.front().unwrap() {
            indent_stack.push_front(indent);
            par.parse(Token::Indent(pos.into()))?;
        } else {
            while indent < *indent_stack.front().unwrap() {
                par.parse(Token::Dedent(pos.into()))?;
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

fn parse_file<P: AsRef<Path>>(source_file: P) -> miette::Result<Vec<ModNode>> {
    let source = fs::read_to_string(source_file).into_diagnostic()?;
    parse_string(&source).map_err(|error| error.with_source_code(source))
}

fn collapse_consts(node: &mut ExprNode) {
    if let ExprNodeData::BinOp { left, right, .. } = &mut node.data {
        collapse_consts(left);
        collapse_consts(right);
        if let ExprNodeData::IntLiteral(left_part) = left.data
            && let ExprNodeData::IntLiteral(right_part) = right.data
        {
            *node = ExprNode {
                datatype: ast::ExprType::IntLiteral,
                data: ExprNodeData::IntLiteral(left_part + right_part),
                primary_span: node.primary_span,
                overall_span: node.overall_span,
            };
        }
    }
}

fn opt_test(root: &mut Vec<ModNode>) {
    for ModNode { data: item, .. } in root {
        if let ModNodeData::FuncDecl { code, .. } = item {
            for stmt in &mut code.stmts {
                if let CodeNodeData::Assign { value, .. } = &mut stmt.data {
                    collapse_consts(value);
                }
            }
        }
    }
}

fn main() -> miette::Result<()> {
    let mut root = parse_file("test2.txt")?;
    //opt_test(&mut root);
    ast::debug_dump(&root, "test2_result.txt").unwrap();
    //let mut sem = Module::new();
    //sem.collect_constants(&mut root)?;
    /*
    let a = -1.4;
    let b = FixedPoint::from(a);
    println!("a={}  b={} ({1:?}), float={}", a, b, f64::from(b));

    let c = -3i32;
    let b = FixedPoint::from(c);
    println!("c={}  b={} ({1:?}), int={}", c, b, f64::from(b));

    println!("0.25 x 2 = {}", FixedPoint::from(0.25) * FixedPoint::from(2));
    println!("0.5 / 2 = {}", FixedPoint::from(0.5) / FixedPoint::from(2));
    println!("0.25 + 2 = {}", FixedPoint::from(0.25) + FixedPoint::from(2));
    println!("1.5 - 0.25 = {}", FixedPoint::from(1.5) - FixedPoint::from(0.25));
    println!("-0.5 = {}", -FixedPoint::from(0.5));
    println!("-1.2 = {}", -FixedPoint::from(1.2));

    let mut d = FixedPoint::from(0);
    let inc = FixedPoint::from(1) / FixedPoint::from(32);
    println!("d= {}  inc={}", d, inc);
    for _ in 0..32 {
        d = d + inc;
        println!("d= {}", d,);
    }
    */
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

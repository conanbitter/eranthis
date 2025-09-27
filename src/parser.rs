use pomelo::pomelo;

pomelo! {
     %include {
        use crate::ast::*;
        use crate::lexer::FilePos;
    }

    %token #[derive(Clone,Debug,PartialEq)] pub enum Token {};
    %extra_argument FilePos;
    %error anyhow::Error;

    %syntax_error {
        let expected_list = expected
            .map(|x| if let Some(sometoken) = x.token{
                    format!(" {:?}", sometoken)
                }else{
                    format!(" {}", x.name)
                })
            .collect::<Vec<String>>()
            .join(", ");

        if let Some(sometoken) = token{
            Err(anyhow::anyhow!("[Ln {}, Col {}] ERROR: got {:?}, expecting {}", extra.line, extra.col, sometoken, expected_list))
        }else{
            Err(anyhow::anyhow!("[Ln {}, Col {}] ERROR: expecting {}", extra.line, extra.col, expected_list))
        }
    }

    %parse_fail {
        anyhow::anyhow!("[Ln {}, Col {}] ERROR: total parser fail", extra.line, extra.col)
    }

    %stack_overflow {
        anyhow::anyhow!("[Ln {}, Col {}] ERROR: parser stack overflow", extra.line, extra.col)
    }


    %type Name String;
    %type Str String;
    %type Int i64;
    %type Float f64;
    %type Eof;

    %type root Node;
    %type stmt_list Vec<Node>;
    %type stmt Node;
    %type expr Node;
    %type var String;


    %left KwOr;
    %left KwAnd;
    %nonassoc Eq NotEq;
    %nonassoc Less LessOrEq Greater GreaterOrEq;
    %left Add Sub;
    %left Mul Div Mod;
    %right KwNot;

    root ::= stmt_list(sl) { Node::DummyVec(sl) };

    stmt_list ::= stmt_list(mut sl) stmt(s) { sl.push(s); sl };
    stmt_list ::= stmt(s) { vec![s] };

    stmt ::= expr NewLine;

    expr ::= Int(v)   { Node::IntLiteral(v) };
    expr ::= Float(v) { Node::FloatLiteral(v) };
    expr ::= var(v)   { Node::Var(v) };
    expr ::= LParen expr RParen;
    expr ::= expr(l) Add         expr(r) { Node::BinOp( BinOp::Add,      Box::new(l), Box::new(r) ) };
    expr ::= expr(l) Sub         expr(r) { Node::BinOp( BinOp::Sub,      Box::new(l), Box::new(r) ) };
    expr ::= expr(l) Mul         expr(r) { Node::BinOp( BinOp::Mul,      Box::new(l), Box::new(r) ) };
    expr ::= expr(l) Div         expr(r) { Node::BinOp( BinOp::Div,      Box::new(l), Box::new(r) ) };
    expr ::= expr(l) Mod         expr(r) { Node::BinOp( BinOp::Mod,      Box::new(l), Box::new(r) ) };
    expr ::= expr(l) Less        expr(r) { Node::BinOp( BinOp::Less,     Box::new(l), Box::new(r) ) };
    expr ::= expr(l) LessOrEq    expr(r) { Node::BinOp( BinOp::LessEq,   Box::new(l), Box::new(r) ) };
    expr ::= expr(l) Greater     expr(r) { Node::BinOp( BinOp::Greater,  Box::new(l), Box::new(r) ) };
    expr ::= expr(l) GreaterOrEq expr(r) { Node::BinOp( BinOp::GreaterEq,Box::new(l), Box::new(r) ) };
    expr ::= expr(l) Eq          expr(r) { Node::BinOp( BinOp::Eq,       Box::new(l), Box::new(r) ) };
    expr ::= expr(l) NotEq       expr(r) { Node::BinOp( BinOp::NotEq,    Box::new(l), Box::new(r) ) };
    expr ::= expr(l) KwAnd       expr(r) { Node::BinOp( BinOp::And,      Box::new(l), Box::new(r) ) };
    expr ::= expr(l) KwOr        expr(r) { Node::BinOp( BinOp::Or,       Box::new(l), Box::new(r) ) };
    expr ::= KwNot expr(e)       { Node::UnOp( UnOp::Not, Box::new(e) ) };
    expr ::= Sub expr(e) [KwNot] { Node::UnOp( UnOp::Neg, Box::new(e) ) };

    var ::= Name;

    root ::= NewLine { Node::Dummy };
    root ::= Indent { Node::Dummy };
    root ::= Dedent { Node::Dummy };
    root ::= Name { Node::Dummy };
    root ::= Str { Node::Dummy };

    //root ::= KwAnd { Node::Dummy };
    root ::= KwConst { Node::Dummy };
    root ::= KwElif { Node::Dummy };
    root ::= KwElse { Node::Dummy };
    root ::= KwFor { Node::Dummy };
    root ::= KwFunc { Node::Dummy };
    root ::= KwIf { Node::Dummy };
    root ::= KwIn { Node::Dummy };
    root ::= KwNot { Node::Dummy };
    //root ::= KwOr { Node::Dummy };
    root ::= KwRef { Node::Dummy };
    root ::= KwReturn { Node::Dummy };
    root ::= KwStep { Node::Dummy };
    root ::= KwStruct { Node::Dummy };
    root ::= KwThen { Node::Dummy };
    root ::= KwTo { Node::Dummy };
    root ::= KwType { Node::Dummy };
    root ::= KwVar { Node::Dummy };
    root ::= KwWhile { Node::Dummy };

    root ::= Assign { Node::Dummy };
    //root ::= Add { Node::Dummy };
    root ::= AddAssign { Node::Dummy };
    //root ::= Sub { Node::Dummy };
    root ::= SubAssign { Node::Dummy };
    //root ::= Mul { Node::Dummy };
    root ::= MulAssign { Node::Dummy };
    //root ::= Div { Node::Dummy };
    root ::= DivAssign { Node::Dummy };
    //root ::= Mod { Node::Dummy };
    root ::= ModAssign { Node::Dummy };
    //root ::= Less { Node::Dummy };
    //root ::= LessOrEq { Node::Dummy };
    //root ::= Greater { Node::Dummy };
    //root ::= GreaterOrEq { Node::Dummy };
    //root ::= Eq { Node::Dummy };
    //root ::= NotEq { Node::Dummy };
    root ::= Comma { Node::Dummy };
    root ::= Period { Node::Dummy };
    root ::= Colon { Node::Dummy };
    root ::= LParen { Node::Dummy };
    root ::= RParen { Node::Dummy };
    root ::= LSqBracket { Node::Dummy };
    root ::= RSqBracket { Node::Dummy };
}

pub use parser::Parser;
pub use parser::Token;

use std::fmt::Display;

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
                    format!(" {}", sometoken)
                }else{
                    format!(" {}", x.name)
                })
            .collect::<Vec<String>>()
            .join(", ");

        if let Some(sometoken) = token{
            Err(anyhow::anyhow!("[Ln {}, Col {}] ERROR: got {}, expecting {}", extra.line, extra.col, sometoken, expected_list))
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
    %type var Vec<String>;
    %type fncall Node;
    %type param_list Vec<Node>;
    %type assign Node;
    %type block Vec<Node>;
    %type ifstmt Node;
    %type else_branch Vec<Node>;
    %type elif_branch (Node, Vec<Node>);
    %type elif_list Vec<(Node, Vec<Node>)>;
    %type boolval bool;
    %type forstmt Node;
    %type step_variant Box<Node>;
    %type opassign BinOp;
    %type vardecl Node;
    %type single_vardecl (String, DataType, Option<Node>);
    %type basic_type DataType;
    %type opt_assign Node;
    %type vardecl_list Vec<(String, DataType, Option<Node>)>;
    %type constdecl Node;
    %type single_constdecl (String, DataType, Node);
    %type constdecl_list Vec<(String, DataType, Node)>;
    %type type_convert Node;
    %type returnstmt Node;
    %type whilestmt Node;
    %type subscript Node;


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

    stmt ::= assign NewLine;
    stmt ::= fncall NewLine;
    stmt ::= returnstmt NewLine;
    stmt ::= ifstmt;
    stmt ::= forstmt;
    stmt ::= vardecl;
    stmt ::= constdecl; // temporary
    stmt ::= whilestmt;

    assign ::= var(v) Assign expr(e) { Node::Assign(v, Box::new(e)) };
    assign ::= var(v) opassign(o) expr(e) { Node::OpAssign(v, o, Box::new(e)) };
    opassign ::= AddAssign { BinOp::Add };
    opassign ::= SubAssign { BinOp::Sub };
    opassign ::= MulAssign { BinOp::Mul };
    opassign ::= DivAssign { BinOp::Div };
    opassign ::= ModAssign { BinOp::Mod };

    ifstmt ::= KwIf expr(e) KwThen expr(te) NewLine { Node::If(Box::new(e), vec![te], vec![], vec![]) };
    ifstmt ::= KwIf expr(e) NewLine block(b) elif_list?(el) else_branch?(eb) { Node::If(Box::new(e), b, el.unwrap_or(vec![]), eb.unwrap_or(vec![])) };
    else_branch ::= KwElse NewLine block;
    elif_branch ::= KwElif expr(e) NewLine block(b) { (e, b) };
    elif_list ::= elif_list(mut el) elif_branch(eb) { el.push(eb); el };
    elif_list ::= elif_branch(el) { vec![el] };

    forstmt ::= KwFor var(v) Assign expr(es) KwTo expr(ef) step_variant?(s) NewLine block(b) { Node::For(v,Box::new(es), Box::new(ef), s, b) };
    step_variant ::= KwStep expr(e) { Box::new(e) };
    forstmt ::= KwFor var(v) KwIn expr(ea) NewLine block(b) { Node::ForIn(v,Box::new(ea), b) };

    vardecl ::= KwVar single_vardecl(sv) NewLine { Node::VarDecl(vec![sv]) };
    vardecl ::= KwVar NewLine Indent vardecl_list(dl) NewLine Dedent { Node::VarDecl(dl) };
    single_vardecl ::= Name(n) basic_type(t) opt_assign?(a) { (n, t, a) };
    opt_assign ::= Assign expr;
    vardecl_list ::= vardecl_list(mut dl) NewLine single_vardecl(d) { dl.push(d); dl };
    vardecl_list ::= single_vardecl(d) { vec![d] };

    constdecl ::= KwConst single_constdecl(sc) NewLine { Node::ConstDecl(vec![sc]) };
    constdecl ::= KwConst NewLine Indent constdecl_list(dl) NewLine Dedent { Node::ConstDecl(dl) };
    single_constdecl ::= Name(n) basic_type(t) Assign expr(e) { (n, t, e) };
    constdecl_list ::= constdecl_list(mut dl) NewLine single_constdecl(d) { dl.push(d); dl };
    constdecl_list ::= single_constdecl(d) { vec![d] };

    returnstmt ::= KwReturn expr(e) { Node::Return(Box::new(e)) };

    whilestmt ::= KwWhile expr(e) NewLine block(b) { Node::While(Box::new(e), b) };

    expr ::= Int(v)     { Node::IntLiteral(v) };
    expr ::= Float(v)   { Node::FloatLiteral(v) };
    expr ::= Str(v)     { Node::StringLiteral(v) };
    expr ::= boolval(v) { Node::BoolLiteral(v)};
    expr ::= var(v)     { Node::Var(v) };
    expr ::= LParen expr RParen;
    expr ::= fncall;
    expr ::= type_convert;
    expr ::= subscript;
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

    boolval ::= KwTrue  { true };
    boolval ::= KwFalse { false };

    basic_type ::= KwByte { DataType::Byte };
    basic_type ::= KwInt { DataType::Int };
    basic_type ::= KwFloat { DataType::Float };
    basic_type ::= KwFixed { DataType::Fixed };
    basic_type ::= KwString { DataType::String };
    basic_type ::= KwBool { DataType::Bool };

    var ::= var(mut v) Period Name(n) { v.push(n); v };
    var ::= Name(n) { vec![n] };

    fncall ::= var(v) LParen param_list(pl) RParen { Node::FnCall(v, pl) };

    param_list ::= param_list(mut pl) Comma expr(e) { pl.push(e); pl };
    param_list ::= expr(e) { vec![e] };
    param_list ::= { vec![] };

    type_convert ::= basic_type(t) LParen expr(e) RParen { Node::TypeConvert(Box::new(e), t) };

    subscript ::= var(v) LSqBracket expr(e) RSqBracket { Node::Subscript(v, Box::new(e)) };

    block ::= Indent stmt_list Dedent;
    block ::= Indent KwPass NewLine Dedent { vec![] };

    // Reserved tokens

    // Reserved for function definition
    root ::= KwFunc { Node::Dummy };
    root ::= KwRef { Node::Dummy };

    // Reserved for struct definition
    root ::= KwStruct { Node::Dummy };

    // Reserved for type aliases
    root ::= KwType { Node::Dummy };

    // Reserved for static members
    root ::= Colon { Node::Dummy };
}

pub use parser::Parser;
pub use parser::Token;

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Name(name) => write!(f, "name '{}'", name),
            Token::Str(st) => write!(f, "string literal '{}'", st),
            Token::Int(v) => write!(f, "int literal ({})", v),
            Token::Float(v) => write!(f, "float literal ({})", v),
            Token::Eof => write!(f, "end of file"),
            Token::KwOr => write!(f, "'or'"),
            Token::KwAnd => write!(f, "'and'"),
            Token::Eq => write!(f, "'=='"),
            Token::NotEq => write!(f, "'!='"),
            Token::Less => write!(f, "'<'"),
            Token::LessOrEq => write!(f, "'<='"),
            Token::Greater => write!(f, "'>'"),
            Token::GreaterOrEq => write!(f, "'>='"),
            Token::Add => write!(f, "'+'"),
            Token::Sub => write!(f, "'-'"),
            Token::Mul => write!(f, "'*'"),
            Token::Div => write!(f, "'/'"),
            Token::Mod => write!(f, "'%'"),
            Token::KwNot => write!(f, "'not'"),
            Token::NewLine => write!(f, "new line"),
            Token::Assign => write!(f, "'='"),
            Token::AddAssign => write!(f, "'+='"),
            Token::SubAssign => write!(f, "'-='"),
            Token::MulAssign => write!(f, "'*='"),
            Token::DivAssign => write!(f, "'/='"),
            Token::ModAssign => write!(f, "'%='"),
            Token::KwIf => write!(f, "'if'"),
            Token::KwThen => write!(f, "'then'"),
            Token::KwElse => write!(f, "'else'"),
            Token::KwElif => write!(f, "'elif'"),
            Token::KwFor => write!(f, "'for'"),
            Token::KwTo => write!(f, "'to'"),
            Token::KwStep => write!(f, "'step'"),
            Token::KwIn => write!(f, "'in'"),
            Token::KwVar => write!(f, "'var'"),
            Token::Indent => write!(f, "indent"),
            Token::Dedent => write!(f, "dedent"),
            Token::KwConst => write!(f, "'const'"),
            Token::LParen => write!(f, "'('"),
            Token::RParen => write!(f, "')'"),
            Token::KwTrue => write!(f, "'true'"),
            Token::KwFalse => write!(f, "'false'"),
            Token::KwByte => write!(f, "'byte'"),
            Token::KwInt => write!(f, "'int'"),
            Token::KwFloat => write!(f, "'float'"),
            Token::KwFixed => write!(f, "'fixed'"),
            Token::KwString => write!(f, "'string'"),
            Token::KwBool => write!(f, "'bool'"),
            Token::Period => write!(f, "'.'"),
            Token::Comma => write!(f, "','"),
            Token::KwPass => write!(f, "'pass'"),
            Token::KwFunc => write!(f, "'func'"),
            Token::KwRef => write!(f, "'ref'"),
            Token::KwReturn => write!(f, "'return'"),
            Token::KwStruct => write!(f, "'struct'"),
            Token::KwType => write!(f, "'type'"),
            Token::KwWhile => write!(f, "'while'"),
            Token::Colon => write!(f, "':'"),
            Token::LSqBracket => write!(f, "'['"),
            Token::RSqBracket => write!(f, "']'"),
        }
    }
}

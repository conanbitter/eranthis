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

    %type arg_list Vec<ExprNode>;
    %type assign CodeNode;
    %type basic_type DataType;
    %type block Vec<CodeNode>;
    %type boolval bool;
    %type constdecl ModNode;
    %type constdecl_list Vec<(String, DataType, ExprNode)>;
    %type decl ModNode;
    %type decl_list Vec<ModNode>;
    %type elif_branch (ExprNode, Vec<CodeNode>);
    %type elif_list Vec<(ExprNode, Vec<CodeNode>)>;
    %type else_branch Vec<CodeNode>;
    %type expr ExprNode;
    %type fncall CodeNode;
    %type fncall_expr ExprNode;
    %type fndecl ModNode;
    %type forstmt CodeNode;
    %type ifstmt CodeNode;
    %type opassign BinOp;
    %type opt_assign ExprNode;
    %type param (String, DataType);
    %type param_list Vec<(String, DataType)>;
    %type returnstmt CodeNode;
    %type root Vec<ModNode>;
    %type single_constdecl (String, DataType, ExprNode);
    %type single_vardecl (String, DataType, Option<ExprNode>);
    %type step_variant ExprNode;
    %type stmt CodeNode;
    %type stmt_list Vec<CodeNode>;
    %type stmt_multiline CodeNode;
    %type stmt_oneline CodeNode;
    %type subscript ExprNode;
    %type type_convert ExprNode;
    %type var Vec<String>;
    %type vardecl CodeNode;
    %type vardecl_global ModNode;
    %type vardecl_list Vec<(String, DataType, Option<ExprNode>)>;
    %type whilestmt CodeNode;


    %left KwOr;
    %left KwAnd;
    %nonassoc Eq NotEq;
    %nonassoc Less LessOrEq Greater GreaterOrEq;
    %left Add Sub;
    %left Mul Div Mod;
    %right KwNot;

    root ::= decl_list;

    decl_list ::= decl_list(mut dl) decl(d) { dl.push(d); dl };
    decl_list ::= decl(d) { vec![d] };

    decl ::= vardecl_global;
    decl ::= constdecl;
    decl ::= fndecl;

    vardecl_global ::= KwVar single_vardecl(sv) NewLine { ModNode::VarDecl(vec![sv]) };
    vardecl_global ::= KwVar NewLine Indent vardecl_list(dl) NewLine Dedent { ModNode::VarDecl(dl) };

    constdecl ::= KwConst single_constdecl(sc) NewLine { ModNode::ConstDecl(vec![sc]) };
    constdecl ::= KwConst NewLine Indent constdecl_list(dl) NewLine Dedent { ModNode::ConstDecl(dl) };
    single_constdecl ::= Name(n) basic_type(t) Assign expr(e) { (n, t, e) };
    constdecl_list ::= constdecl_list(mut dl) NewLine single_constdecl(d) { dl.push(d); dl };
    constdecl_list ::= single_constdecl(d) { vec![d] };

    fndecl ::= KwFunc Name(n) LParen param_list(pl) RParen basic_type?(t) NewLine block(b) { ModNode::FuncDecl(n, pl, t, b) };
    param_list ::= param_list(mut pl) Comma param(p) { pl.push(p); pl };
    param_list ::= param(p) { vec![p] };
    param_list ::= { vec![] };
    param ::= Name(n) basic_type(t) { (n, t) };

    stmt_list ::= stmt_list(mut sl) stmt(s) { sl.push(s); sl };
    stmt_list ::= stmt(s) { vec![s] };

    stmt ::= stmt_oneline NewLine;
    stmt ::= stmt_multiline;

    stmt_oneline ::= assign;
    stmt_oneline ::= fncall;
    stmt_oneline ::= returnstmt;

    stmt_multiline ::= ifstmt;
    stmt_multiline ::= forstmt;
    stmt_multiline ::= vardecl;
    stmt_multiline ::= whilestmt;

    assign ::= var(v) Assign expr(e) { CodeNode::Assign(v, e) };
    assign ::= var(v) opassign(o) expr(e) { CodeNode::OpAssign(v, o, e) };
    opassign ::= AddAssign { BinOp::Add };
    opassign ::= SubAssign { BinOp::Sub };
    opassign ::= MulAssign { BinOp::Mul };
    opassign ::= DivAssign { BinOp::Div };
    opassign ::= ModAssign { BinOp::Mod };

    ifstmt ::= KwIf expr(e) KwThen stmt_oneline(so) NewLine { CodeNode::If(e, vec![so], vec![], vec![]) };
    ifstmt ::= KwIf expr(e) NewLine block(b) elif_list?(el) else_branch?(eb) { CodeNode::If(e, b, el.unwrap_or(vec![]), eb.unwrap_or(vec![])) };
    else_branch ::= KwElse NewLine block;
    elif_branch ::= KwElif expr(e) NewLine block(b) { (e, b) };
    elif_list ::= elif_list(mut el) elif_branch(eb) { el.push(eb); el };
    elif_list ::= elif_branch(el) { vec![el] };

    forstmt ::= KwFor var(v) Assign expr(es) KwTo expr(ef) step_variant?(s) NewLine block(b) { CodeNode::For(v, es, ef, s, b) };
    step_variant ::= KwStep expr;
    forstmt ::= KwFor var(v) KwIn expr(ea) NewLine block(b) { CodeNode::ForIn(v,ea, b) };
    forstmt ::= KwFor var(v) Assign expr(es) KwTo expr(ef) step_variant?(s) KwDo stmt_oneline(so) NewLine  { CodeNode::For(v, es, ef, s, vec![so]) };
    forstmt ::= KwFor var(v) KwIn expr(ea) KwDo stmt_oneline(so) NewLine { CodeNode::ForIn(v,ea, vec![so]) };

    vardecl ::= KwVar single_vardecl(sv) NewLine { CodeNode::VarDecl(vec![sv]) };
    vardecl ::= KwVar NewLine Indent vardecl_list(dl) NewLine Dedent { CodeNode::VarDecl(dl) };
    single_vardecl ::= Name(n) basic_type(t) opt_assign?(a) { (n, t, a) };
    opt_assign ::= Assign expr;
    vardecl_list ::= vardecl_list(mut dl) NewLine single_vardecl(d) { dl.push(d); dl };
    vardecl_list ::= single_vardecl(d) { vec![d] };

    returnstmt ::= KwReturn expr(e) { CodeNode::Return(e) };

    whilestmt ::= KwWhile expr(e) NewLine block(b) { CodeNode::While(e, b) };
    whilestmt ::= KwWhile expr(e) KwDo stmt_oneline(so) NewLine { CodeNode::While(e, vec![so]) };

    expr ::= Int(v)     { ExprNode::new(ExprNodeData::IntLiteral(v)) };
    expr ::= Float(v)   { ExprNode::new(ExprNodeData::FloatLiteral(v)) };
    expr ::= Str(v)     { ExprNode::new(ExprNodeData::StringLiteral(v)) };
    expr ::= boolval(v) { ExprNode::new(ExprNodeData::BoolLiteral(v)) };
    expr ::= var(v)     { ExprNode::new(ExprNodeData::Var(v)) };
    expr ::= LParen expr RParen;
    expr ::= fncall_expr;
    expr ::= type_convert;
    expr ::= subscript;
    expr ::= expr(l) Add         expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::Add,      Box::new(l), Box::new(r) )) };
    expr ::= expr(l) Sub         expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::Sub,      Box::new(l), Box::new(r) )) };
    expr ::= expr(l) Mul         expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::Mul,      Box::new(l), Box::new(r) )) };
    expr ::= expr(l) Div         expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::Div,      Box::new(l), Box::new(r) )) };
    expr ::= expr(l) Mod         expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::Mod,      Box::new(l), Box::new(r) )) };
    expr ::= expr(l) Less        expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::Less,     Box::new(l), Box::new(r) )) };
    expr ::= expr(l) LessOrEq    expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::LessEq,   Box::new(l), Box::new(r) )) };
    expr ::= expr(l) Greater     expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::Greater,  Box::new(l), Box::new(r) )) };
    expr ::= expr(l) GreaterOrEq expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::GreaterEq,Box::new(l), Box::new(r) )) };
    expr ::= expr(l) Eq          expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::Eq,       Box::new(l), Box::new(r) )) };
    expr ::= expr(l) NotEq       expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::NotEq,    Box::new(l), Box::new(r) )) };
    expr ::= expr(l) KwAnd       expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::And,      Box::new(l), Box::new(r) )) };
    expr ::= expr(l) KwOr        expr(r) { ExprNode::new(ExprNodeData::BinOp( BinOp::Or,       Box::new(l), Box::new(r) )) };
    expr ::= KwNot expr(e)       { ExprNode::new(ExprNodeData::UnOp( UnOp::Not, Box::new(e) )) };
    expr ::= Sub expr(e) [KwNot] { ExprNode::new(ExprNodeData::UnOp( UnOp::Neg, Box::new(e) )) };

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

    fncall ::= var(v) LParen arg_list(al) RParen { CodeNode::FnCall(v, al) };
    fncall_expr ::= var(v) LParen arg_list(al) RParen { ExprNode::new(ExprNodeData::FnCall(v, al)) };

    arg_list ::= arg_list(mut al) Comma expr(e) { al.push(e); al };
    arg_list ::= expr(e) { vec![e] };
    arg_list ::= { vec![] };

    type_convert ::= basic_type(t) LParen expr(e) RParen { ExprNode::new(ExprNodeData::TypeConvert(Box::new(e), t)) };

    subscript ::= var(v) LSqBracket expr(e) RSqBracket { ExprNode::new(ExprNodeData::Subscript(v, Box::new(e))) };

    block ::= Indent stmt_list Dedent;
    block ::= Indent KwPass NewLine Dedent { vec![] };

    // Reserved tokens

    // Reserved for function definition
    //root ::= KwFunc { ModNode::Dummy };
    decl ::= KwRef { ModNode::Dummy };

    // Reserved for struct definition
    decl ::= KwStruct { ModNode::Dummy };

    // Reserved for type aliases
    decl ::= KwType { ModNode::Dummy };

    // Reserved for static members
    decl ::= Colon { ModNode::Dummy };
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
            Token::KwDo => write!(f, "'do'"),
        }
    }
}

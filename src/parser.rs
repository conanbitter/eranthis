use std::fmt::Display;

use miette::SourceOffset;
use pomelo::pomelo;

pomelo! {
     %include {
        use crate::ast::*;
        use miette::{SourceSpan, SourceOffset};
        use crate::lexer::{FilePos, get_overall_span};
    }

    %token #[derive(Clone,Debug)] pub enum Token {};
    %extra_token SourceSpan;
    %extra_argument SourceOffset;
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
            Err(anyhow::anyhow!("[Offset {}] ERROR: got {}, expecting {}", extra.offset(), sometoken, expected_list))
        }else{
            Err(anyhow::anyhow!("[Offset {}] ERROR: expecting {}", extra.offset(), expected_list))
        }
    }

    %parse_fail {
        anyhow::anyhow!("[Offset {}] ERROR: total parser fail", extra.offset())
    }

    %stack_overflow {
        anyhow::anyhow!("[Offset {}] ERROR: parser stack overflow", extra.offset())
    }


    %type Name String;
    %type Str String;
    %type Int i64;
    %type Float f64;
    %type Eof;

    %type arg_list Vec<ExprNode>;
    %type assign CodeNode;
    %type basic_type (SourceSpan, DataType);
    %type block CodeBlock;
    %type boolval (SourceSpan, bool);
    %type constdecl ModNode;
    %type constdecl_list Vec<ConstDeclData>;
    %type decl ModNode;
    %type decl_list Vec<ModNode>;
    %type elif_branch (ExprNode, CodeBlock);
    %type elif_list Vec<(ExprNode, CodeBlock)>;
    %type else_branch CodeBlock;
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
    %type single_constdecl ConstDeclData;
    %type single_vardecl VarDeclData;
    %type step_variant ExprNode;
    %type stmt CodeNode;
    %type stmt_list CodeBlock;
    %type stmt_multiline CodeNode;
    %type stmt_oneline CodeNode;
    %type subscript ExprNode;
    %type type_convert ExprNode;
    %type var (SourceSpan, Vec<String>);
    %type vardecl CodeNode;
    %type vardecl_global ModNode;
    %type vardecl_list Vec<VarDeclData>;
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

    vardecl_global ::= KwVar(p) single_vardecl(sv) NewLine { ModNode::new(ModNodeData::VarDecl(vec![sv]), p) };
    vardecl_global ::= KwVar(p) NewLine Indent vardecl_list(dl) NewLine Dedent { ModNode::new(ModNodeData::VarDecl(dl), p) };

    constdecl ::= KwConst(p) single_constdecl(sc) NewLine { ModNode::new(ModNodeData::ConstDecl(vec![sc]), p) };
    constdecl ::= KwConst(p) NewLine Indent constdecl_list(dl) NewLine Dedent { ModNode::new(ModNodeData::ConstDecl(dl), p) };
    single_constdecl ::= Name(n) basic_type(t) Assign expr(e) { ConstDeclData{ name: n.1, consttype: t.1, value: e, span: n.0 } };
    constdecl_list ::= constdecl_list(mut dl) NewLine single_constdecl(d) { dl.push(d); dl };
    constdecl_list ::= single_constdecl(d) { vec![d] };

    fndecl ::= KwFunc(p) Name(n) LParen param_list(pl) RParen basic_type?(t) NewLine block(b) { ModNode::new(ModNodeData::FuncDecl{ name: n.1, params: pl, rettype: t.map(|t| t.1), code: b }, p) };
    param_list ::= param_list(mut pl) Comma param(p) { pl.push(p); pl };
    param_list ::= param(p) { vec![p] };
    param_list ::= { vec![] };
    param ::= Name(n) basic_type(t) { (n.1, t.1) };

    stmt_list ::= stmt_list(mut sl) stmt(s) { sl.stmts.push(s); sl };
    stmt_list ::= stmt(s) { CodeBlock::new(vec![s]) };

    stmt ::= stmt_oneline(st) NewLine { st };
    stmt ::= stmt_multiline;

    stmt_oneline ::= assign;
    stmt_oneline ::= fncall;
    stmt_oneline ::= returnstmt;

    stmt_multiline ::= ifstmt;
    stmt_multiline ::= forstmt;
    stmt_multiline ::= vardecl;
    stmt_multiline ::= whilestmt;

    assign ::= var(v) Assign expr(e) { CodeNode::new(CodeNodeData::Assign{ dst: v.1, value: e }, v.0) };
    assign ::= var(v) opassign(o) expr(e) { CodeNode::new(CodeNodeData::OpAssign{ dst: v.1, op: o, value: e }, v.0) };
    opassign ::= AddAssign { BinOp::Add };
    opassign ::= SubAssign { BinOp::Sub };
    opassign ::= MulAssign { BinOp::Mul };
    opassign ::= DivAssign { BinOp::Div };
    opassign ::= ModAssign { BinOp::Mod };

    ifstmt ::= KwIf(p) expr(e) KwThen stmt_oneline(so) NewLine { CodeNode::new(CodeNodeData::If{ cond: e, then: CodeBlock::new(vec![so]), elifs: vec![], elsebranch: CodeBlock::new(vec![]) }, p) };
    ifstmt ::= KwIf(p) expr(e) NewLine block(b) elif_list?(el) else_branch?(eb) { CodeNode::new(CodeNodeData::If{ cond: e, then: b, elifs: el.unwrap_or(vec![]), elsebranch: eb.unwrap_or(CodeBlock::new(vec![])) }, p) };
    else_branch ::= KwElse NewLine block(b) { b };
    elif_branch ::= KwElif expr(e) NewLine block(b) { (e, b) };
    elif_list ::= elif_list(mut el) elif_branch(eb) { el.push(eb); el };
    elif_list ::= elif_branch(el) { vec![el] };

    forstmt ::= KwFor(p) var(v) Assign expr(es) KwTo expr(ef) step_variant?(s) NewLine block(b) { CodeNode::new(CodeNodeData::For{ index: v.1, start: es, stop: ef, step: s, block: b }, p) };
    step_variant ::= KwStep expr(e) { e };
    forstmt ::= KwFor(p) var(v) KwIn expr(ea) NewLine block(b) { CodeNode::new(CodeNodeData::ForIn{ index: v.1, array: ea, block: b }, p) };
    forstmt ::= KwFor(p) var(v) Assign expr(es) KwTo expr(ef) step_variant?(s) KwDo stmt_oneline(so) NewLine  { CodeNode::new(CodeNodeData::For{ index: v.1, start: es, stop: ef, step: s, block: CodeBlock::new(vec![so]) }, p) };
    forstmt ::= KwFor(p) var(v) KwIn expr(ea) KwDo stmt_oneline(so) NewLine { CodeNode::new(CodeNodeData::ForIn{ index: v.1, array: ea, block: CodeBlock::new(vec![so]) }, p) };

    vardecl ::= KwVar(p) single_vardecl(sv) NewLine { CodeNode::new(CodeNodeData::VarDecl(vec![sv]), p) };
    vardecl ::= KwVar(p) NewLine Indent vardecl_list(dl) NewLine Dedent { CodeNode::new(CodeNodeData::VarDecl(dl), p) };
    single_vardecl ::= Name(n) basic_type(t) opt_assign?(a) { VarDeclData{ name: n.1, vartype: t.1, init: a, span: n.0 } };
    opt_assign ::= Assign expr(e) { e };
    vardecl_list ::= vardecl_list(mut dl) NewLine single_vardecl(d) { dl.push(d); dl };
    vardecl_list ::= single_vardecl(d) { vec![d] };

    returnstmt ::= KwReturn(p) expr(e) { CodeNode::new(CodeNodeData::Return(e), p) };

    whilestmt ::= KwWhile(p) expr(e) NewLine block(b) { CodeNode::new(CodeNodeData::While{ cond: e, block: b}, p) };
    whilestmt ::= KwWhile(p) expr(e) KwDo stmt_oneline(so) NewLine { CodeNode::new(CodeNodeData::While{ cond: e, block: CodeBlock::new(vec![so]) }, p) };

    expr ::= Int(v)     { ExprNode::new(ExprNodeData::IntLiteral(v.1), v.0, v.0) };
    expr ::= Float(v)   { ExprNode::new(ExprNodeData::FloatLiteral(v.1), v.0, v.0) };
    expr ::= Str(v)     { ExprNode::new(ExprNodeData::StringLiteral(v.1), v.0, v.0) };
    expr ::= boolval(v) { ExprNode::new(ExprNodeData::BoolLiteral(v.1), v.0, v.0) };
    expr ::= var(v)     { ExprNode::new(ExprNodeData::Var(v.1), v.0, v.0) };
    expr ::= LParen expr(e) RParen { e };
    expr ::= fncall_expr;
    expr ::= type_convert;
    expr ::= subscript;
    expr ::= expr(l) Add(p)         expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::Add,       left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) Sub(p)         expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::Sub,       left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) Mul(p)         expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::Mul,       left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) Div(p)         expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::Div,       left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) Mod(p)         expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::Mod,       left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) Less(p)        expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::Less,      left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) LessOrEq(p)    expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::LessEq,    left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) Greater(p)     expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::Greater,   left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) GreaterOrEq(p) expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::GreaterEq, left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) Eq(p)          expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::Eq,        left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) NotEq(p)       expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::NotEq,     left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) KwAnd(p)       expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::And,       left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= expr(l) KwOr(p)        expr(r) { let overall = get_overall_span(l.overall_span, r.overall_span); ExprNode::new(ExprNodeData::BinOp{ op: BinOp::Or,        left: Box::new(l), right: Box::new(r) }, p, overall) };
    expr ::= KwNot(p) expr(e)       { let overall = get_overall_span(p, e.overall_span); ExprNode::new(ExprNodeData::UnOp{ op: UnOp::Not, expr: Box::new(e) }, p, overall) };
    expr ::= Sub(p) expr(e) [KwNot] { let overall = get_overall_span(p, e.overall_span); ExprNode::new(ExprNodeData::UnOp{ op: UnOp::Neg, expr: Box::new(e) }, p, overall) };

    boolval ::= KwTrue(p)  { (p, true) };
    boolval ::= KwFalse(p) { (p, false) };

    basic_type ::= KwByte(p)   { (p, DataType::Byte) };
    basic_type ::= KwInt(p)    { (p, DataType::Int) };
    basic_type ::= KwFloat(p)  { (p, DataType::Float) };
    basic_type ::= KwFixed(p)  { (p, DataType::Fixed) };
    basic_type ::= KwString(p) { (p, DataType::String) };
    basic_type ::= KwBool(p)   { (p, DataType::Bool) };

    var ::= var(mut v) Period Name(n) { v.1.push(n.1); v };
    var ::= Name(n) { (n.0, vec![n.1]) };

    fncall ::= var(v) LParen arg_list(al) RParen { CodeNode::new(CodeNodeData::FnCall{ name: v.1, args: al }, v.0) };
    fncall_expr ::= var(v) LParen arg_list(al) RParen(p) { ExprNode::new(ExprNodeData::FnCall{ name: v.1, args: al }, v.0, get_overall_span(v.0, p)) };

    arg_list ::= arg_list(mut al) Comma expr(e) { al.push(e); al };
    arg_list ::= expr(e) { vec![e] };
    arg_list ::= { vec![] };

    type_convert ::= basic_type(t) LParen expr(e) RParen(p) { ExprNode::new(ExprNodeData::TypeConvert{ expr: Box::new(e), newtype: t.1 }, t.0, get_overall_span(t.0, p)) };

    subscript ::= var(v) LSqBracket expr(e) RSqBracket(p) { ExprNode::new(ExprNodeData::Subscript{ name: v.1, index: Box::new(e)}, v.0, get_overall_span(v.0, p)) };

    block ::= Indent stmt_list(sl) Dedent { sl };
    block ::= Indent KwPass NewLine Dedent { CodeBlock::new(vec![]) };

    // Reserved tokens

    // Reserved for function definition
    //root ::= KwFunc { ModNode::Dummy };
    decl ::= KwRef(p) { ModNode::new(ModNodeData::Dummy, p) };

    // Reserved for struct definition
    decl ::= KwStruct(p) { ModNode::new(ModNodeData::Dummy, p) };

    // Reserved for type aliases
    decl ::= KwType(p) { ModNode::new(ModNodeData::Dummy, p) };

    // Reserved for static members
    decl ::= Colon(p) { ModNode::new(ModNodeData::Dummy, p) };
}

pub use parser::Parser;
pub use parser::Token;

use crate::lexer::get_overall_span;

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Name((_, name)) => write!(f, "name '{}'", name),
            Token::Str((_, st)) => write!(f, "string literal '{}'", st),
            Token::Int((_, v)) => write!(f, "int literal ({})", v),
            Token::Float((_, v)) => write!(f, "float literal ({})", v),
            Token::Eof(_) => write!(f, "end of file"),
            Token::KwOr(_) => write!(f, "'or'"),
            Token::KwAnd(_) => write!(f, "'and'"),
            Token::Eq(_) => write!(f, "'=='"),
            Token::NotEq(_) => write!(f, "'!='"),
            Token::Less(_) => write!(f, "'<'"),
            Token::LessOrEq(_) => write!(f, "'<='"),
            Token::Greater(_) => write!(f, "'>'"),
            Token::GreaterOrEq(_) => write!(f, "'>='"),
            Token::Add(_) => write!(f, "'+'"),
            Token::Sub(_) => write!(f, "'-'"),
            Token::Mul(_) => write!(f, "'*'"),
            Token::Div(_) => write!(f, "'/'"),
            Token::Mod(_) => write!(f, "'%'"),
            Token::KwNot(_) => write!(f, "'not'"),
            Token::NewLine(_) => write!(f, "new line"),
            Token::Assign(_) => write!(f, "'='"),
            Token::AddAssign(_) => write!(f, "'+='"),
            Token::SubAssign(_) => write!(f, "'-='"),
            Token::MulAssign(_) => write!(f, "'*='"),
            Token::DivAssign(_) => write!(f, "'/='"),
            Token::ModAssign(_) => write!(f, "'%='"),
            Token::KwIf(_) => write!(f, "'if'"),
            Token::KwThen(_) => write!(f, "'then'"),
            Token::KwElse(_) => write!(f, "'else'"),
            Token::KwElif(_) => write!(f, "'elif'"),
            Token::KwFor(_) => write!(f, "'for'"),
            Token::KwTo(_) => write!(f, "'to'"),
            Token::KwStep(_) => write!(f, "'step'"),
            Token::KwIn(_) => write!(f, "'in'"),
            Token::KwVar(_) => write!(f, "'var'"),
            Token::Indent(_) => write!(f, "indent"),
            Token::Dedent(_) => write!(f, "dedent"),
            Token::KwConst(_) => write!(f, "'const'"),
            Token::LParen(_) => write!(f, "'('"),
            Token::RParen(_) => write!(f, "')'"),
            Token::KwTrue(_) => write!(f, "'true'"),
            Token::KwFalse(_) => write!(f, "'false'"),
            Token::KwByte(_) => write!(f, "'byte'"),
            Token::KwInt(_) => write!(f, "'int'"),
            Token::KwFloat(_) => write!(f, "'float'"),
            Token::KwFixed(_) => write!(f, "'fixed'"),
            Token::KwString(_) => write!(f, "'string'"),
            Token::KwBool(_) => write!(f, "'bool'"),
            Token::Period(_) => write!(f, "'.'"),
            Token::Comma(_) => write!(f, "','"),
            Token::KwPass(_) => write!(f, "'pass'"),
            Token::KwFunc(_) => write!(f, "'func'"),
            Token::KwRef(_) => write!(f, "'ref'"),
            Token::KwReturn(_) => write!(f, "'return'"),
            Token::KwStruct(_) => write!(f, "'struct'"),
            Token::KwType(_) => write!(f, "'type'"),
            Token::KwWhile(_) => write!(f, "'while'"),
            Token::Colon(_) => write!(f, "':'"),
            Token::LSqBracket(_) => write!(f, "'['"),
            Token::RSqBracket(_) => write!(f, "']'"),
            Token::KwDo(_) => write!(f, "'do'"),
        }
    }
}

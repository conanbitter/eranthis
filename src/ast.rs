#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ExprType {
    Unknown,
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    BoolLiteral,
    Byte,
    Int,
    Float,
    Fixed,
    String,
    Bool,
    //Struct,
}

#[derive(Debug)]
pub enum ExprNodeData {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    BinOp {
        op: BinOp,
        left: Box<ExprNode>,
        right: Box<ExprNode>,
    },
    UnOp {
        op: UnOp,
        expr: Box<ExprNode>,
    },
    Var(Vec<String>),
    FnCall {
        name: Vec<String>,
        args: Vec<ExprNode>,
    },
    TypeConvert {
        expr: Box<ExprNode>,
        newtype: DataType,
    },
    Subscript {
        name: Vec<String>,
        index: Box<ExprNode>,
    },
}

#[derive(Debug)]
pub struct ExprNode {
    pub datatype: ExprType,
    pub data: ExprNodeData,
    pub pos: SourceSpan,
}

#[derive(Debug)]
pub struct VarDeclData {
    pub name: String,
    pub vartype: DataType,
    pub init: Option<ExprNode>,
    pub span: SourceSpan,
}

#[derive(Debug)]
pub enum CodeNodeData {
    FnCall {
        name: Vec<String>,
        args: Vec<ExprNode>,
    },
    Assign {
        dst: Vec<String>,
        value: ExprNode,
    },
    OpAssign {
        dst: Vec<String>,
        op: BinOp,
        value: ExprNode,
    },
    If {
        cond: ExprNode,
        then: CodeBlock,
        elifs: Vec<(/*cond*/ ExprNode, CodeBlock)>,
        elsebranch: CodeBlock,
    },
    For {
        index: Vec<String>,
        start: ExprNode,
        stop: ExprNode,
        step: Option<ExprNode>,
        block: CodeBlock,
    },
    ForIn {
        index: Vec<String>,
        array: ExprNode,
        block: CodeBlock,
    },
    VarDecl(Vec<VarDeclData>),
    While {
        cond: ExprNode,
        block: CodeBlock,
    },
    Return(ExprNode),
}

#[derive(Debug)]
pub struct CodeNode {
    pos: SourceSpan,
    pub data: CodeNodeData,
}

#[derive(Debug)]
pub struct CodeBlock {
    pub stmts: Vec<CodeNode>,
    pub context_id: u32,
}

#[derive(Debug)]
pub struct ConstDeclData {
    pub name: String,
    pub consttype: DataType,
    pub value: ExprNode,
    pub span: SourceSpan,
}

pub enum ModNodeData {
    VarDecl(Vec<VarDeclData>),
    ConstDecl(Vec<ConstDeclData>),
    FuncDecl {
        name: String,
        params: Vec<(String, DataType)>,
        rettype: Option<DataType>,
        code: CodeBlock,
    },
    Dummy,
}

pub struct ModNode {
    pos: SourceSpan,
    pub data: ModNodeData,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Eq,
    NotEq,
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum DataType {
    Byte,
    Int,
    Float,
    Fixed,
    String,
    Bool,
}
// DEBUG

use std::{
    fmt::Display,
    fs::File,
    io::{BufWriter, Write},
    path::Path,
};

use miette::SourceSpan;

use crate::semantic;

impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Mod => write!(f, "%"),
            BinOp::Less => write!(f, "<"),
            BinOp::LessEq => write!(f, "<="),
            BinOp::Greater => write!(f, ">"),
            BinOp::GreaterEq => write!(f, ">="),
            BinOp::Eq => write!(f, "=="),
            BinOp::NotEq => write!(f, "!="),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
        }
    }
}

impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOp::Neg => write!(f, "-"),
            UnOp::Not => write!(f, "not"),
        }
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Byte => write!(f, "byte"),
            DataType::Int => write!(f, "int"),
            DataType::Float => write!(f, "float"),
            DataType::Fixed => write!(f, "fixed"),
            DataType::String => write!(f, "string"),
            DataType::Bool => write!(f, "bool"),
        }
    }
}

fn get_expr_type(data: &ExprNodeData) -> ExprType {
    match data {
        ExprNodeData::IntLiteral(_) => ExprType::IntLiteral,
        ExprNodeData::FloatLiteral(_) => ExprType::FloatLiteral,
        ExprNodeData::StringLiteral(_) => ExprType::StringLiteral,
        ExprNodeData::BoolLiteral(_) => ExprType::BoolLiteral,
        ExprNodeData::BinOp { left, right, .. } => {
            if left.datatype == right.datatype {
                left.datatype
            } else if left.datatype == ExprType::IntLiteral {
                match right.datatype {
                    ExprType::Byte => ExprType::Byte,
                    ExprType::Int => ExprType::Int,
                    ExprType::FloatLiteral => ExprType::FloatLiteral,
                    _ => ExprType::Unknown,
                }
            } else if right.datatype == ExprType::IntLiteral {
                match left.datatype {
                    ExprType::Byte => ExprType::Byte,
                    ExprType::Int => ExprType::Int,
                    ExprType::FloatLiteral => ExprType::FloatLiteral,
                    _ => ExprType::Unknown,
                }
            } else if left.datatype == ExprType::FloatLiteral {
                match right.datatype {
                    ExprType::Float => ExprType::Float,
                    ExprType::Fixed => ExprType::Fixed,
                    ExprType::IntLiteral => ExprType::FloatLiteral,
                    _ => ExprType::Unknown,
                }
            } else if right.datatype == ExprType::FloatLiteral {
                match left.datatype {
                    ExprType::Float => ExprType::Float,
                    ExprType::Fixed => ExprType::Fixed,
                    ExprType::IntLiteral => ExprType::FloatLiteral,
                    _ => ExprType::Unknown,
                }
            } else if (left.datatype == ExprType::StringLiteral && right.datatype == ExprType::String)
                || (right.datatype == ExprType::StringLiteral && left.datatype == ExprType::String)
            {
                ExprType::String
            } else if (left.datatype == ExprType::BoolLiteral && right.datatype == ExprType::Bool)
                || (right.datatype == ExprType::BoolLiteral && left.datatype == ExprType::Bool)
            {
                ExprType::Bool
            } else {
                ExprType::Unknown
            }
        }
        ExprNodeData::UnOp { expr, .. } => expr.datatype,
        ExprNodeData::Var(_) => ExprType::Unknown,
        ExprNodeData::FnCall { .. } => ExprType::Unknown,
        ExprNodeData::TypeConvert { newtype, .. } => match newtype {
            DataType::Byte => ExprType::Byte,
            DataType::Int => ExprType::Int,
            DataType::Float => ExprType::Float,
            DataType::Fixed => ExprType::Fixed,
            DataType::String => ExprType::String,
            DataType::Bool => ExprType::Bool,
        },
        ExprNodeData::Subscript { .. } => ExprType::Unknown,
    }
}

impl ExprNode {
    pub fn new(data: ExprNodeData, pos: SourceSpan) -> ExprNode {
        let datatype = get_expr_type(&data);
        ExprNode { datatype, data, pos }
    }
}

impl CodeNode {
    pub fn new(data: CodeNodeData, pos: SourceSpan) -> CodeNode {
        CodeNode { data, pos }
    }
}

impl CodeBlock {
    pub fn new(stmts: Vec<CodeNode>) -> CodeBlock {
        CodeBlock { stmts, context_id: 0 }
    }
}

impl ModNode {
    pub fn new(data: ModNodeData, pos: SourceSpan) -> ModNode {
        ModNode { data, pos }
    }
}

const DEBUG_INDENT: &str = "    ";

fn dump_block(block: &CodeBlock, w: &mut BufWriter<File>, indent: String) -> anyhow::Result<()> {
    for (i, node) in block.stmts.iter().enumerate() {
        writeln!(w, "{}[{}]:", indent.clone(), i)?;
        dump_codenode(node, w, indent.clone() + DEBUG_INDENT)?;
    }
    Ok(())
}

fn dump_expr(node: &ExprNode, w: &mut BufWriter<File>, indent: String) -> anyhow::Result<()> {
    match &node.data {
        ExprNodeData::IntLiteral(v) => {
            writeln!(w, "{}{:?}({:?}) int {}", indent, node.pos, node.datatype, v)?;
        }
        ExprNodeData::FloatLiteral(v) => {
            writeln!(w, "{}{:?}({:?}) float {}", indent, node.pos, node.datatype, v)?;
        }
        ExprNodeData::StringLiteral(v) => {
            writeln!(w, "{}{:?}({:?}) string {:?}", indent, node.pos, node.datatype, v)?;
        }
        ExprNodeData::BoolLiteral(v) => {
            writeln!(
                w,
                "{}{:?}({:?}) bool {}",
                indent,
                node.pos,
                node.datatype,
                if *v { "true" } else { "false" }
            )?;
        }
        ExprNodeData::Var(items) => {
            writeln!(
                w,
                "{}{:?}({:?}) var {{ {} }}",
                indent,
                node.pos,
                node.datatype,
                items.join(" -> ")
            )?;
        }
        ExprNodeData::FnCall { name, args } => {
            writeln!(
                w,
                "{}{:?}({:?}) func {{ {} }}",
                indent,
                node.pos,
                node.datatype,
                name.join(" -> ")
            )?;
            for (i, node) in args.iter().enumerate() {
                writeln!(w, "{}param[{}]:", indent.clone(), i)?;
                dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        ExprNodeData::BinOp { op, left, right } => {
            writeln!(
                w,
                "{}{:?}({:?}) binop '{}'",
                indent.clone(),
                node.pos,
                node.datatype,
                op
            )?;
            writeln!(w, "{}left:", indent.clone())?;
            dump_expr(left, w, indent.clone() + DEBUG_INDENT)?;
            writeln!(w, "{}right:", indent.clone())?;
            dump_expr(right, w, indent.clone() + DEBUG_INDENT)?;
        }
        ExprNodeData::UnOp { op, expr } => {
            writeln!(
                w,
                "{}{:?}({:?}) unop '{}':",
                indent.clone(),
                node.pos,
                node.datatype,
                op
            )?;
            dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;
        }
        ExprNodeData::TypeConvert { expr, newtype } => {
            writeln!(
                w,
                "{}{:?}({:?}) convert to {} from:",
                indent, node.pos, node.datatype, newtype
            )?;
            dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;
        }
        ExprNodeData::Subscript { name, index } => {
            writeln!(
                w,
                "{}{:?}({:?}) array {{{}}} index:",
                indent,
                node.pos,
                node.datatype,
                name.join(" -> ")
            )?;
            dump_expr(index, w, indent.clone() + DEBUG_INDENT)?;
        }
    }
    Ok(())
}

fn dump_codenode(node: &CodeNode, w: &mut BufWriter<File>, indent: String) -> anyhow::Result<()> {
    match &node.data {
        CodeNodeData::FnCall { name, args } => {
            writeln!(w, "{}{:?}func {{ {} }}", indent, node.pos, name.join(" -> "))?;
            for (i, node) in args.iter().enumerate() {
                writeln!(w, "{}param[{}]:", indent.clone(), i)?;
                dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        CodeNodeData::Assign { dst, value } => {
            writeln!(w, "{}{:?}assign to {{ {} }}:", indent, node.pos, dst.join(" -> "))?;
            dump_expr(value, w, indent.clone() + DEBUG_INDENT)?;
        }
        CodeNodeData::OpAssign { dst, op, value } => {
            writeln!(
                w,
                "{}{:?}assign and {} to {{ {} }}:",
                indent,
                node.pos,
                op,
                dst.join(" -> ")
            )?;
            dump_expr(value, w, indent.clone() + DEBUG_INDENT)?;
        }
        CodeNodeData::If {
            cond,
            then,
            elifs,
            elsebranch,
        } => {
            writeln!(w, "{}{:?}if", indent, node.pos)?;
            writeln!(w, "{}cond:", indent)?;
            dump_expr(cond, w, indent.clone() + DEBUG_INDENT)?;

            writeln!(w, "{}then:", indent)?;
            dump_block(then, w, indent.clone())?;

            if !elifs.is_empty() {
                for (i, (expr, block)) in elifs.iter().enumerate() {
                    writeln!(w, "{}elif[{}]", indent.clone(), i)?;
                    writeln!(w, "{}expr:", indent)?;
                    dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;
                    dump_block(block, w, indent.clone())?;
                }
            }

            if !elsebranch.stmts.is_empty() {
                writeln!(w, "{}else:", indent)?;
                dump_block(elsebranch, w, indent)?;
            }
        }
        CodeNodeData::For {
            index,
            start,
            stop,
            step,
            block,
        } => {
            writeln!(w, "{}{:?}for {{{}}}", indent, node.pos, index.join(" -> "))?;
            writeln!(w, "{}start expr:", indent)?;
            dump_expr(start, w, indent.clone() + DEBUG_INDENT)?;
            writeln!(w, "{}stop expr:", indent)?;
            dump_expr(stop, w, indent.clone() + DEBUG_INDENT)?;
            if let Some(step) = step {
                writeln!(w, "{}step expr:", indent)?;
                dump_expr(step, w, indent.clone() + DEBUG_INDENT)?;
            }
            dump_block(block, w, indent)?;
        }
        CodeNodeData::ForIn { index, array, block } => {
            writeln!(w, "{}{:?}for {{{}}} in:", indent, node.pos, index.join(" -> "))?;
            dump_expr(array, w, indent.clone() + DEBUG_INDENT)?;
            dump_block(block, w, indent)?;
        }
        CodeNodeData::VarDecl(vars) => {
            for VarDeclData {
                name,
                vartype,
                init,
                span,
            } in vars
            {
                if let Some(node) = init {
                    writeln!(w, "{}{:?}var {} type {} init:", indent, span, name, vartype)?;
                    dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
                } else {
                    writeln!(w, "{}{:?}var {} type {}", indent, span, name, vartype)?;
                }
            }
        }
        CodeNodeData::While { cond, block } => {
            writeln!(w, "{}{:?}while", indent, node.pos)?;
            writeln!(w, "{}condition:", indent)?;
            dump_expr(cond, w, indent.clone() + DEBUG_INDENT)?;
            dump_block(block, w, indent)?;
        }
        CodeNodeData::Return(expr) => {
            writeln!(w, "{}{:?}return:", indent, node.pos)?;
            dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;
        }
    }
    Ok(())
}

fn dump_modnode(node: &ModNode, w: &mut BufWriter<File>) -> anyhow::Result<()> {
    match &node.data {
        ModNodeData::Dummy => {
            writeln!(w, "\n#dummy")?;
        }
        ModNodeData::VarDecl(vars) => {
            for VarDeclData {
                name,
                vartype,
                init,
                span,
            } in vars
            {
                if let Some(node) = init {
                    writeln!(w, "\n{:?}#vardecl {} type {} init:", span, name, vartype)?;
                    dump_expr(node, w, String::from(DEBUG_INDENT))?;
                } else {
                    writeln!(w, "\n{:?}#vardecl {} type {}", span, name, vartype)?;
                }
            }
        }
        ModNodeData::ConstDecl(vars) => {
            for ConstDeclData {
                name,
                consttype,
                value,
                span,
            } in vars
            {
                writeln!(w, "\n{:?}#constdecl {} type {}:", span, name, consttype)?;
                dump_expr(value, w, String::from(DEBUG_INDENT))?;
            }
        }
        ModNodeData::FuncDecl {
            name,
            params,
            rettype,
            code,
        } => {
            write!(w, "\n{:?}#fndecl {} ( ", node.pos, name)?;
            for (param_name, param_type) in params {
                write!(w, "{}:{} ", param_name, param_type)?;
            }
            write!(w, ")")?;
            if let Some(rettype) = rettype {
                write!(w, " -> {}", rettype)?;
            }
            writeln!(w, ":")?;
            dump_block(code, w, String::from(""))?;
        }
    }
    Ok(())
}

#[allow(dead_code)]
pub fn debug_dump<P: AsRef<Path>>(root: &Vec<ModNode>, output_file: P) -> anyhow::Result<()> {
    let out = File::create(output_file)?;
    let mut wr = BufWriter::new(out);
    for node in root {
        dump_modnode(node, &mut wr)?;
    }
    wr.flush()?;
    Ok(())
}

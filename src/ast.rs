type Name = Vec<String>;
type ExprBoxed = Box<ExprNode>;

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
    BinOp(BinOp, ExprBoxed, ExprBoxed),
    UnOp(UnOp, ExprBoxed),
    Var(Name),
    FnCall(Name, /*params*/ Vec<ExprNode>),
    TypeConvert(ExprBoxed, DataType),
    Subscript(Name, /*index*/ ExprBoxed),
}

#[derive(Debug)]
pub struct ExprNode {
    datatype: ExprType,
    data: ExprNodeData,
    pos: FilePos,
}

#[derive(Debug)]
pub enum CodeNodeData {
    FnCall(Name, /*params*/ Vec<ExprNode>),
    Assign(Name, ExprNode),
    OpAssign(Name, BinOp, ExprNode),
    If(
        /*cond*/ ExprNode,
        /*then*/ CodeBlock,
        /*elifs*/ Vec<(/*cond*/ ExprNode, CodeBlock)>,
        /*else*/ CodeBlock,
    ),
    For(
        Name,
        /*start*/ ExprNode,
        /*stop*/ ExprNode,
        /*step*/ Option<ExprNode>,
        CodeBlock,
    ),
    ForIn(Name, /*array*/ ExprNode, CodeBlock),
    VarDecl(Vec<(/*name*/ String, DataType, Option<ExprNode>, FilePos)>),
    While(ExprNode, CodeBlock),
    Return(ExprNode),
}

#[derive(Debug)]
pub struct CodeNode {
    pos: FilePos,
    data: CodeNodeData,
}

#[derive(Debug)]
pub struct CodeBlock {
    pub stmts: Vec<CodeNode>,
}

pub enum ModNodeData {
    VarDecl(Vec<(/*name*/ String, DataType, Option<ExprNode>, FilePos)>),
    ConstDecl(Vec<(/*name*/ String, DataType, ExprNode, FilePos)>),
    FuncDecl(
        /*name*/ String,
        /*params*/ Vec<(String, DataType)>,
        /*return type*/ Option<DataType>,
        /* code */ CodeBlock,
    ),
    Dummy,
}

pub struct ModNode {
    pos: FilePos,
    data: ModNodeData,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug)]
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

use crate::lexer::FilePos;

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
        ExprNodeData::BinOp(_, left, right) => {
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
        ExprNodeData::UnOp(_, expr_node) => expr_node.datatype,
        ExprNodeData::Var(_) => ExprType::Unknown,
        ExprNodeData::FnCall(_, _) => ExprType::Unknown,
        ExprNodeData::TypeConvert(_, data_type) => match data_type {
            DataType::Byte => ExprType::Byte,
            DataType::Int => ExprType::Int,
            DataType::Float => ExprType::Float,
            DataType::Fixed => ExprType::Fixed,
            DataType::String => ExprType::String,
            DataType::Bool => ExprType::Bool,
        },
        ExprNodeData::Subscript(_, _) => ExprType::Unknown,
    }
}

impl ExprNode {
    pub fn new(data: ExprNodeData, pos: FilePos) -> ExprNode {
        let datatype = get_expr_type(&data);
        ExprNode { datatype, data, pos }
    }
}

impl CodeNode {
    pub fn new(data: CodeNodeData, pos: FilePos) -> CodeNode {
        CodeNode { data, pos }
    }
}

impl CodeBlock {
    pub fn new(stmts: Vec<CodeNode>) -> CodeBlock {
        CodeBlock { stmts }
    }
}

impl ModNode {
    pub fn new(data: ModNodeData, pos: FilePos) -> ModNode {
        ModNode { data, pos }
    }
}

impl Display for FilePos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{{}:{}}}", self.line, self.col)
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
            writeln!(w, "{}{}({:?}) int {}", indent, node.pos, node.datatype, v)?;
        }
        ExprNodeData::FloatLiteral(v) => {
            writeln!(w, "{}{}({:?}) float {}", indent, node.pos, node.datatype, v)?;
        }
        ExprNodeData::StringLiteral(v) => {
            writeln!(w, "{}{}({:?}) string {:?}", indent, node.pos, node.datatype, v)?;
        }
        ExprNodeData::BoolLiteral(v) => {
            writeln!(
                w,
                "{}{}({:?}) bool {}",
                indent,
                node.pos,
                node.datatype,
                if *v { "true" } else { "false" }
            )?;
        }
        ExprNodeData::Var(items) => {
            writeln!(
                w,
                "{}{}({:?}) var {{ {} }}",
                indent,
                node.pos,
                node.datatype,
                items.join(" -> ")
            )?;
        }
        ExprNodeData::FnCall(items, params) => {
            writeln!(
                w,
                "{}{}({:?}) func {{ {} }}",
                indent,
                node.pos,
                node.datatype,
                items.join(" -> ")
            )?;
            for (i, node) in params.iter().enumerate() {
                writeln!(w, "{}param[{}]:", indent.clone(), i)?;
                dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        ExprNodeData::BinOp(bin_op, node_left, node_right) => {
            writeln!(
                w,
                "{}{}({:?}) binop '{}'",
                indent.clone(),
                node.pos,
                node.datatype,
                bin_op
            )?;
            writeln!(w, "{}left:", indent.clone())?;
            dump_expr(node_left, w, indent.clone() + DEBUG_INDENT)?;
            writeln!(w, "{}right:", indent.clone())?;
            dump_expr(node_right, w, indent.clone() + DEBUG_INDENT)?;
        }
        ExprNodeData::UnOp(un_op, node) => {
            writeln!(
                w,
                "{}{}({:?}) unop '{}':",
                indent.clone(),
                node.pos,
                node.datatype,
                un_op
            )?;
            dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
        }
        ExprNodeData::TypeConvert(expr, data_type) => {
            writeln!(
                w,
                "{}{}({:?}) convert to {} from:",
                indent, node.pos, node.datatype, data_type
            )?;
            dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;
        }
        ExprNodeData::Subscript(name, index) => {
            writeln!(
                w,
                "{}{}({:?}) array {{{}}} index:",
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
        CodeNodeData::FnCall(items, params) => {
            writeln!(w, "{}{}func {{ {} }}", indent, node.pos, items.join(" -> "))?;
            for (i, node) in params.iter().enumerate() {
                writeln!(w, "{}param[{}]:", indent.clone(), i)?;
                dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        CodeNodeData::Assign(items, node) => {
            writeln!(w, "{}{}assign to {{ {} }}:", indent, node.pos, items.join(" -> "))?;
            dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
        }
        CodeNodeData::OpAssign(items, op, node) => {
            writeln!(
                w,
                "{}{}assign and {} to {{ {} }}:",
                indent,
                node.pos,
                op,
                items.join(" -> ")
            )?;
            dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
        }
        CodeNodeData::If(expr, then_block, elifs, else_block) => {
            writeln!(w, "{}{}if", indent, node.pos)?;
            writeln!(w, "{}expr:", indent)?;
            dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;

            writeln!(w, "{}then:", indent)?;
            dump_block(then_block, w, indent.clone())?;

            if !elifs.is_empty() {
                for (i, (expr, block)) in elifs.iter().enumerate() {
                    writeln!(w, "{}elif[{}]", indent.clone(), i)?;
                    writeln!(w, "{}expr:", indent)?;
                    dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;
                    dump_block(block, w, indent.clone())?;
                }
            }

            if !else_block.stmts.is_empty() {
                writeln!(w, "{}else:", indent)?;
                dump_block(else_block, w, indent)?;
            }
        }
        CodeNodeData::For(var, start, stop, step, block) => {
            writeln!(w, "{}{}for {{{}}}", indent, node.pos, var.join(" -> "))?;
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
        CodeNodeData::ForIn(var, array, block) => {
            writeln!(w, "{}{}for {{{}}} in:", indent, node.pos, var.join(" -> "))?;
            dump_expr(array, w, indent.clone() + DEBUG_INDENT)?;
            dump_block(block, w, indent)?;
        }
        CodeNodeData::VarDecl(vars) => {
            for (name, vartype, init, pos) in vars {
                if let Some(node) = init {
                    writeln!(w, "{}{}var {} type {} init:", indent, pos, name, vartype)?;
                    dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
                } else {
                    writeln!(w, "{}{}var {} type {}", indent, pos, name, vartype)?;
                }
            }
        }
        CodeNodeData::While(cond, block) => {
            writeln!(w, "{}{}while", indent, node.pos)?;
            writeln!(w, "{}condition:", indent)?;
            dump_expr(cond, w, indent.clone() + DEBUG_INDENT)?;
            dump_block(block, w, indent)?;
        }
        CodeNodeData::Return(expr) => {
            writeln!(w, "{}{}return:", indent, node.pos)?;
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
            for (name, vartype, init, pos) in vars {
                if let Some(node) = init {
                    writeln!(w, "\n{}#vardecl {} type {} init:", pos, name, vartype)?;
                    dump_expr(node, w, String::from(DEBUG_INDENT))?;
                } else {
                    writeln!(w, "\n{}#vardecl {} type {}", pos, name, vartype)?;
                }
            }
        }
        ModNodeData::ConstDecl(vars) => {
            for (name, consttype, init, pos) in vars {
                writeln!(w, "\n{}#constdecl {} type {}:", pos, name, consttype)?;
                dump_expr(init, w, String::from(DEBUG_INDENT))?;
            }
        }
        ModNodeData::FuncDecl(name, params, return_type, code) => {
            write!(w, "\n{}#fndecl {} ( ", node.pos, name)?;
            for (param_name, param_type) in params {
                write!(w, "{}:{} ", param_name, param_type)?;
            }
            write!(w, ")")?;
            if let Some(return_type) = return_type {
                write!(w, " -> {}", return_type)?;
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

type Block = Vec<Node>;
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
}

#[derive(Debug)]
pub enum Node {
    FnCall(Name, /*params*/ Vec<ExprNode>),
    Assign(Name, ExprNode),
    OpAssign(Name, BinOp, ExprNode),
    If(
        /*cond*/ ExprNode,
        /*then*/ Block,
        /*elifs*/ Vec<(/*cond*/ ExprNode, Block)>,
        /*else*/ Block,
    ),
    For(
        Name,
        /*start*/ ExprNode,
        /*stop*/ ExprNode,
        /*step*/ Option<ExprNode>,
        Block,
    ),
    ForIn(Name, /*array*/ ExprNode, Block),
    VarDecl(Vec<(/*name*/ String, DataType, Option<ExprNode>)>),
    ConstDecl(Vec<(/*name*/ String, DataType, ExprNode)>),
    While(ExprNode, Block),
    Return(ExprNode),
    Dummy,
    DummyVec(Block),
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
    pub fn new(data: ExprNodeData) -> ExprNode {
        let datatype = get_expr_type(&data);
        ExprNode { datatype, data }
    }
}

const DEBUG_INDENT: &str = "    ";

fn dump_expr(node: &ExprNode, w: &mut BufWriter<File>, indent: String) -> anyhow::Result<()> {
    match &node.data {
        ExprNodeData::IntLiteral(v) => {
            writeln!(w, "{}({:?}) int {}", indent, node.datatype, v)?;
        }
        ExprNodeData::FloatLiteral(v) => {
            writeln!(w, "{}({:?}) float {}", indent, node.datatype, v)?;
        }
        ExprNodeData::StringLiteral(v) => {
            writeln!(w, "{}({:?}) string {:?}", indent, node.datatype, v)?;
        }
        ExprNodeData::BoolLiteral(v) => {
            writeln!(
                w,
                "{}({:?}) bool {}",
                indent,
                node.datatype,
                if *v { "true" } else { "false" }
            )?;
        }
        ExprNodeData::Var(items) => {
            writeln!(w, "{}({:?}) var {{ {} }}", indent, node.datatype, items.join(" -> "))?;
        }
        ExprNodeData::FnCall(items, params) => {
            writeln!(w, "{}({:?}) func {{ {} }}", indent, node.datatype, items.join(" -> "))?;
            for (i, node) in params.iter().enumerate() {
                writeln!(w, "{}param[{}]:", indent.clone(), i)?;
                dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        ExprNodeData::BinOp(bin_op, node_left, node_right) => {
            writeln!(w, "{}({:?}) binop '{}'", indent.clone(), node.datatype, bin_op)?;
            writeln!(w, "{}left:", indent.clone())?;
            dump_expr(node_left, w, indent.clone() + DEBUG_INDENT)?;
            writeln!(w, "{}right:", indent.clone())?;
            dump_expr(node_right, w, indent.clone() + DEBUG_INDENT)?;
        }
        ExprNodeData::UnOp(un_op, node) => {
            writeln!(w, "{}({:?}) unop '{}':", indent.clone(), node.datatype, un_op)?;
            dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
        }
        ExprNodeData::TypeConvert(expr, data_type) => {
            writeln!(w, "{}({:?}) convert to {} from:", indent, node.datatype, data_type)?;
            dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;
        }
        ExprNodeData::Subscript(name, index) => {
            writeln!(
                w,
                "{}({:?}) array {{{}}} index:",
                indent,
                node.datatype,
                name.join(" -> ")
            )?;
            dump_expr(index, w, indent.clone() + DEBUG_INDENT)?;
        }
    }
    Ok(())
}

fn dump_node(node: &Node, w: &mut BufWriter<File>, indent: String) -> anyhow::Result<()> {
    match node {
        Node::FnCall(items, params) => {
            writeln!(w, "{}func {{ {} }}", indent, items.join(" -> "))?;
            for (i, node) in params.iter().enumerate() {
                writeln!(w, "{}param[{}]:", indent.clone(), i)?;
                dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        Node::Dummy => {
            writeln!(w, "{}dummy", indent)?;
        }
        Node::DummyVec(nodes) => {
            writeln!(w, "{}dummyvec", indent.clone())?;
            for (i, node) in nodes.iter().enumerate() {
                writeln!(w, "{}[{}]:", indent.clone(), i)?;
                dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        Node::Assign(items, node) => {
            writeln!(w, "{}assign to {{ {} }}:", indent, items.join(" -> "))?;
            dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
        }
        Node::OpAssign(items, op, node) => {
            writeln!(w, "{}assign and {} to {{ {} }}:", indent, op, items.join(" -> "))?;
            dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
        }
        Node::If(expr, then_block, elifs, else_block) => {
            writeln!(w, "{}if", indent)?;
            writeln!(w, "{}expr:", indent)?;
            dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;

            writeln!(w, "{}then:", indent)?;
            for (i, node) in then_block.iter().enumerate() {
                writeln!(w, "{}[{}]:", indent.clone(), i)?;
                dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
            }

            if !elifs.is_empty() {
                for (i, (expr, block)) in elifs.iter().enumerate() {
                    writeln!(w, "{}elif[{}]", indent.clone(), i)?;
                    writeln!(w, "{}expr:", indent)?;
                    dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;
                    for (i, node) in block.iter().enumerate() {
                        writeln!(w, "{}[{}]:", indent.clone(), i)?;
                        dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
                    }
                }
            }

            if !else_block.is_empty() {
                writeln!(w, "{}else:", indent)?;
                for (i, node) in else_block.iter().enumerate() {
                    writeln!(w, "{}[{}]:", indent.clone(), i)?;
                    dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
                }
            }
        }
        Node::For(var, start, stop, step, block) => {
            writeln!(w, "{}for {{{}}}", indent, var.join(" -> "))?;
            writeln!(w, "{}start expr:", indent)?;
            dump_expr(start, w, indent.clone() + DEBUG_INDENT)?;
            writeln!(w, "{}stop expr:", indent)?;
            dump_expr(stop, w, indent.clone() + DEBUG_INDENT)?;
            if let Some(step) = step {
                writeln!(w, "{}step expr:", indent)?;
                dump_expr(step, w, indent.clone() + DEBUG_INDENT)?;
            }
            for (i, node) in block.iter().enumerate() {
                writeln!(w, "{}[{}]:", indent.clone(), i)?;
                dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        Node::ForIn(var, array, block) => {
            writeln!(w, "{}for {{{}}} in:", indent, var.join(" -> "))?;
            dump_expr(array, w, indent.clone() + DEBUG_INDENT)?;
            for (i, node) in block.iter().enumerate() {
                writeln!(w, "{}[{}]:", indent.clone(), i)?;
                dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        Node::VarDecl(vars) => {
            for (name, vartype, init) in vars {
                if let Some(node) = init {
                    writeln!(w, "{}var {} type {} init:", indent, name, vartype)?;
                    dump_expr(node, w, indent.clone() + DEBUG_INDENT)?;
                } else {
                    writeln!(w, "{}var {} type {}", indent, name, vartype)?;
                }
            }
        }
        Node::ConstDecl(vars) => {
            for (name, consttype, init) in vars {
                writeln!(w, "{}const {} type {}:", indent, name, consttype)?;
                dump_expr(init, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        Node::While(cond, block) => {
            writeln!(w, "{}while", indent)?;
            writeln!(w, "{}condition:", indent)?;
            dump_expr(cond, w, indent.clone() + DEBUG_INDENT)?;
            for (i, node) in block.iter().enumerate() {
                writeln!(w, "{}[{}]:", indent.clone(), i)?;
                dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        Node::Return(expr) => {
            writeln!(w, "{}return:", indent)?;
            dump_expr(expr, w, indent.clone() + DEBUG_INDENT)?;
        }
    }
    Ok(())
}

#[allow(dead_code)]
pub fn debug_dump<P: AsRef<Path>>(root: &Node, output_file: P) -> anyhow::Result<()> {
    let out = File::create(output_file)?;
    let mut wr = BufWriter::new(out);
    dump_node(root, &mut wr, String::from(""))?;
    wr.flush()?;
    Ok(())
}

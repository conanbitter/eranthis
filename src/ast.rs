#[derive(Debug)]
pub enum Node {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    BinOp(BinOp, Box<Node>, Box<Node>),
    UnOp(UnOp, Box<Node>),
    Var(Vec<String>),
    FnCall(Vec<String>, Vec<Node>),
    Assign(Vec<String>, Box<Node>),
    OpAssign(Vec<String>, BinOp, Box<Node>),
    If(Box<Node>, Vec<Node>, Vec<(Node, Vec<Node>)>, Vec<Node>), // if (expr) (then block) (elifs blocks) (else block)
    For(Vec<String>, Box<Node>, Box<Node>, Option<Box<Node>>, Vec<Node>), // for (var) (start) (stop) (step) (block)
    ForIn(Vec<String>, Box<Node>, Vec<Node>),                    // for (var) in (array) (block)
    VarDecl(Vec<(String, DataType, Option<Node>)>),              // var (name) (type) [=(expr)]
    Dummy,
    DummyVec(Vec<Node>),
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

const DEBUG_INDENT: &str = "    ";

fn dump_node(node: &Node, w: &mut BufWriter<File>, indent: String) -> anyhow::Result<()> {
    match node {
        Node::IntLiteral(v) => {
            writeln!(w, "{}int {}", indent, v)?;
        }
        Node::FloatLiteral(v) => {
            writeln!(w, "{}float {}", indent, v)?;
        }
        Node::StringLiteral(v) => {
            writeln!(w, "{}string {:?}", indent, v)?;
        }
        Node::BoolLiteral(v) => {
            writeln!(w, "{}bool {}", indent, if *v { "true" } else { "false" })?;
        }
        Node::Var(items) => {
            writeln!(w, "{}var {{ {} }}", indent, items.join(" -> "))?;
        }
        Node::FnCall(items, params) => {
            writeln!(w, "{}func {{ {} }}", indent, items.join(" -> "))?;
            for (i, node) in params.iter().enumerate() {
                writeln!(w, "{}param[{}]:", indent.clone(), i)?;
                dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        Node::Dummy => {
            writeln!(w, "{}dummy", indent)?;
        }
        Node::BinOp(bin_op, node_left, node_right) => {
            writeln!(w, "{}binop '{}'", indent.clone(), bin_op)?;
            writeln!(w, "{}left:", indent.clone())?;
            dump_node(node_left, w, indent.clone() + DEBUG_INDENT)?;
            writeln!(w, "{}right:", indent.clone())?;
            dump_node(node_right, w, indent.clone() + DEBUG_INDENT)?;
        }
        Node::UnOp(un_op, node) => {
            writeln!(w, "{}unop '{}':", indent.clone(), un_op)?;
            dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
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
            dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
        }
        Node::OpAssign(items, op, node) => {
            writeln!(w, "{}assign and {} to {{ {} }}:", indent, op, items.join(" -> "))?;
            dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
        }
        Node::If(expr, then_block, elifs, else_block) => {
            writeln!(w, "{}if", indent)?;
            writeln!(w, "{}expr:", indent)?;
            dump_node(expr, w, indent.clone() + DEBUG_INDENT)?;

            writeln!(w, "{}then:", indent)?;
            for (i, node) in then_block.iter().enumerate() {
                writeln!(w, "{}[{}]:", indent.clone(), i)?;
                dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
            }

            if !elifs.is_empty() {
                for (i, (expr, block)) in elifs.iter().enumerate() {
                    writeln!(w, "{}elif[{}]", indent.clone(), i)?;
                    writeln!(w, "{}expr:", indent)?;
                    dump_node(expr, w, indent.clone() + DEBUG_INDENT)?;
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
            dump_node(start, w, indent.clone() + DEBUG_INDENT)?;
            writeln!(w, "{}stop expr:", indent)?;
            dump_node(stop, w, indent.clone() + DEBUG_INDENT)?;
            if let Some(step) = step {
                writeln!(w, "{}step expr:", indent)?;
                dump_node(step, w, indent.clone() + DEBUG_INDENT)?;
            }
            for (i, node) in block.iter().enumerate() {
                writeln!(w, "{}[{}]:", indent.clone(), i)?;
                dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        Node::ForIn(var, array, block) => {
            writeln!(w, "{}for {{{}}} in:", indent, var.join(" -> "))?;
            dump_node(array, w, indent.clone() + DEBUG_INDENT)?;
            for (i, node) in block.iter().enumerate() {
                writeln!(w, "{}[{}]:", indent.clone(), i)?;
                dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
            }
        }
        Node::VarDecl(vars) => {
            for (name, vartype, init) in vars {
                if let Some(node) = init {
                    writeln!(w, "{}var {} type {} init:", indent, name, vartype)?;
                    dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
                } else {
                    writeln!(w, "{}var {} type {}", indent, name, vartype)?;
                }
            }
        }
    }
    Ok(())
}

pub fn debug_dump<P: AsRef<Path>>(root: &Node, output_file: P) -> anyhow::Result<()> {
    let out = File::create(output_file)?;
    let mut wr = BufWriter::new(out);
    dump_node(root, &mut wr, String::from(""))?;
    wr.flush()?;
    Ok(())
}

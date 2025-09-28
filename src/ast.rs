#[derive(Debug)]
pub enum Node {
    IntLiteral(i64),
    FloatLiteral(f64),
    BinOp(BinOp, Box<Node>, Box<Node>),
    UnOp(UnOp, Box<Node>),
    Var(Vec<String>),
    FnCall(Vec<String>),
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

const DEBUG_INDENT: &str = "    ";

fn dump_node(node: &Node, w: &mut BufWriter<File>, indent: String) -> anyhow::Result<()> {
    match node {
        Node::IntLiteral(v) => {
            writeln!(w, "{}int:{}", indent, v)?;
        }
        Node::FloatLiteral(v) => {
            writeln!(w, "{}float:{}", indent, v)?;
        }
        Node::Var(items) => {
            writeln!(w, "{}var {{ {} }}", indent, items.join(" -> "))?;
        }
        Node::FnCall(items) => {
            writeln!(w, "{}func {{ {} }}", indent, items.join(" -> "))?;
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
            writeln!(w, "{}dummyvec:", indent.clone())?;
            for (i, node) in nodes.iter().enumerate() {
                writeln!(w, "[{}]", i)?;
                dump_node(node, w, indent.clone() + DEBUG_INDENT)?;
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

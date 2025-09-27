#[derive(Debug)]
pub enum Node {
    IntLiteral(i64),
    FloatLiteral(f64),
    BinOp(BinOp, Box<Node>, Box<Node>),
    UnOp(UnOp, Box<Node>),
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

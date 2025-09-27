use pomelo::pomelo;

pomelo! {
     %include {
        use crate::ast::Node;
        use crate::lexer::FilePos;
    }

    %token #[derive(Clone,Debug,PartialEq)] pub enum Token {};

    %type Name String;
    %type Str String;
    %type Int i64;
    %type Float f64;
    %type Eof;

    %type root Node;
    %type expr Node;

    root ::= expr;

    expr ::= Int(v) { Node::IntLiteral(v) };
    expr ::= Float(v) { Node::FloatLiteral(v) };


    root ::= NewLine { Node::Dummy };
    root ::= Indent { Node::Dummy };
    root ::= Dedent { Node::Dummy };
    root ::= Name { Node::Dummy };
    root ::= Str { Node::Dummy };

    root ::= KwAnd { Node::Dummy };
    root ::= KwConst { Node::Dummy };
    root ::= KwElif { Node::Dummy };
    root ::= KwElse { Node::Dummy };
    root ::= KwFor { Node::Dummy };
    root ::= KwFunc { Node::Dummy };
    root ::= KwIf { Node::Dummy };
    root ::= KwIn { Node::Dummy };
    root ::= KwNot { Node::Dummy };
    root ::= KwOr { Node::Dummy };
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
    root ::= Add { Node::Dummy };
    root ::= AddAssign { Node::Dummy };
    root ::= Sub { Node::Dummy };
    root ::= SubAssign { Node::Dummy };
    root ::= Mul { Node::Dummy };
    root ::= MulAssign { Node::Dummy };
    root ::= Div { Node::Dummy };
    root ::= DivAssign { Node::Dummy };
    root ::= Mod { Node::Dummy };
    root ::= ModAssign { Node::Dummy };
    root ::= Less { Node::Dummy };
    root ::= LessOrEq { Node::Dummy };
    root ::= Greater { Node::Dummy };
    root ::= GreaterOrEq { Node::Dummy };
    root ::= Eq { Node::Dummy };
    root ::= NotEq { Node::Dummy };
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

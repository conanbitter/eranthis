use pomelo::pomelo;

pomelo! {
    %token #[derive(Clone,Debug,PartialEq)] pub enum Token {};

    %type Name String;
    %type Str String;
    %type Int i64;
    %type Float f64;
    %type Eof;

    root ::= NewLine;
    root ::= Indent;
    root ::= Dedent;
    root ::= Name;
    root ::= Int;
    root ::= Float;
    root ::= Str;

    root ::= KwAnd;
    root ::= KwConst;
    root ::= KwElif;
    root ::= KwElse;
    root ::= KwFor;
    root ::= KwFunc;
    root ::= KwIf;
    root ::= KwIn;
    root ::= KwNot;
    root ::= KwOr;
    root ::= KwRef;
    root ::= KwReturn;
    root ::= KwStep;
    root ::= KwStruct;
    root ::= KwThen;
    root ::= KwTo;
    root ::= KwType;
    root ::= KwVar;
    root ::= KwWhile;

    root ::= Assign;
    root ::= Add;
    root ::= AddAssign;
    root ::= Sub;
    root ::= SubAssign;
    root ::= Mul;
    root ::= MulAssign;
    root ::= Div;
    root ::= DivAssign;
    root ::= Mod;
    root ::= ModAssign;
    root ::= Less;
    root ::= LessOrEq;
    root ::= Greater;
    root ::= GreaterOrEq;
    root ::= Eq;
    root ::= NotEq;
    root ::= Comma;
    root ::= Period;
    root ::= Colon;
    root ::= LParen;
    root ::= RParen;
    root ::= LSqBracket;
    root ::= RSqBracket;
}

pub use parser::Token;

use pomelo::pomelo;

pomelo! {
    %token #[derive(Clone,Debug)] pub enum Token {};

    %type Name String;

    root ::= NewLine;
    root ::= Indent;
    root ::= Dedent;
    root ::= Name;

    root ::= KwConst;
    root ::= KwElif;
    root ::= KwElse;
    root ::= KwFor;
    root ::= KwFunc;
    root ::= KwIf;
    root ::= KwIn;
    root ::= KwRef;
    root ::= KwReturn;
    root ::= KwStep;
    root ::= KwStruct;
    root ::= KwThen;
    root ::= KwTo;
    root ::= KwType;
    root ::= KwVar;
    root ::= KwWhile;
}

pub use parser::Token;

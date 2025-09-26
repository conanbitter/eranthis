use pomelo::pomelo;

pomelo! {
    %token #[derive(Clone,Debug)] pub enum Token {};

    root ::= KwTest;
    root ::= NewLine;
    root ::= Indent;
    root ::= Dedent;
}

pub use parser::Token;

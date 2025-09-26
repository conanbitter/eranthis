use pomelo::pomelo;

pomelo! {
    %token #[derive(Clone,Debug)] pub enum Token {};

    root ::= KwTest
}

pub use parser::Token;

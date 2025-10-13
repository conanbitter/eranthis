use miette::{Diagnostic, SourceOffset, SourceSpan};
use thiserror::Error;

use crate::parser::Token;

#[derive(Error, Diagnostic, Debug)]
pub enum LexicalError {
    #[error("Unexpected symbol \"{symbol:}\"")]
    #[diagnostic(code("lexical::unexpected_symbol"))]
    UnexpectedSymbol {
        symbol: char,
        #[label("this symbol")]
        pos: SourceOffset,
    },

    #[error("Expecting space, operator or end of line before \"{symbol:}\"")]
    #[diagnostic(code("lexical::expecting_separator"))]
    ExpectingSeparator {
        symbol: char,
        #[label("before this")]
        pos: SourceOffset,
    },

    #[error("Can't parse HEX value")]
    #[diagnostic(code("lexical::hex_parse"))]
    HexParse {
        #[label("this value")]
        pos: SourceSpan,
    },

    #[error("Unexpected end of string")]
    #[diagnostic(code("lexical::unexpected_eos"))]
    UnexpectedEos {
        #[label("here")]
        pos: SourceOffset,
    },
}

#[derive(Error, Diagnostic, Debug)]
pub enum SyntaxError {
    #[error("Got {token:} \nexpecting {expecting:}")]
    #[diagnostic(code("syntax::unexpected_token"))]
    UnexpectedToken {
        token: Token,
        expecting: String,
        #[label("this is unexpected")]
        pos: SourceSpan,
    },

    #[error("Expected {expecting:}")]
    #[diagnostic(code("syntax::unexpected_other"))]
    UnexpectedSomething {
        expecting: String,
        #[label("at this")]
        pos: SourceOffset,
    },

    #[error("Total parser fail")]
    #[diagnostic(code("syntax::total_fail"))]
    TotalFail {
        #[label("at this point")]
        pos: SourceOffset,
    },

    #[error("Parser stack overflow")]
    #[diagnostic(code("syntax::stack_overflow"))]
    StackOverflow {
        #[label("at this point")]
        pos: SourceOffset,
    },
}

use miette::{Diagnostic, SourceOffset, SourceSpan};
use thiserror::Error;

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

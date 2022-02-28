//! Low level raw token
//! Represents chunks of texts without distinguishing between their grammatical
//! meaning

pub use self::TokenKind::*;

/// Raw token
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub size: usize,
}

impl Token {
    pub fn new(kind: TokenKind, size: usize) -> Self {
        Token { kind, size }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Whitespace,
    Identifier,
    IntLiteral {
        base: usize,
    },
    FloatLiteral {
        base: usize,
    },
    StringLiteral {
        terminated: bool,
    },
    CharLiteral {
        terminated: bool,
    },
    Comment,

    /// :
    Colon,
    /// ;
    Semicolon,
    /// ,
    Comma,
    /// .
    Dot,
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Asterisk,
    /// /
    Slash,
    /// !
    Exclamation,
    /// @
    At,
    /// $
    Dollar,
    /// %
    Percent,
    /// ^
    Caret,
    /// &
    Ampersand,
    /// |
    Pipe,
    /// ~
    Tilde,
    /// =
    Eq,
    /// <
    Lt,
    /// >
    Gt,

    /// (
    LeftParen,
    /// )
    RightParen,
    /// [
    LeftBracket,
    /// ]
    RightBracket,
    /// {
    LeftBrace,
    /// }
    RightBrace,

    /// End of file
    Eof,

    /// Any sequence that isn't expected by the lexer
    Unknown,
}

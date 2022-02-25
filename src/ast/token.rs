use crate::source::*;
pub use BinOpKind::*;
pub use DelimKind::*;
pub use KeywordKind::*;
pub use LiteralKind::*;
pub use TokenKind::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Identifier,
    Comment,

    Colon,
    Semicolon,
    Comma,
    Dot,
    Minus,
    Exclamation,
    At,
    Not,
    Eq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    NotEq,
    EqEq,

    Keyword(KeywordKind),
    Literal(LiteralKind),
    OpenDelim(DelimKind),
    CloseDelim(DelimKind),
    BinOp(BinOpKind),
    BinOpEq(BinOpKind),
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum LiteralKind {
    Int { base: usize },
    Float { base: usize },
    String,
    Char,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum KeywordKind {
    Let,
    Fn,
}
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DelimKind {
    Paren,
    Bracket,
    Brace,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Or,
    Xor,
    Iff,
    Implies,
    Mod,
    Shl,
    Shr,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
}

pub fn glue_tokens(left: Token, right: Token) -> Option<Token> {
    if let Some(new_kind) = glue_kinds(left.kind, right.kind) {
        return Some(Token {
            kind: new_kind,
            span: span_union(left.span, right.span),
        });
    }
    None
}

pub fn glue_kinds(left: TokenKind, right: TokenKind) -> Option<TokenKind> {
    match (left, right) {
        (Lt, Eq) => Some(LtEq),
        (Gt, Eq) => Some(GtEq),
        (Eq, Eq) => Some(EqEq),
        (Exclamation, Eq) => Some(NotEq),
        (Lt, Lt) => Some(BinOp(Shl)),
        (Gt, Gt) => Some(BinOp(Shr)),
        (BinOp(kind), Eq) => Some(BinOpEq(kind)),
        _ => None,
    }
}

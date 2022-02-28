/// Lexer module
#[cfg(test)]
mod tests;

use std::sync::Arc;

use crate::ast::token as ast;
use crate::errors::*;
use crate::source::*;

mod errors;
use errors::*;

mod raw;
use raw::*;

mod token;
use token::*;

// Helper macros to shorten notation
macro_rules! bin_op {
    ($op:ident) => {
        ast::BinOp(ast::$op)
    };
}

macro_rules! lit {
    ($op:ident) => {
        ast::Literal(ast::LiteralKind::$op)
    };
}

macro_rules! kw {
    ($op:ident) => {
        ast::Keyword(ast::KeywordKind::$op)
    };
}

macro_rules! open_delim {
    ($op:ident) => {
        ast::OpenDelim(ast::DelimKind::$op)
    };
}

macro_rules! close_delim {
    ($op:ident) => {
        ast::CloseDelim(ast::DelimKind::$op)
    };
}

pub struct Lexer<'a> {
    source: Arc<Source>,
    data: &'a str,
    index: usize,
}

type LexingResult = CompilingResult<Vec<ast::Token>>;
type PartialLexingResult<T> = PartialCompilingResult<T>;

impl<'a> Lexer<'a> {
    pub fn new(source: &'a Arc<Source>) -> Self {
        Self {
            source: source.clone(),
            data: &source.data,
            index: 0,
        }
    }

    /// Converts low-level tokens into richer ones used by ast
    pub fn lex(&mut self) -> LexingResult {
        let mut tokens = Vec::new();
        let mut report = ErrorReport::new();

        while self.index < self.data.len() {
            self.skip_whitespace();
            let raw_token = lex_single_token(&self.data[self.index..]);
            let result = self.convert_raw(raw_token);
            match result {
                Ok(token) => {
                    self.push_token(&mut tokens, token);
                }
                Err(err) => {
                    report.add(err);
                }
            };
            self.index += raw_token.size;
            self.skip_whitespace();
        }
        if report.empty() {
            return Ok(tokens);
        } else {
            return Err(report);
        }
    }

    pub fn push_token(&mut self, tokens: &mut Vec<ast::Token>, token: ast::Token) {
        if let Some(glued) = tokens
            .last()
            .and_then(|&last| ast::glue_tokens(last, token))
        {
            tokens.pop();
            tokens.push(glued);
        } else {
            tokens.push(token);
        }
    }

    fn skip_whitespace(&mut self) {
        let token = lex_single_token(&self.data[self.index..]);
        match token.kind {
            Whitespace => self.index += token.size,
            _ => {}
        };
    }

    fn current_span(&self, size: usize) -> Span {
        Span {
            begin: self.index,
            size,
        }
    }

    fn convert_raw(&self, token: Token) -> PartialLexingResult<ast::Token> {
        let span = self.current_span(token.size);
        let content: &str = &self.data[self.index..self.index + token.size];
        let kind = match token.kind {
            // Symbols
            Comment => ast::Comment,
            Colon => ast::Colon,
            Semicolon => ast::Semicolon,
            Comma => ast::Comma,
            Dot => ast::Dot,
            Minus => ast::Minus,
            Exclamation => ast::Exclamation,
            At => ast::At,
            Eq => ast::Eq,
            Lt => ast::Lt,
            Gt => ast::Gt,

            // Operators
            Plus => bin_op!(Add),
            Asterisk => bin_op!(Mul),
            Slash => bin_op!(Div),
            Percent => bin_op!(Mod),
            Pipe => bin_op!(BitwiseOr),
            Ampersand => bin_op!(BitwiseAnd),
            Caret => bin_op!(BitwiseXor),

            // Delimeters
            LeftParen => open_delim!(Paren),
            RightParen => close_delim!(Paren),
            LeftBracket => open_delim!(Bracket),
            RightBracket => close_delim!(Bracket),
            LeftBrace => open_delim!(Brace),
            RightBrace => close_delim!(Brace),

            // Literals
            IntLiteral { base } => ast::Literal(ast::Int { base }),
            FloatLiteral { base } => ast::Literal(ast::Float { base }),
            Identifier => self.process_identifier(span),
            StringLiteral { terminated } => self.make_string_literal(terminated, span)?,
            CharLiteral { terminated } => self.make_char_literal(terminated, span)?,

            // If nothing was matched then emit an error
            _ => return Err(unknown_token(self.source.clone(), content, span)),
        };
        Ok(ast::Token { kind, span })
    }

    fn make_string_literal(
        &self,
        terminated: bool,
        span: Span,
    ) -> PartialLexingResult<ast::TokenKind> {
        match terminated {
            true => Ok(lit!(String)),
            false => Err(unterminated_string_literal(self.source.clone(), span)),
        }
    }

    fn make_char_literal(
        &self,
        terminated: bool,
        span: Span,
    ) -> PartialLexingResult<ast::TokenKind> {
        match terminated {
            true => match self.source.span_string(&span).chars().count() {
                3 => Ok(lit!(Char)),
                _ => Err(char_literal_has_to_contain_one_character(
                    self.source.clone(),
                    span,
                )),
            },
            false => Err(unterminated_char_literal(self.source.clone(), span)),
        }
    }

    fn process_identifier(&self, span: Span) -> ast::TokenKind {
        let data = self.source.span_string(&span);
        match data {
            "not" => ast::Not,
            "and" => bin_op!(And),
            "or" => bin_op!(Or),
            "xor" => bin_op!(Xor),
            "iff" => bin_op!(Iff),
            "implies" => bin_op!(Implies),
            "let" => kw!(Let),
            "fn" => kw!(Fn),
            _ => ast::Identifier,
        }
    }
}

//! Errors that may occur during lexing

use crate::errors::*;
use crate::source::*;

use std::sync::Arc;

/// Met character unrecongizable by the lexer
pub fn unknown_token(source: Arc<Source>, token: &str, span: Span) -> CompilerError {
    CompilerError::error(CompilerError::UNKNOWN_TOKEN)
        .add_span(span)
        .add_source(source)
        .add_message(format!("Unknown token: `{}`", token))
        .build()
}

/// String literal was not terminated
pub fn unterminated_string_literal(source: Arc<Source>, span: Span) -> CompilerError {
    CompilerError::error(CompilerError::UNTERMINATED_STRING_LITERAL)
        .add_span(span)
        .add_source(source)
        .add_message("Unterminated string literal".to_string())
        .build()
}

/// Char literal was not terminated
pub fn unterminated_char_literal(source: Arc<Source>, span: Span) -> CompilerError {
    CompilerError::error(CompilerError::UNTERMINATED_CHAR_LITERAL)
        .add_span(span)
        .add_source(source)
        .add_message("Unterminated char literal".to_string())
        .build()
}

/// Char literal has to contain one character
pub fn char_literal_has_to_contain_one_character(source: Arc<Source>, span: Span) -> CompilerError {
    CompilerError::error(CompilerError::CHAR_LITERAL_HAS_TO_CONTAIN_ONE_CHARACTER)
        .add_span(span)
        .add_source(source)
        .add_message("Char literal has to contain one character".to_string())
        .build()
}

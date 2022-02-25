/// Lexer module
#[cfg(test)]
mod tests;

use std::sync::Arc;

use self::TokenKind::*;
use crate::ast::token as ast;
use crate::errors::*;
use crate::source::*;

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

/// Raw token
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Token {
    kind: TokenKind,
    size: usize,
}

impl Token {
    fn new(kind: TokenKind, size: usize) -> Self {
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

/// Lex first token from input
fn lex_single_token(data: &str) -> Token {
    let next = match data.chars().next() {
        Some(c) => c,
        None => return Token::new(Eof, 0),
    };
    match next {
        ':' => Token::new(Colon, 1),
        ';' => Token::new(Semicolon, 1),
        ',' => Token::new(Comma, 1),
        '.' => Token::new(Dot, 1),
        '+' => Token::new(Plus, 1),
        '-' => Token::new(Minus, 1),
        '*' => Token::new(Asterisk, 1),
        '/' => Token::new(Slash, 1),
        '!' => Token::new(Exclamation, 1),
        '@' => Token::new(At, 1),
        '$' => Token::new(Dollar, 1),
        '%' => Token::new(Percent, 1),
        '^' => Token::new(Caret, 1),
        '&' => Token::new(Ampersand, 1),
        '|' => Token::new(Pipe, 1),
        '=' => Token::new(Eq, 1),
        '<' => Token::new(Lt, 1),
        '>' => Token::new(Gt, 1),
        '(' => Token::new(LeftParen, 1),
        ')' => Token::new(RightParen, 1),
        '[' => Token::new(LeftBracket, 1),
        ']' => Token::new(RightBracket, 1),
        '{' => Token::new(LeftBrace, 1),
        '}' => Token::new(RightBrace, 1),
        '~' => Token::new(Tilde, 1),
        '#' => lex_line_comment(data),
        '"' => lex_string_literal(data),
        '\'' => lex_char_literal(data),
        c if c.is_digit(10) => lex_number_literal(data),
        c if is_identifier_start(c) => lex_identifier(data),
        c if is_whitespace(c) => lex_whitespace(data),
        _ => Token::new(Unknown, next.len_utf8()),
    }
}

fn lex_line_comment(data: &str) -> Token {
    // Ensure that first character is a pound
    debug_assert_eq!(data.chars().next().unwrap(), '#');
    let size = eat_while(data, |c| c != '\n').len();
    Token {
        kind: Comment,
        size,
    }
}

enum QuoteKind {
    Single,
    Double,
}

fn lex_string_literal(data: &str) -> Token {
    lex_quoted_literal(data, QuoteKind::Double)
}

fn lex_char_literal(data: &str) -> Token {
    lex_quoted_literal(data, QuoteKind::Single)
}

fn lex_quoted_literal(data: &str, quote_kind: QuoteKind) -> Token {
    let quote = match quote_kind {
        QuoteKind::Single => '\'',
        QuoteKind::Double => '"',
    };
    // Ensure that first character is a quote
    debug_assert_eq!(data.chars().next().unwrap(), quote);
    let mut lexed_size = 1; // Skip first  quote
    let mut terminated = false;
    // Consume characters until reached unescaped quote
    while lexed_size < data.len() {
        let next_size = size_of_next_character_symbol(&data[lexed_size..]);

        let begin = lexed_size;
        let end = lexed_size + next_size;
        lexed_size += next_size;

        if &data[begin..end] == quote.to_string() {
            terminated = true;
            break;
        }
    }

    let kind = match quote_kind {
        QuoteKind::Single => CharLiteral { terminated },
        QuoteKind::Double => StringLiteral { terminated },
    };

    Token {
        kind,
        size: lexed_size,
    }
}

/// Calculates size of next character symbol
/// Escaped characters count as one i.e `\n` is of size 2
fn size_of_next_character_symbol(data: &str) -> usize {
    match data.chars().next() {
        None => 0,
        Some('\\') => data.chars().take(2).map(|c| c.len_utf8()).sum(),
        Some(c) => c.len_utf8(),
    }
}

fn lex_number_literal(data: &str) -> Token {
    // Ensure that first character is a  decimal digit
    debug_assert!(data.chars().next().unwrap().is_digit(10));

    let first_two: String = data.chars().take(2).collect();
    let (base, prefix_size) = match first_two.as_str() {
        "0b" => (2, 2),
        "0o" => (8, 2),
        "0x" => (16, 2),
        _ => (10, 0),
    };

    let chars = data.chars().skip(prefix_size);

    let digits_before_dot = chars
        .clone()
        .take_while(|c| c.is_digit(base))
        .map(|c| c.len_utf8())
        .sum();

    let chars = chars.skip(digits_before_dot);
    let dot_size = chars
        .clone()
        .take(1)
        .filter(|&c| c == '.')
        .map(|c| c.len_utf8())
        .sum();

    let digits_after_dot: usize = chars
        .skip(dot_size)
        .take_while(|c| c.is_digit(base))
        .map(|c| c.len_utf8())
        .sum();

    let is_float = dot_size == 1 && digits_after_dot > 0;
    let mut size = prefix_size + digits_before_dot;
    let kind = if is_float {
        size += dot_size + digits_after_dot;
        FloatLiteral {
            base: base as usize,
        }
    } else {
        IntLiteral {
            base: base as usize,
        }
    };

    Token { kind, size }
}

fn lex_identifier(data: &str) -> Token {
    // Ensure that first character is a letter
    debug_assert!(is_identifier_start(data.chars().next().unwrap()));
    Token {
        kind: Identifier,
        size: eat_while(data, is_identifier_char).len(),
    }
}

fn lex_whitespace(data: &str) -> Token {
    // Ensure that first character is whitespace
    debug_assert!(is_whitespace(data.chars().next().unwrap()));
    let size = eat_while(data, is_whitespace).len();
    Token {
        kind: Whitespace,
        size,
    }
}

/// Take maximal prefix of characters matching predicate
fn eat_while<P>(data: &str, mut f: P) -> &str
where
    P: FnMut(char) -> bool,
{
    let size = data.chars().take_while(|c| f(*c)).collect::<String>().len();
    &data[..size]
}

/// true if given character is considered as whitespace by lexer
fn is_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\r' | '\n')
}

/// true if an identifier can start with the character
fn is_identifier_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}

/// true if an identifier can contain the character after first character
fn is_identifier_continue(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}

/// true if an identifier can contain the character anywhere
fn is_identifier_char(c: char) -> bool {
    is_identifier_start(c) || is_identifier_continue(c)
}

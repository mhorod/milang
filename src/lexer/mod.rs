//! Low level lexer for basic chunks of text such as
//! - symbols
//! - identifiers
//! - literals

#[cfg(test)]
mod tests;

use self::TokenKind::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Token {
    kind: TokenKind,
    size: usize,
}

impl Token {
    fn new(kind: TokenKind, size: usize) -> Self {
        Token { kind, size }
    }
    fn empty() -> Self {
        Token {
            kind: Empty,
            size: 0,
        }
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

    Comment,

    Colon,
    Semicolon,
    Comma,
    Dot,
    Plus,
    Minus,
    Asterisk,
    Slash,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    /// End of file
    Eof,

    /// Any sequence that isn't expected by the lexer
    Unknown,

    /// Helper token that does not contain anything
    Empty,
}

/// Lex all tokens from input string
pub fn lex(data: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut index = 0;
    let mut remaining_size = data.len();
    while remaining_size > 0 {
        let token = lex_single_token(&data[index..]);
        index += token.size;
        remaining_size -= token.size;
        tokens.push(token);
    }
    tokens
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
        '(' => Token::new(LeftParen, 1),
        ')' => Token::new(RightParen, 1),
        '[' => Token::new(LeftBracket, 1),
        ']' => Token::new(RightBracket, 1),
        '{' => Token::new(LeftBrace, 1),
        '}' => Token::new(RightBrace, 1),
        '"' => lex_string_literal(data),
        c if c.is_digit(10) => lex_number_literal(data),
        c if is_identifier_start(c) => lex_identifier(data),
        c if is_whitespace(c) => Token::new(Whitespace, c.len_utf8()),
        _ => Token::new(Unknown, next.len_utf8()),
    }
}

fn lex_string_literal(data: &str) -> Token {
    // Ensure that first character is a double quote
    match data.chars().next() {
        Some('"') => {}
        None | Some(_) => return Token::empty(),
    };

    let mut lexed_size = 1; // Skip first double quote
    let mut terminated = false;
    // Consume characters until reached unescaped double quote
    while lexed_size < data.len() {
        let next_size = size_of_next_symbol_in_string_literal(&data[lexed_size..]);

        let begin = lexed_size;
        let end = lexed_size + next_size;
        lexed_size += next_size;

        if &data[begin..end] == "\"" {
            terminated = true;
            break;
        }
    }

    Token {
        kind: StringLiteral { terminated },
        size: lexed_size,
    }
}

/// Calculates size of next symbol inside string literal
/// Escaped characters count as one i.e `\n` is of size 2
fn size_of_next_symbol_in_string_literal(data: &str) -> usize {
    match data.chars().next() {
        None => 0,
        Some('\\') => data.chars().take(2).map(|c| c.len_utf8()).sum(),
        Some(c) => c.len_utf8(),
    }
}

fn lex_number_literal(data: &str) -> Token {
    // Ensure that first character is a digit
    match data.chars().next() {
        Some(c) if !c.is_digit(10) => return Token::empty(),
        None => return Token::empty(),
        Some(_) => {}
    };

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
    match data.chars().next().map(is_identifier_start) {
        Some(false) | None => return Token::empty(),
        Some(true) => Token {
            kind: Identifier,
            size: eat_while(data, is_identifier_char).len(),
        },
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

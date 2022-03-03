use std::sync::Arc;

use crate::source::*;
use crate::utils::color::*;

pub type CompilingResult<T> = Result<T, ErrorReport>;
pub type PartialCompilingResult<T> = Result<T, CompilerError>;

pub struct ErrorReport {
    pub errors: Vec<CompilerError>,
}

impl ErrorReport {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn empty(&self) -> bool {
        self.errors.len() == 0
    }
    pub fn add(&mut self, error: CompilerError) {
        self.errors.push(error);
    }
    pub fn print(&self) {
        self.errors.iter().for_each(print_error);
    }
}

impl<T> From<T> for ErrorReport
where
    T: IntoIterator<Item = CompilerError>,
{
    fn from(errors: T) -> Self {
        let mut report = Self::new();
        errors.into_iter().for_each(|e| report.add(e));
        report
    }
}

pub struct CompilerError {
    pub kind: CompilerErrorKind,
    pub code: u32,
    pub source: Arc<Source>,
    pub span: Span,
    pub message: String,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CompilerErrorKind {
    Error,
    Warning,
}

impl CompilerErrorKind {
    pub fn string(&self) -> String {
        match *self {
            CompilerErrorKind::Error => "error",
            CompilerErrorKind::Warning => "warning",
        }
        .into()
    }
    pub fn letter(&self) -> char {
        match *self {
            CompilerErrorKind::Error => 'E',
            CompilerErrorKind::Warning => 'W',
        }
    }
}

// Error codes
impl CompilerError {
    // Lexical errors
    pub const UNKNOWN_TOKEN: u32 = 0;
    pub const UNTERMINATED_STRING_LITERAL: u32 = 1;
    pub const UNTERMINATED_CHAR_LITERAL: u32 = 2;
    pub const CHAR_LITERAL_HAS_TO_CONTAIN_ONE_CHARACTER: u32 = 3;

    // Syntax errors
    pub const UNEXPECTED_TOKEN: u32 = 1000;
    pub const UNEXPECTED_EOF: u32 = 1001;
}

pub struct CompilerErrorBuilder {
    kind: Option<CompilerErrorKind>,
    code: Option<u32>,
    source: Option<Arc<Source>>,
    span: Option<Span>,
    message: Option<String>,
}

impl CompilerErrorBuilder {
    fn new() -> Self {
        Self {
            kind: None,
            code: None,
            source: None,
            span: None,
            message: None,
        }
    }

    pub fn add_kind(mut self, kind: CompilerErrorKind) -> Self {
        self.kind = Some(kind);
        self
    }

    pub fn add_code(mut self, code: u32) -> Self {
        self.code = Some(code);
        self
    }

    pub fn add_source(mut self, source: Arc<Source>) -> Self {
        self.source = Some(source);
        self
    }

    pub fn add_span<T>(mut self, span: T) -> Self
    where
        T: Into<Span>,
    {
        self.span = Some(span.into());
        self
    }

    pub fn add_message<T>(mut self, message: T) -> Self
    where
        T: Into<String>,
    {
        self.message = Some(message.into());
        self
    }

    /// Construct `CompilerError` from provided data
    pub fn build(self) -> CompilerError {
        CompilerError {
            kind: self.kind.unwrap(),
            code: self.code.unwrap(),
            source: self.source.unwrap(),
            span: self.span.unwrap(),
            message: self.message.unwrap(),
        }
    }
}

impl CompilerError {
    pub fn error(code: u32) -> CompilerErrorBuilder {
        CompilerErrorBuilder::new()
            .add_kind(CompilerErrorKind::Error)
            .add_code(code)
    }
}

/// Print single compiler error to stdout
pub fn print_error(error: &CompilerError) {
    let kind_name = error.kind.string();
    let kind_string = format!("{}[{}{:04}]", kind_name, error.kind.letter(), error.code);

    let spans = error.source.break_span_by_lines(&error.span);
    let lines = error.source.spanned_lines(&error.span);
    let number_length = lines.last().unwrap().to_string().chars().count();

    let location = error.source.index_to_linecol(error.span.begin);
    println!("{}: {}", kind_string.bold().red(), error.message.bold());
    println!(
        "{}:{}:{}",
        error.source.filename, location.line, location.column
    );

    for span in spans.iter() {
        print_underlined_span(error, number_length, *span);
    }
    println!("");
}

/// Print underlined span to stdout.
/// Function expects the span to be contained in a single line
fn print_underlined_span(error: &CompilerError, number_length: usize, span: Span) {
    let location = error.source.index_to_linecol(span.begin);
    let line = error.source.line(location.line);
    let normalized_before = line
        .chars()
        .take(location.column - 1)
        .collect::<String>()
        .normalize_whitespace()
        .chars()
        .count();

    let normalized_length = error
        .source
        .span_string(&span)
        .normalize_whitespace()
        .chars()
        .count();

    let line = line.normalize_whitespace();
    let prefix = format!("{:<length$} | ", location.line, length = number_length);
    let underline_prefix = format!("{:<length$} | ", "", length = number_length);

    let spaces: String = std::iter::repeat(' ').take(normalized_before).collect();
    let carets = std::iter::repeat('^')
        .take(normalized_length)
        .collect::<String>()
        .red()
        .bold();

    println!("{}{}", prefix, line);
    println!("{}{}{}", underline_prefix, spaces, carets);
}

// Normalize whitespace characters for correct display
trait NormalizeWhitespace {
    fn normalize_whitespace(self) -> String;
}

impl<T> NormalizeWhitespace for T
where
    T: Into<String>,
{
    fn normalize_whitespace(self) -> String {
        self.into().replace('\t', "    ") // Replace tab with four spaces for consistent error display
    }
}

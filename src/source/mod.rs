#[cfg(test)]
mod tests;

/// Represents contiguous span of text
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Span {
    /// first byte of the span
    pub begin: usize,
    /// how many bytes it takes
    pub size: usize,
}

impl Span {
    pub fn empty() -> Self {
        Self { begin: 0, size: 0 }
    }
    pub fn new(begin: usize, size: usize) -> Self {
        Self { begin, size }
    }
    pub fn from_range(begin: usize, end: usize) -> Self {
        Self {
            begin,
            size: end - begin,
        }
    }

    pub fn end(&self) -> usize {
        self.begin + self.size
    }
}

impl<T: Into<usize>> From<std::ops::Range<T>> for Span {
    fn from(r: std::ops::Range<T>) -> Self {
        Self::from_range(r.start.into(), r.end.into())
    }
}

pub fn span_union(left: Span, right: Span) -> Span {
    let begin = std::cmp::min(left.begin, right.begin);
    let end = std::cmp::max(left.end(), right.end());
    Span::from_range(begin, end)
}

/// Return union of spans in the iterable if its non-empty
/// None otherwise
pub fn spans_union<T, I>(spans: T) -> Option<Span>
where
    T: IntoIterator<Item = I>,
    I: Into<Span>,
{
    let mut it = spans.into_iter();
    let first = it.next()?;
    Some(it.fold(first.into(), |l, r| span_union(l, r.into())))
}

/// Line and column in source file, both indexed from one
pub struct LineCol {
    pub line: usize,
    pub column: usize,
}

/// Represents file with source code
pub struct Source {
    pub filename: String,
    pub data: String,

    lines: Vec<Span>,
}

impl Source {
    /// Loads source from file
    pub fn from_file(filename: String) -> Source {
        let content = std::fs::read_to_string(&filename).expect("Failed to read file");
        return Source::new(filename, content);
    }

    /// Creates new Source
    pub fn new(filename: String, data: String) -> Self {
        // Convert CRLF and split on new lines
        let lines = data.split_inclusive("\n");

        let lines = lines
            .scan(0, |index, line| {
                let line_size = line.len();

                let line = line.replace("\r\n", "\n");
                let line = line.replace("\n", "");
                let result = Some(Span::new(*index, line.len()));
                *index += line_size;
                result
            })
            .collect();

        Self {
            filename,
            data,
            lines,
        }
    }

    /// Converts byte position in source to line and column
    pub fn index_to_linecol(&self, index: usize) -> LineCol {
        let line = self.lines.binary_search_by_key(&index, |&span| span.begin);
        let line = match line {
            Ok(i) => i,
            Err(i) => i - 1,
        };

        let line_begin = self.lines[line].begin;
        let column = self.data[line_begin..index].chars().count() + 1;
        LineCol {
            line: line + 1, // Lines are indexed from 1
            column: column,
        }
    }

    /// Returns contents of line with given number
    pub fn line(&self, line_number: usize) -> &str {
        let line = &self.lines[line_number - 1];
        &self.data[line.begin..line.end()]
    }

    /// Returns contens represented by the span
    pub fn span_string(&self, span: &Span) -> &str {
        &self.data[span.begin..span.end()]
    }

    /// Returns numbers of lines that intersect the span
    pub fn spanned_lines(&self, span: &Span) -> std::ops::Range<usize> {
        let begin = self.index_to_linecol(span.begin);
        let end = self.index_to_linecol(span.end());
        begin.line..(end.line + 1)
    }

    /// Divides span into sub-spans such that each is contained in a single line
    pub fn break_span_by_lines(&self, span: &Span) -> Vec<Span> {
        let mut result = Vec::new();
        for line_number in self.spanned_lines(span) {
            let line = self.lines[line_number - 1];
            let begin = std::cmp::max(span.begin, line.begin);
            let end = std::cmp::min(span.end(), line.end());
            result.push(Span::from_range(begin, end));
        }
        result
    }

    /// Returns span of length one representing end of file
    pub fn eof_span(&self) -> Span {
        Span::new(self.data.len(), 1)
    }
}

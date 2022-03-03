use super::*;

mod span {
    use super::*;

    #[test]
    fn union_argument_order_does_not_matter() {
        let first = Span { begin: 2, size: 5 };
        let second = Span { begin: 7, size: 5 };
        assert_eq!(span_union(first, second), span_union(second, first));
    }

    #[test]
    fn union_of_two_overlapping_spans() {
        let first = Span { begin: 2, size: 5 };
        let second = Span { begin: 3, size: 5 };
        let expected = Span { begin: 2, size: 6 };
        assert_eq!(span_union(first, second), expected);
    }

    #[test]
    fn union_of_two_nested_spans_is_the_outer_span() {
        let outer = Span { begin: 3, size: 10 };
        let inner = Span { begin: 5, size: 4 };
        assert_eq!(span_union(inner, outer), outer);
    }

    #[test]
    fn created_from_range_ends_on_range_end() {
        let begin = 10;
        let end = 20;
        assert_eq!(Span::from_range(begin, end).end(), end);
    }

    #[test]
    fn spans_union_of_ranges() {
        let spans: Vec<std::ops::Range<usize>> = vec![1..3, 3..5, 2..7];
        let expected = Some(Span::from_range(1, 7));
        assert_eq!(spans_union(spans), expected);
    }

    #[test]
    fn union_of_empty_vector_is_none() {
        let spans: Vec<Span> = Vec::new();
        assert_eq!(spans_union(spans), None);
    }
}

fn source() -> Source {
    let data = r#"ab
cd
ef"#;
    Source::new("test".to_string(), data.to_string())
}

mod break_span_by_lines {
    use super::*;

    #[test]
    fn each_span_spans_one_line() {
        let span = Span::from_range(1, 5);
        let s = source();
        let spans = s.break_span_by_lines(&span);
        for span in spans {
            assert_eq!(s.spanned_lines(&span).len(), 1);
        }
    }
    #[test]
    fn union_of_subspans_is_the_original_span() {
        let span = Span::from_range(0, 6);
        let s = source();
        let spans = s.break_span_by_lines(&span);
        assert_eq!(spans_union(spans), Some(span));
    }
}

#[test]
fn source_lines_are_split_on_line_break() {
    let s = source();
    let lines: Vec<&str> = (1..=3).map(|l| s.line(l)).collect();
    assert_eq!(lines, vec!["ab", "cd", "ef"])
}

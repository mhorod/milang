use super::*;

mod span {
    use super::*;

    #[test]
    fn span_union_argument_order_does_not_matter() {
        let first = Span { begin: 2, size: 5 };
        let second = Span { begin: 7, size: 5 };
        assert_eq!(span_union(first, second), span_union(second, first));
    }

    #[test]
    fn span_union_of_overlapping_spans() {
        let first = Span { begin: 2, size: 5 };
        let second = Span { begin: 3, size: 5 };
        let expected = Span { begin: 2, size: 6 };
        assert_eq!(span_union(first, second), expected);
    }

    #[test]
    fn span_union_of_nested_spans_is_the_outer_span() {
        let outer = Span { begin: 3, size: 10 };
        let inner = Span { begin: 5, size: 4 };
        assert_eq!(span_union(inner, outer), outer);
    }

    #[test]
    fn span_created_from_range_ends_on_range_end() {
        let begin = 10;
        let end = 20;
        assert_eq!(Span::from_range(begin, end).end(), end);
    }
}

mod break_span_by_lines {
    use super::*;

    fn source() -> Source {
        let data = r#"
First line
Second line
Third line
        "#;
        Source::new("test".to_string(), data.to_string())
    }

    #[test]
    fn each_span_spans_one_line() {
        let span = Span::from_range(3, 30);
        let s = source();
        let spans = s.break_span_by_lines(&span);
        for span in spans {
            assert_eq!(s.spanned_lines(&span).len(), 1);
        }
    }
}

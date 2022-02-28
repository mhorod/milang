use std::sync::Arc;

use crate::ast::token as ast;
use crate::errors::*;
use crate::source::Source;

use super::raw::*;
use super::token::*;
use super::Lexer;

fn assert_lexes_to(data: &str, expected: &str) {
    assert_eq!(&data[..lex_single_token(data).size], expected);
}

mod raw {
    use super::*;
    mod identifier {
        use super::*;
        #[test]
        fn can_contain_letters() {
            assert_lexes_to("ident", "ident");
        }

        #[test]
        fn can_contain_underscore() {
            assert_lexes_to("_abc_", "_abc_");
        }

        #[test]
        fn does_not_contain_symbols() {
            assert_lexes_to("abc;", "abc");
        }

        #[test]
        fn does_not_contain_space() {
            assert_lexes_to("abc def", "abc");
        }
    }

    mod string_literal {
        use super::*;

        #[test]
        fn without_escaped_characters() {
            assert_lexes_to(r#""abc""#, r#""abc""#);
        }

        #[test]
        fn ends_on_second_double_quote() {
            assert_lexes_to(r#""abc"1"#, r#""abc""#)
        }
        #[test]
        fn is_unterminated_when_there_is_no_right_delimeter() {
            let data = r#""1234abc()"#;
            let token = lex_single_token(data);
            assert_eq!(
                token,
                Token {
                    kind: StringLiteral { terminated: false },
                    size: data.len(),
                }
            );
        }

        #[test]
        fn unterminated_with_escaped_double_quotes() {
            let data = r#""1234\"abc()\""#;
            let token = lex_single_token(data);
            assert_eq!(
                token,
                Token {
                    kind: StringLiteral { terminated: false },
                    size: data.len(),
                }
            );
        }
        #[test]
        fn terminated_with_escaped_double_quotes() {
            let data = r#""1234\"abc()\"""#;
            let token = lex_single_token(data);
            assert_eq!(
                token,
                Token {
                    kind: StringLiteral { terminated: true },
                    size: data.len(),
                }
            );
        }

        #[test]
        fn with_trailing_backslash() {
            assert_lexes_to(r#""123\"#, r#""123\"#);
        }
    }

    mod number_literal {
        use super::*;

        macro_rules! lit {
            ($content:expr, $kind:ident, $base:expr) => {
                Token {
                    kind: $kind { base: $base },
                    size: $content.len(),
                }
            };
        }

        #[test]
        fn int_base_10_contains_only_digits_0_to_9() {
            let data = "0123456789a";
            let expected = "0123456789";
            assert_eq!(lex_single_token(data), lit!(expected, IntLiteral, 10))
        }

        #[test]
        fn int_base_2_contains_only_digits_0_and_1() {
            let data = "0b1012";
            let expected = "0b101";
            assert_eq!(lex_single_token(data), lit!(expected, IntLiteral, 2))
        }
        #[test]
        fn int_base_8_contains_digits_0_to_7() {
            let data = "0o012345678";
            let expected = "0o01234567";
            assert_eq!(lex_single_token(data), lit!(expected, IntLiteral, 8))
        }
        #[test]
        fn int_base_16_contains_digits_0_to_f() {
            let data = "0x0123456789abcdefg";
            let expected = "0x0123456789abcdef";
            assert_eq!(lex_single_token(data), lit!(expected, IntLiteral, 16))
        }
        #[test]
        fn float_cannot_end_with_dot() {
            let data = "10.";
            let expected = "10";
            assert_eq!(lex_single_token(data), lit!(expected, IntLiteral, 10));
        }

        #[test]
        fn float_cannot_contain_two_consecutive_dots() {
            let data = "10..23";
            let expected = "10";
            assert_eq!(lex_single_token(data), lit!(expected, IntLiteral, 10));
        }

        #[test]
        fn float_base_10_cannot_contain_two_dots() {
            let data = "123.456.789";
            let expected = "123.456";
            assert_eq!(lex_single_token(data), lit!(expected, FloatLiteral, 10));
        }
    }

    mod comment {
        use super::*;

        #[test]
        fn ends_before_line_break() {
            assert_lexes_to("#comment\n", "#comment");
        }
    }
}

mod lexer {
    use super::*;
    fn lex_str(data: &str) -> CompilingResult<Vec<ast::Token>> {
        let source = Source::new("test_file".to_string(), data.to_string());
        let source = Arc::from(source);
        Lexer::new(&source).lex()
    }

    fn kinds(tokens: &Vec<ast::Token>) -> Vec<ast::TokenKind> {
        tokens.iter().map(|t| t.kind).collect()
    }
    #[test]
    fn ignores_whitespace() {
        let result = lex_str("  \tan_identifier\t  ");
        match result {
            Ok(tokens) => assert_eq!(kinds(&tokens), vec![ast::Identifier]),
            Err(_) => panic!("No error should be emitted."),
        }
    }

    #[test]
    fn lexing_unterminated_string_literal_emits_an_error() {
        let result = lex_str(r#""abc"#);
        match result {
            Ok(_) => panic!("An error should be emitted."),
            Err(report) => {
                let err = &report.errors[0];
                assert_eq!(err.kind, CompilerErrorKind::Error);
                assert_eq!(err.code, CompilerError::UNTERMINATED_STRING_LITERAL);
            }
        }
    }
    #[test]
    fn tokens_are_glued_from_left() {
        let result = lex_str("<<=>>=");
        match result {
            Ok(tokens) => assert_eq!(
                kinds(&tokens),
                vec![ast::BinOpEq(ast::Shl), ast::BinOpEq(ast::Shr)]
            ),
            Err(_) => panic!("No error should be emitted."),
        }
    }
}

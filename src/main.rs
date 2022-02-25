mod ast;
mod errors;
mod lexer;
mod source;
mod utils;

use std::sync::Arc;

use lexer::*;

fn main() {
    let data = r#"
    # This is a comment 
    2 + 2 == 4 implies fn x = "1234";
    (array[2] + 0xDEADBEEF){}
    a >>= 3.1415
    'a'
    "abc"
    "#;
    let source = source::Source::new("test_file".to_string(), data.to_string());
    let source = Arc::from(source);
    match Lexer::new(&source).lex() {
        Ok(tokens) => println!("{:?}", kinds(&tokens)),
        Err(report) => report.print(),
    }
}

fn kinds(tokens: &Vec<ast::token::Token>) -> Vec<ast::token::TokenKind> {
    tokens.iter().map(|t| t.kind).collect()
}

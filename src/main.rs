mod lexer;

use lexer::*;

fn main() {
    let tokens = lex("abc * 12..23 + 0x45.");
    println!("{:?}", tokens);
}

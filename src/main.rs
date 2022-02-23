mod lexer;

use lexer::*;

fn main() {
    let tokens = lex("!@$%^&* \t (#\nabcd+2.2..3");
    println!("{:?}", tokens);
}

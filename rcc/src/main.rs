use rcc_lexer::lex;
use rcc_parser::Parser;

fn main() {
    let tokens = lex("if (10) { break; } else { break; continue; }").unwrap();
    println!("{:#?}", Parser::new(tokens).parse());
}

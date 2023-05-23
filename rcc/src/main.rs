use rcc_lexer::lex;
use rcc_parser::Parser;

fn main() {
    let input = std::io::read_to_string(std::io::stdin()).unwrap();
    let tokens = lex(input).unwrap();
    println!("{:#?}", Parser::new(tokens).parse());
}

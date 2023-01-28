use boop::{lex::Lexer, parse::Parser};

fn main() {
  let input = "3 / (2 + 1.5)";
  let lexer = Lexer::new(input);
  let parser = Parser::new(lexer.lex());

  let tree = parser.parse().unwrap();
  syntree::print::print_with_source(std::io::stdout(), &tree, input).unwrap();
}

use boop::{
  eval::{Engine, Scope},
  lex::Lexer,
  parse::Parser,
};

fn main() {
  let input = "(1.5 + 3) / 2";
  let lexer = Lexer::new(input);
  // lexer.lex().for_each(|token| println!("{token:?}"));

  let mut parser = Parser::new(lexer);
  // println!("{}", parser.parse().unwrap());

  let mut scope = Scope::new();
  let eval = Engine::eval_with_scope(&mut scope, parser.parse().unwrap());
  println!("{}", eval.unwrap());

  // let module = Module::new(parser.parse().unwrap());
  // Engine::eval(&module);
}

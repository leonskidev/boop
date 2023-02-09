use boop::eval::Engine;

fn main() {
  println!("{}", Engine::eval("3 * 2 + 3 / 2").unwrap());
}

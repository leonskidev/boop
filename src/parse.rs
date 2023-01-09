//! Contains the parser.

use chumsky::prelude::*;

use crate::ast::*;

/// Creates a statement parser.
pub fn stmt_parser() -> impl Parser<char, Stmt, Error = Simple<char>> + Clone {
  Stmt::parser()
    .padded()
    .then_ignore(end().recover_with(skip_then_retry_until([])))
}

/// Creates a file parser.
pub fn file_parser(
) -> impl Parser<char, Vec<Stmt>, Error = Simple<char>> + Clone {
  Stmt::parser()
    .separated_by(text::newline())
    .padded()
    .then_ignore(end().recover_with(skip_then_retry_until([])))
}

impl Stmt {
  /// Creates a parser.
  pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> + Clone {
    let var_def = Ident::parser()
      .then_ignore(just('='))
      .then(Expr::parser())
      .map(|(ident, expr)| Self::VarDef { ident, expr });

    let fn_def = Ident::parser()
      .then(
        Ident::parser()
          .separated_by(text::whitespace())
          .delimited_by(just('('), just(')')),
      )
      .padded()
      .then_ignore(just('='))
      .then(Expr::parser())
      .map(|((ident, args), body)| Self::FnDef { ident, args, body });

    fn_def
      .or(var_def)
      .or(Expr::parser().map(Self::Expr))
      .padded()
      .recover_with(nested_delimiters(
        '(',
        ')',
        [('[', ']'), ('{', '}')],
        |_| Self::Error,
      ))
  }
}

impl Expr {
  /// Creates a parser.
  pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> + Clone {
    recursive(|expr| {
      let var = Ident::parser().map(Self::Var);

      let r#fn = Ident::parser()
        .then(
          expr
            .clone()
            .separated_by(text::whitespace())
            .delimited_by(just('('), just(')')),
        )
        .map(|(ident, args)| Self::Fn(ident, args));

      let atom = Lit::parser()
        .map(Self::Lit)
        .or(r#fn)
        .or(var)
        .or(expr.delimited_by(just('('), just(')')))
        .padded()
        .recover_with(nested_delimiters(
          '(',
          ')',
          [('[', ']'), ('{', '}')],
          |_| Self::Error,
        ));

      let unary = just('-')
        .to(UnOp::Neg)
        .repeated()
        .then(atom)
        .foldr(|op, rhs| Expr::Unary(op, Box::new(rhs)));

      let product = unary
        .clone()
        .then(
          just('*')
            .to(BinOp::Mul)
            .or(just('/').to(BinOp::Div))
            .padded()
            .then(unary)
            .repeated(),
        )
        .foldl(|lhs, (op, rhs)| Self::Binary(op, Box::new(lhs), Box::new(rhs)));

      let sum = product
        .clone()
        .then(
          just('+')
            .to(BinOp::Add)
            .or(just('-').to(BinOp::Sub))
            .padded()
            .then(product)
            .repeated(),
        )
        .foldl(|lhs, (op, rhs)| Self::Binary(op, Box::new(lhs), Box::new(rhs)));

      sum.padded()
    })
  }
}

impl Lit {
  /// Creates a parser.
  pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> + Clone {
    text::int(10)
      .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
      .collect::<String>()
      .from_str()
      .unwrapped()
      .map(Self)
      .padded()
  }
}

impl Ident {
  /// Creates a parser.
  pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> + Clone {
    text::ident().map(Self).padded()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use fixed::{types::extra::U64, FixedI128};
  use test_case::case;

  #[case("12" => Stmt::Expr(Expr::Lit(Lit(FixedI128::<U64>::from_num(12)))))]
  fn stmt(input: &str) -> Stmt {
    Stmt::parser().parse(input).unwrap()
  }

  #[case("12" => Expr::Lit(Lit(FixedI128::<U64>::from_num(12))))]
  #[case("-1" => Expr::Unary(UnOp::Neg, Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(1))))))]
  #[case("1 * 2" => Expr::Binary(BinOp::Mul, Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(1)))), Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(2))))))]
  #[case("1 + 2" => Expr::Binary(BinOp::Add, Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(1)))), Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(2))))))]
  #[case("1 + 2 * 3" => Expr::Binary(BinOp::Add, Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(1)))), Box::new(Expr::Binary(BinOp::Mul, Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(2)))), Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(3))))))))]
  #[case("1 + -2 * 3" => Expr::Binary(BinOp::Add, Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(1)))), Box::new(Expr::Binary(BinOp::Mul, Box::new(Expr::Unary(UnOp::Neg, Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(2)))))), Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(3))))))))]
  #[case("(1 + 2) * 3" => Expr::Binary(BinOp::Mul, Box::new(Expr::Binary(BinOp::Add, Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(1)))), Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(2)))))), Box::new(Expr::Lit(Lit(FixedI128::<U64>::from_num(3))))))]
  fn expr(input: &str) -> Expr {
    Expr::parser().parse(input).unwrap()
  }

  #[case("12" => Lit(FixedI128::<U64>::from_num(12)))]
  #[case("1.5" => Lit(FixedI128::<U64>::from_num(1.5)))]
  fn lit(input: &str) -> Lit {
    Lit::parser().parse(input).unwrap()
  }
}

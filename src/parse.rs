//! Contains the parser.

// TODO: add spans to return types

use chumsky::prelude::*;

use crate::{ast::*, lex::*};

impl Stmt {
  /// Creates a [`Stmt`] parser.
  pub fn parser() -> impl Parser<Token, Self, Error = Simple<Token>> + Clone {
    let ident = select! { Token::Ident(a) => a };

    let var_def = ident
      .then_ignore(just(Token::ColonEquals))
      .then(Expr::parser())
      .map(|(ident, expr)| Self::VarDef { ident, expr });

    let fn_def = ident
      .then(
        ident
          .separated_by(just(Token::Comma))
          .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
      )
      .then_ignore(just(Token::ColonEquals))
      .then(Expr::parser())
      .map(|((ident, args), body)| Self::FnDef { ident, args, body });

    fn_def
      .or(var_def)
      .or(Expr::parser().map(Self::Expr))
      .recover_with(nested_delimiters(
        Token::LeftBracket,
        Token::RightBracket,
        [],
        |_| Self::Error,
      ))
      .then_ignore(end().recover_with(skip_then_retry_until([])))
  }
}

impl Expr {
  /// Creates a [`Expr`] parser.
  pub fn parser() -> impl Parser<Token, Self, Error = Simple<Token>> + Clone {
    recursive(|expr| {
      let ident = select! { Token::Ident(a) => a };

      let num = select! { Token::Num(a) => Self::Num(a) };

      let var = ident.map(Self::Var);

      let r#fn = ident
        .then(
          expr
            .clone()
            .separated_by(just(Token::Comma))
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
        )
        .map(|(ident, args)| Self::Fn(ident, args));

      let atom = num
        .or(r#fn)
        .or(var)
        .or(
          expr
            .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
        )
        .recover_with(nested_delimiters(
          Token::LeftBracket,
          Token::RightBracket,
          [],
          |_| Self::Error,
        ));

      let unary = just(Token::Minus)
        .to(UnOp::Neg)
        .repeated()
        .then(atom)
        .foldr(|op, rhs| Expr::Unary(op, Box::new(rhs)));

      let product = unary
        .clone()
        .then(
          just(Token::Asterisk)
            .to(BinOp::Mul)
            .or(just(Token::Slash).to(BinOp::Div))
            .then(unary)
            .repeated(),
        )
        .foldl(|lhs, (op, rhs)| Self::Binary(op, Box::new(lhs), Box::new(rhs)));

      // sum
      product
        .clone()
        .then(
          just(Token::Plus)
            .to(BinOp::Add)
            .or(just(Token::Minus).to(BinOp::Sub))
            .then(product)
            .repeated(),
        )
        .foldl(|lhs, (op, rhs)| Self::Binary(op, Box::new(lhs), Box::new(rhs)))
    })
  }
}

// impl Stmt {
//   /// Creates a parser.
//   pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> + Clone {
//     let var_def = Ident::parser()
//       .then_ignore(just(":="))
//       .then(Expr::parser())
//       .map(|(ident, expr)| Self::VarDef { ident, expr });

//     let fn_def = Ident::parser()
//       .then(
//         Ident::parser()
//           .separated_by(just(','))
//           .delimited_by(just('('), just(')')),
//       )
//       .padded()
//       .then_ignore(just(":="))
//       .then(Expr::parser())
//       .map(|((ident, args), body)| Self::FnDef { ident, args, body });

//     fn_def
//       .or(var_def)
//       .or(Expr::parser().map(Self::Expr))
//       .padded()
//       .recover_with(nested_delimiters(
//         '(',
//         ')',
//         [('[', ']'), ('{', '}')],
//         |_| Self::Error,
//       ))
//   }
// }

// impl Expr {
//   /// Creates a parser.
//   pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> + Clone {
//     recursive(|expr| {
//       let var = Ident::parser().map(Self::Var);

//       let r#fn = Ident::parser()
//         .then(
//           expr
//             .clone()
//             .separated_by(just(','))
//             .delimited_by(just('('), just(')')),
//         )
//         .map(|(ident, args)| Self::Fn(ident, args));

//       let atom = Num::parser()
//         .map(Self::Num)
//         .or(r#fn)
//         .or(var)
//         .or(expr.delimited_by(just('('), just(')')))
//         .padded()
//         .recover_with(nested_delimiters(
//           '(',
//           ')',
//           [('[', ']'), ('{', '}')],
//           |_| Self::Error,
//         ));

//       let unary = just('-')
//         .to(UnOp::Neg)
//         .repeated()
//         .then(atom)
//         .foldr(|op, rhs| Expr::Unary(op, Box::new(rhs)));

//       let product = unary
//         .clone()
//         .then(
//           just('*')
//             .to(BinOp::Mul)
//             .or(just('/').to(BinOp::Div))
//             .padded()
//             .then(unary)
//             .repeated(),
//         )
//         .foldl(|lhs, (op, rhs)| Self::Binary(op, Box::new(lhs), Box::new(rhs)));

//       let sum = product
//         .clone()
//         .then(
//           just('+')
//             .to(BinOp::Add)
//             .or(just('-').to(BinOp::Sub))
//             .padded()
//             .then(product)
//             .repeated(),
//         )
//         .foldl(|lhs, (op, rhs)| Self::Binary(op, Box::new(lhs), Box::new(rhs)));

//       sum.padded()
//     })
//   }
// }

// impl Num {
//   /// Creates a parser.
//   pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> + Clone {
//     text::int(10)
//       .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
//       .collect::<String>()
//       .from_str()
//       .unwrapped()
//       .map(Self)
//       .padded()
//   }
// }

// impl Ident {
//   /// Creates a parser.
//   pub fn parser() -> impl Parser<char, Self, Error = Simple<char>> + Clone {
//     text::ident().map(Self).padded()
//   }
// }

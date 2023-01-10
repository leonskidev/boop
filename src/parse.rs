//! Contains the parser.

// TODO: add spans to return types

use chumsky::prelude::*;

use crate::{ast::*, lex::*};

impl Stmt {
  /// Creates a [`Stmt`] parser.
  pub fn parser() -> impl Parser<Token, Self, Error = Simple<Token>> + Clone {
    let ident = select! { Token::Ident(a) => a };

    let var_def = just(Token::Let)
      .ignore_then(ident)
      .then_ignore(just(Token::Equals))
      .then(Expr::parser())
      .map(|(ident, expr)| Self::VarDef { ident, expr });

    let fn_def = just(Token::Let)
      .ignore_then(ident)
      .then(
        ident
          .separated_by(just(Token::Comma))
          .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
      )
      .then_ignore(just(Token::Equals))
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

      let sum = product
        .clone()
        .then(
          just(Token::Plus)
            .to(BinOp::Add)
            .or(just(Token::Minus).to(BinOp::Sub))
            .then(product)
            .repeated(),
        )
        .foldl(|lhs, (op, rhs)| Self::Binary(op, Box::new(lhs), Box::new(rhs)));

      // cmp
      sum
        .clone()
        .then(just(Token::Equals).to(BinOp::Eql).then(sum).repeated())
        .foldl(|lhs, (op, rhs)| Self::Binary(op, Box::new(lhs), Box::new(rhs)))
    })
  }
}

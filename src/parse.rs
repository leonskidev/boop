//! Contains the parser.

use chumsky::zero_copy::{error::Error, prelude::*};
use core::fmt;

use crate::lex::*;

/// Creates a parser.
pub fn parser<'a, E>() -> impl Parser<'a, [Token], Stmt, E>
where
  E: 'a + Error<[Token]>,
{
  let ident =
    any()
      .filter(|token| matches!(token, Token::Ident(_)))
      .map(|token| match token {
        Token::Ident(a) => a,
        _ => unreachable!(),
      });

  let expr = recursive(|expr| {
    let real =
      any()
        .filter(|token| matches!(token, Token::Real(_)))
        .map(|token| match token {
          Token::Real(a) => Lit::Real(a),
          _ => unreachable!(),
        });

    let r#fn = ident
      .separated_by(just(Token::Comma))
      .collect()
      .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
      .then_ignore(just(Token::RightArrow))
      .then(expr.clone())
      .map(|(args, body)| Lit::Fn(args, Box::new(body)));

    let lit = real.or(r#fn).map(Expr::Lit);

    let atom = lit.or(ident.map(Expr::Var)).or(
      expr
        .clone()
        .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
    );

    let call = atom
      .then(
        expr
          .separated_by(just(Token::Comma))
          .collect()
          .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
          .repeated()
          .collect::<Vec<_>>(),
      )
      .foldl(|lhs, args| Expr::Call(Box::new(lhs), args));

    let unary = just(Token::Minus)
      .to(UnOp::Neg)
      .repeated()
      .collect::<Vec<_>>()
      .then(call)
      .foldr(|op, rhs| Expr::Unary(op, Box::new(rhs)));

    let order = unary
      .clone()
      .then(
        just(Token::Circumflex)
          .to(BinOp::Pow)
          .then(unary)
          .repeated()
          .collect::<Vec<_>>(),
      )
      .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

    let product = order
      .clone()
      .then(
        just(Token::Asterisk)
          .to(BinOp::Mul)
          .or(just(Token::Slash).to(BinOp::Div))
          .then(order)
          .repeated()
          .collect::<Vec<_>>(),
      )
      .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

    let sum = product
      .clone()
      .then(
        just(Token::Plus)
          .to(BinOp::Add)
          .or(just(Token::Minus).to(BinOp::Sub))
          .then(product)
          .repeated()
          .collect::<Vec<_>>(),
      )
      .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

    let compare = sum
      .clone()
      .then(
        just(Token::Equals)
          .to(BinOp::Eq)
          .or(just(Token::ExclamationEquals).to(BinOp::Ne))
          .or(just(Token::LeftAngleBracket).to(BinOp::Lt))
          .or(just(Token::RightAngleBracket).to(BinOp::Gt))
          .or(just(Token::LeftAngleBracketEquals).to(BinOp::Le))
          .or(just(Token::RightAngleBracketEquals).to(BinOp::Ge))
          .then(sum)
          .repeated()
          .collect::<Vec<_>>(),
      )
      .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

    #[allow(clippy::let_and_return)]
    compare
  });

  let def = {
    let var = just(Token::Let)
      .ignore_then(ident)
      .then_ignore(just(Token::Equals))
      .then(expr.clone())
      .map(|(ident, body)| Stmt::Def(ident, body));

    let r#fn = just(Token::Let)
      .ignore_then(ident)
      .then(
        ident
          .separated_by(just(Token::Comma))
          .collect()
          .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
      )
      .then_ignore(just(Token::Equals))
      .then(expr.clone())
      .map(|((ident, args), body)| {
        Stmt::Def(ident, Expr::Lit(Lit::Fn(args, Box::new(body))))
      });

    r#fn.or(var)
  };

  let stmt = def.or(expr.map(Stmt::Expr));

  stmt.then_ignore(end())
}

/// Represents a statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
  /// An expression.
  Expr(Expr),

  /// A definition.
  Def(Ident, Expr),
}

/// Represents an expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
  /// A literal.
  Lit(Lit),

  /// A variable reference.
  Var(Ident),
  /// A function call.
  Call(Box<Self>, Vec<Self>),

  /// A unary operation.
  Unary(UnOp, Box<Self>),
  /// A binary operation.
  Binary(BinOp, Box<Self>, Box<Self>),
}

/// Represents a literal.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Lit {
  /// A real number.
  Real(Real),
  /// An arrow function.
  Fn(Vec<Ident>, Box<Expr>),
}

/// Represents a unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
  /// A negation.
  Neg,
}

impl fmt::Display for UnOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      Self::Neg => "-",
    };
    write!(f, "{}", s)
  }
}

/// Represents a binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
  /// The addition operator.
  Add,
  /// The subtraction operator.
  Sub,
  /// The multiplication operator.
  Mul,
  /// The division operator.
  Div,
  /// The power operator.
  Pow,

  /// The equality operator.
  Eq,
  /// The inequality operator.
  Ne,
  /// The less than operator.
  Lt,
  /// The greater than operator.
  Gt,
  /// The less than or equals operator.
  Le,
  /// The greater than or equals operator.
  Ge,
}

impl fmt::Display for BinOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      Self::Add => "+",
      Self::Sub => "-",
      Self::Mul => "*",
      Self::Div => "/",
      Self::Pow => "^",

      Self::Eq => "=",
      Self::Ne => "!=",
      Self::Lt => ">",
      Self::Gt => ">",
      Self::Le => "<=",
      Self::Ge => ">=",
    };
    write!(f, "{}", s)
  }
}

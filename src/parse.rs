//! Contains the parser.

use core::hint;

use chumsky::zero_copy::{error::Error, prelude::*};
use core::fmt;

use crate::lex::*;

/// Creates a parser.
pub fn parser<'a, E>() -> impl Parser<'a, [Token], Expr, E>
where
  E: 'a + Error<[Token]>,
{
  let recover_delimiters = |start, end| {
    just(start)
      .ignore_then(take_until(just(end)).ignored())
      .try_map(move |_, span| {
        Err(E::expected_found([Some(start)], Some(end), span))
      })
  };

  let real =
    any()
      .filter(|token| matches!(token, Token::Real(_)))
      .map(|token| match token {
        Token::Real(a) => a,
        _ => unsafe { hint::unreachable_unchecked() },
      });

  let symbol =
    any()
      .filter(|token| matches!(token, Token::Symbol(_)))
      .map(|token| match token {
        Token::Symbol(a) => a,
        _ => unsafe { hint::unreachable_unchecked() },
      });

  let expr = recursive(|expr| {
    let var = symbol.map(Expr::Var);

    let r#fn = symbol
      .map(|symbol| vec![symbol])
      .or(
        symbol
          .separated_by(just(Token::Comma))
          .collect()
          .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
      )
      .then_ignore(just(Token::RightArrow))
      .then(expr.clone())
      .map(|(symbols, body)| Expr::Fn(symbols, Box::new(body)))
      .recover_with(recover_delimiters(
        Token::LeftBracket,
        Token::RightBracket,
      ));

    let atom = real
      .map(Expr::Real)
      .or(r#fn)
      .or(var)
      .or(
        expr
          .clone()
          .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
      )
      .recover_with(recover_delimiters(
        Token::LeftBracket,
        Token::RightBracket,
      ));

    let call = atom
      .clone()
      .then(
        expr
          .separated_by(just(Token::Comma))
          .collect()
          .delimited_by(just(Token::LeftBracket), just(Token::RightBracket))
          .repeated()
          .collect::<Vec<_>>(),
      )
      .foldl(|r#fn, exprs| Expr::Call(Box::new(r#fn), exprs));

    let unary = just(Token::Minus)
      .to(UnOp::Neg)
      .repeated()
      .collect::<Vec<_>>()
      .then(call)
      .foldr(|op, rhs| Expr::Unary(op, Box::new(rhs)));

    let power = unary
      .clone()
      .then(
        just(Token::Circumflex)
          .to(BinOp::Pow)
          .then(unary)
          .repeated()
          .collect::<Vec<_>>(),
      )
      .foldl(|lhs, (op, rhs)| Expr::Binary(op, Box::new(lhs), Box::new(rhs)));

    let product_prefix = power
      .clone()
      .then(atom.repeated().collect::<Vec<_>>())
      .foldl(|lhs, rhs| Expr::Binary(BinOp::Mul, Box::new(lhs), Box::new(rhs)));

    let product = product_prefix
      .clone()
      .then(
        just(Token::Asterisk)
          .to(BinOp::Mul)
          .or(just(Token::Slash).to(BinOp::Div))
          .then(product_prefix)
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

    let comapre = sum
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
    comapre
  });

  let let_fn = just(Token::Let)
    .ignore_then(symbol)
    .then(
      symbol
        .separated_by(just(Token::Comma))
        .collect()
        .delimited_by(just(Token::LeftBracket), just(Token::RightBracket)),
    )
    .then_ignore(just(Token::Equals))
    .then(expr.clone())
    .map(|((symbol, symbols), body)| {
      Expr::Let(symbol, Box::new(Expr::Fn(symbols, Box::new(body))))
    })
    .recover_with(recover_delimiters(Token::LeftBracket, Token::RightBracket));

  let r#let = just(Token::Let)
    .ignore_then(symbol)
    .then_ignore(just(Token::Equals))
    .then(expr.clone())
    .map(|(symbol, body)| Expr::Let(symbol, Box::new(body)));

  let_fn.or(r#let).or(expr).then_ignore(end())
}

/// Represents an expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
  /// A real number: `123`, `1.23`.
  Real(Real),
  /// A variable: `x`, `hello`.
  Var(Symbol),
  /// A function: `x -> x^2`, `(x, y) -> x+y`.
  Fn(Vec<Symbol>, Box<Self>),

  /// A function call: `a(10)`.
  Call(Box<Self>, Vec<Self>),

  /// A unary operation: `-1`.
  Unary(UnOp, Box<Self>),
  /// A binary operation: `1 + 2`, `1 * 2`.
  Binary(BinOp, Box<Self>, Box<Self>),

  /// A let definition: `let a = 123`.
  Let(Symbol, Box<Self>),
}

/// Represents a unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
  /// Negation.
  Neg,
}

impl UnOp {
  /// Returns the <code>&[str](primitive@str)</code> of this operator.
  pub const fn as_str<'a>(self) -> &'a str {
    match self {
      Self::Neg => "-",
    }
  }
}

impl fmt::Display for UnOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

/// Represents a binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
  /// Addition.
  Add,
  /// Subtraction.
  Sub,
  /// Multiplication.
  Mul,
  /// Division.
  Div,
  /// Power.
  Pow,

  /// Equals.
  Eq,
  /// Not equals.
  Ne,
  /// Less than.
  Lt,
  /// Greater than.
  Gt,
  /// Less than or equals.
  Le,
  /// Greater than or equals.
  Ge,
}

impl BinOp {
  /// Returns the <code>&[str](primitive@str)</code> of this operator.
  pub const fn as_str<'a>(self) -> &'a str {
    match self {
      Self::Add => "+",
      Self::Sub => "-",
      Self::Mul => "*",
      Self::Div => "/",
      Self::Pow => "^",

      Self::Eq => "=",
      Self::Ne => "!=",
      Self::Lt => "<",
      Self::Gt => ">",
      Self::Le => "<=",
      Self::Ge => ">=",
    }
  }
}

impl fmt::Display for BinOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.as_str())
  }
}

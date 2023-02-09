//! Contains the parser.

use core::{fmt, iter::Peekable};

use num_bigint::BigInt;
use num_rational::BigRational;

use crate::lex::{Lexer, Token, TokenKind};

/// A parser which converts a sequence of [`Token`]s into [`Stmt`]s.
#[derive(Debug, Clone)]
pub struct Parser<'a> {
  tokens: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
  /// Creates a [`Parser`].
  #[inline]
  pub fn new(tokens: Lexer<'a>) -> Self {
    Self {
      tokens: tokens.peekable(),
    }
  }

  fn expr_bp(&mut self, min_bp: u8) -> Option<Expr> {
    self.skip_whitespace();

    let mut lhs = match self.tokens.next()? {
      Token {
        kind: TokenKind::Int(i),
        ..
      } => Expr::Int(i),
      Token {
        kind: TokenKind::Rat(i),
        ..
      } => Expr::Rat(i),
      Token {
        kind: TokenKind::Minus,
        ..
      } => {
        let op = UnOp::Neg;
        let ((), r_bp) = op.binding_power();
        match self.expr_bp(r_bp) {
          Some(rhs) => Expr::Unary(op, Box::new(rhs)),
          _ => return Some(Expr::Error),
        }
      }
      Token {
        kind: TokenKind::LeftBracket,
        ..
      } => {
        self.skip_whitespace();
        match self.expr_bp(0) {
          Some(lhs) => {
            self.skip_whitespace();
            if self
              .tokens
              .next_if(|token| token.kind == TokenKind::RightBracket)
              .is_some()
            {
              lhs
            } else {
              Expr::Error
            }
          }
          _ => return Some(Expr::Error),
        }
      }
      _ => Expr::Error,
    };

    loop {
      self.skip_whitespace();

      let op = match self.tokens.peek() {
        None => break,

        Some(Token {
          kind: TokenKind::Plus,
          ..
        }) => BinOp::Add,
        Some(Token {
          kind: TokenKind::Minus,
          ..
        }) => BinOp::Sub,
        Some(Token {
          kind: TokenKind::Asterisk,
          ..
        }) => BinOp::Mul,
        Some(Token {
          kind: TokenKind::Slash,
          ..
        }) => BinOp::Div,
        Some(Token {
          kind: TokenKind::RightBracket,
          ..
        }) => break,

        _ => return Some(Expr::Error),
      };

      let (l_bp, r_bp) = op.binding_power();
      if l_bp < min_bp {
        break;
      }

      self.tokens.next();
      self.skip_whitespace();

      match self.expr_bp(r_bp) {
        Some(rhs) => lhs = Expr::Binary(op, Box::new((lhs, rhs))),
        _ => return Some(Expr::Error),
      }
    }

    Some(lhs)
  }

  fn skip_whitespace(&mut self) {
    while self
      .tokens
      .next_if(|Token { kind, .. }| *kind == TokenKind::Whitespace)
      .is_some()
    {}
  }
}

impl<'a> Iterator for Parser<'a> {
  type Item = Stmt;

  fn next(&mut self) -> Option<Self::Item> {
    Some(Stmt::Expr(self.expr_bp(0)?))
  }
}

/// A statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
  /// An expression.
  Expr(Expr),
}

impl fmt::Display for Stmt {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Expr(expr) => write!(f, "{expr}"),
    }
  }
}

/// An expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
  /// An integer.
  Int(BigInt),
  /// A rational.
  Rat(BigRational),

  /// A unary operation.
  Unary(UnOp, Box<Expr>),
  /// A binary operation.
  Binary(BinOp, Box<(Expr, Expr)>),

  /// An invalid expression.
  Error,
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Int(i) => write!(f, "{i}"),
      Self::Rat(r) => write!(f, "{r}"),

      Self::Unary(op, rhs) => write!(f, "{op}{rhs}"),
      Self::Binary(op, exprs) => write!(f, "{} {op} {}", exprs.0, exprs.1),

      Self::Error => write!(f, "<error>"),
    }
  }
}

/// A unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
  /// Negation.
  Neg,
}

impl UnOp {
  /// Returns the binding power.
  pub const fn binding_power(self) -> ((), u8) {
    match self {
      Self::Neg => ((), 5),
    }
  }
}

impl fmt::Display for UnOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Neg => write!(f, "-"),
    }
  }
}

/// A binary operator.
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
}

impl BinOp {
  /// Returns the binding power.
  pub const fn binding_power(self) -> (u8, u8) {
    match self {
      Self::Add | Self::Sub => (1, 2),
      Self::Mul | Self::Div => (3, 4),
    }
  }
}

impl fmt::Display for BinOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Add => write!(f, "+"),
      Self::Sub => write!(f, "-"),
      Self::Mul => write!(f, "*"),
      Self::Div => write!(f, "/"),
    }
  }
}

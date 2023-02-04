//! Contains the parser.

use core::{fmt, iter::Peekable};

use fixed::{types::extra::U64, FixedI128, ParseFixedError};
use thiserror::Error;

use crate::lex::{Lex, Lexer, Span, Token, TokenKind};

/// A parser.
#[derive(Debug, Clone)]
pub struct Parser<'a> {
  lexer: Lexer<'a>,
  tokens: Peekable<Lex<'a>>,
}

impl<'a> Parser<'a> {
  /// Creates a [`Parser`].
  #[inline]
  pub fn new(lexer: Lexer<'a>) -> Self {
    Self {
      lexer,
      tokens: lexer.lex().peekable(),
    }
  }

  /// Parse an [`Expr`].
  #[inline]
  pub fn parse(&mut self) -> Result<Expr, Error> {
    self.expr_bp(0)
  }

  fn expr_bp(&mut self, min_bp: u8) -> Result<Expr, Error> {
    let mut lhs = match self.tokens.next() {
      Some(Token {
        kind: TokenKind::Number,
        span,
      }) => self
        .lexer
        .slice(span)
        .ok_or(Error::InvalidSlice)
        .and_then(|s| s.parse().map_err(Error::InvalidNumber))
        .map(|number| Expr::new(ExprKind::Number(number), span))?,
      Some(Token {
        kind: TokenKind::Symbol,
        span,
      }) => self
        .lexer
        .slice(span)
        .ok_or(Error::InvalidSlice)
        .map(|s| Expr::new(ExprKind::Symbol(s.to_string()), span))?,
      Some(Token {
        kind: TokenKind::LeftBracket,
        span: l_span,
      }) => {
        let lhs = self.expr_bp(0)?;
        match self.tokens.next() {
          Some(Token {
            kind: TokenKind::RightBracket,
            span: r_span,
          }) => Ok(Expr::new(
            ExprKind::Group(Box::new(lhs)),
            (l_span.0, r_span.1),
          )),
          _ => Err(Error::UnmatchedBracket),
        }?
      }
      Some(Token {
        kind: TokenKind::Minus,
        span,
      }) => {
        let op = UnOp::Neg;
        let ((), r_bp) = op.binding_power();

        let rhs = self.expr_bp(r_bp)?;
        let span = (span.0, rhs.span.1);
        Expr::new(ExprKind::Unary(op, Box::new(rhs)), span)
      }
      Some(token) => Err(Error::UnexpectedToken(token))?,
      None => Err(Error::UnexpectedEoi)?,
    };

    loop {
      let op = match self.tokens.peek().copied() {
        None
        | Some(Token {
          kind: TokenKind::RightBracket,
          ..
        }) => break,
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
        Some(token) => Err(Error::UnexpectedToken(token))?,
      };
      let (l_bp, r_bp) = op.binding_power();
      if l_bp < min_bp {
        break;
      }

      self.tokens.next();

      let rhs = self.expr_bp(r_bp)?;
      let span = (lhs.span.0, rhs.span.1);
      lhs = Expr::new(ExprKind::Binary(op, Box::new((lhs, rhs))), span);
    }

    Ok(lhs)
  }
}

/// An expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
  /// The kind.
  pub kind: ExprKind,
  /// The span.
  pub span: Span,
}

impl Expr {
  /// Creates a [`Expr`].
  #[inline]
  pub const fn new(kind: ExprKind, span: Span) -> Self {
    Self { kind, span }
  }
}

impl fmt::Display for Expr {
  #[inline]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.kind)
  }
}

/// An expression kind.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
  /// A number.
  Number(FixedI128<U64>),
  /// A symbol usage.
  // TODO: make this not allocate
  Symbol(String),

  /// A precedence group.
  Group(Box<Expr>),
  /// A unary operation.
  // TODO: make this not allocate
  Unary(UnOp, Box<Expr>),
  /// A binary operation.
  // TODO: make this not allocate
  Binary(BinOp, Box<(Expr, Expr)>),
}

impl fmt::Display for ExprKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Number(n) => write!(f, "{n}"),
      Self::Symbol(s) => write!(f, "{s}"),
      Self::Group(expr) => write!(f, "({expr})"),
      Self::Unary(op, rhs) => write!(f, "{op}{rhs}"),
      Self::Binary(op, exprs) => write!(f, "{} {} {}", exprs.0, op, exprs.1),
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
  #[inline]
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
  #[inline]
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

/// A [`Parser`] error.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum Error {
  /// An invalid slice index.
  #[error("invalid slice index")]
  InvalidSlice,
  /// An invalid number.
  #[error("{0}")]
  InvalidNumber(ParseFixedError),
  /// An unmatched bracket.
  #[error("unmatched bracket")]
  UnmatchedBracket,
  /// An unexpected token.
  #[error("unexpected token: {0:?}")]
  UnexpectedToken(Token),
  /// An unexpected end of input.
  #[error("unexpected end of input")]
  UnexpectedEoi,
}

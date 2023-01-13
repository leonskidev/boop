//! Contains the formatter.

use core::fmt;
use lasso::Interner;

use crate::{eval::Engine, parse::Expr};

/// A formatting context.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Formatter<'a, I: Interner> {
  engine: &'a Engine<I>,
  expr: &'a Expr,
}

impl<'a, I: Interner> Formatter<'a, I> {
  /// Creates a [`Formatter`].
  #[inline]
  pub const fn new(engine: &'a Engine<I>, expr: &'a Expr) -> Self {
    Self { engine, expr }
  }
}

impl<'a, I: Interner> fmt::Display for Formatter<'a, I> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.expr {
      Expr::Real(real) => write!(f, "{}", real),
      Expr::Var(symbol) => {
        write!(f, "{}", self.engine.interner().resolve(symbol))
      }
      Expr::Fn(symbols, body) => {
        write!(f, "(")?;
        symbols
          .iter()
          .map(|symbol| self.engine.interner().resolve(symbol))
          .enumerate()
          .try_for_each(|(i, symbol)| {
            write!(f, "{}{}", if i > 0 { ", " } else { "" }, symbol)
          })?;
        write!(f, ") -> {}", Self::new(self.engine, body))
      }

      Expr::Call(r#fn, exprs) => {
        write!(f, "{}(", Self::new(self.engine, r#fn))?;
        exprs
          .iter()
          .map(|expr| Self::new(self.engine, expr))
          .enumerate()
          .try_for_each(|(i, expr)| {
            write!(f, "{}{}", if i > 0 { ", " } else { "" }, expr)
          })?;
        write!(f, ")")
      }

      Expr::Unary(op, rhs) => {
        write!(f, "{}{}", op, Self::new(self.engine, rhs))
      }
      Expr::Binary(op, lhs, rhs) => write!(
        f,
        "{} {} {}",
        Self::new(self.engine, lhs),
        op,
        Self::new(self.engine, rhs)
      ),

      Expr::Let(symbol, body) => write!(
        f,
        "let {} = {}",
        self.engine.interner().resolve(symbol),
        Self::new(self.engine, body)
      ),
    }
  }
}

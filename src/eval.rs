//! Contains the evaluator.

use std::collections::HashMap;

use thiserror::Error;

use crate::parse::{BinOp, Expr, ExprKind, UnOp};

/// An evaluation engine.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Engine;

impl Engine {
  /// Evaluate an expression.
  pub fn eval_with_scope(
    scope: &mut Scope,
    expr: Expr,
  ) -> Result<ExprKind, Error> {
    // TODO: remove this clone
    let expr_kind_clone = expr.kind.clone();

    match expr.kind {
      ExprKind::Number(_) => Ok(expr.kind),
      ExprKind::Symbol(symbol) => scope
        .symbols()
        .get(symbol.as_str())
        .cloned()
        .ok_or(Error::UndeclaredSymbolUsage(symbol)),

      ExprKind::Group(expr) => Self::eval_with_scope(scope, *expr),
      ExprKind::Unary(op, rhs) => match Self::eval_with_scope(scope, *rhs)? {
        ExprKind::Number(rhs) => match op {
          UnOp::Neg => Ok(ExprKind::Number(-rhs)),
        },
        _ => Err(Error::InvalidUnary(expr_kind_clone)),
      },
      ExprKind::Binary(op, exprs) => match (
        Self::eval_with_scope(scope, exprs.0)?,
        Self::eval_with_scope(scope, exprs.1)?,
      ) {
        (ExprKind::Number(lhs), ExprKind::Number(rhs)) => match op {
          BinOp::Add => Ok(ExprKind::Number(lhs + rhs)),
          BinOp::Sub => Ok(ExprKind::Number(lhs - rhs)),
          BinOp::Mul => Ok(ExprKind::Number(lhs * rhs)),
          BinOp::Div => Ok(ExprKind::Number(lhs / rhs)),
        },
        _ => Err(Error::InvalidBinary(expr_kind_clone)),
      },
    }
  }
}

/// An evaluation context.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Scope<'a> {
  symbols: HashMap<&'a str, ExprKind>,
}

impl<'a> Scope<'a> {
  /// Creates a [`Scope`].
  #[inline]
  pub fn new() -> Self {
    Self {
      symbols: HashMap::default(),
    }
  }

  /// Creates a [`Scope`] with `symbols`.
  #[inline]
  pub const fn with_symbols(symbols: HashMap<&'a str, ExprKind>) -> Self {
    Self { symbols }
  }

  /// Returns a reference to the symbols.
  #[inline]
  pub const fn symbols(&self) -> &HashMap<&'a str, ExprKind> {
    &self.symbols
  }

  /// Returns a mutable reference to the symbols.
  #[inline]
  pub fn symbols_mut(&mut self) -> &mut HashMap<&'a str, ExprKind> {
    &mut self.symbols
  }
}

/// An [`Engine`] error.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Error)]
pub enum Error {
  /// An undeclared symbol usage.
  #[error("undeclared symbol usage: {0}")]
  UndeclaredSymbolUsage(String),
  /// An unary expression.
  #[error("invalid unary expression: {0}")]
  InvalidUnary(ExprKind),
  /// An binary expression.
  #[error("invalid binary expression: {0}")]
  InvalidBinary(ExprKind),
}

// TODO: implement this for easily reusable modules, such as std;
//       add a feature to implement serde for it too

// /// A module context.
// #[derive(Debug, Clone, PartialEq, Eq, Default)]
// pub struct Module<'a> {
//   expr: Vec<Expr>,
//   scope: Scope<'a>,
// }

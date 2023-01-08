//! Contains the AST.

use core::fmt;
use fixed::{types::extra::U64, FixedI128};

/// A statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
  /// An [`Expr`].
  Expr(Expr),
  /// A variable assignment: `a = 123`.
  VarDef {
    /// The [`Ident`].
    ident: Ident,
    /// The [`Expr`].
    expr: Expr,
  },
}

impl fmt::Display for Stmt {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Expr(expr) => write!(f, "{}", expr),
      Self::VarDef { ident, expr } => write!(f, "{} = {}", ident, expr),
    }
  }
}

/// An expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
  /// An invalid expression.
  Error,

  /// A [`Lit`].
  Lit(Lit),
  /// A variable usage: `a`.
  Var(Ident),

  /// A unary operation: `-123`, `-a`.
  Unary(UnOp, Box<Expr>),
  /// A binary operation: `a * 2`.
  Binary(BinOp, Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Error => write!(f, "<error>"),

      Self::Lit(lit) => write!(f, "{}", lit),
      Self::Var(ident) => write!(f, "{}", ident),

      Self::Unary(op, rhs) => write!(f, "{}{}", op, rhs),
      Self::Binary(op, lhs, rhs) => match op {
        BinOp::Add | BinOp::Sub => write!(f, "{} {} {}", lhs, op, rhs),
        BinOp::Mul | BinOp::Div => write!(f, "{}{}{}", lhs, op, rhs),
      },
    }
  }
}

/// A fixed-point literal: `1` or `1.23`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Lit(pub FixedI128<U64>);

impl fmt::Display for Lit {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

/// An identifier: `hello`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Ident(pub String);

impl fmt::Display for Ident {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

/// A unary operator: `-`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
  /// The negation (`-`) operator.
  Neg,
}

impl fmt::Display for UnOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Neg => write!(f, "-"),
    }
  }
}

/// A binary operator: `+`, `*`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
  /// The addition (`+`) operator.
  Add,
  /// The addition (`-`) operator.
  Sub,
  /// The addition (`*`) operator.
  Mul,
  /// The addition (`/`) operator.
  Div,
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

//! Contains the AST.

use core::fmt;
use fixed::{types::extra::U64, FixedI128};

/// A statement.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
  /// An invalid statement.
  Error,

  /// An [`Expr`].
  Expr(Expr),

  /// A variable definition: `a := 123`.
  VarDef {
    /// The variable [`Ident`].
    ident: Ident,
    /// The variable [`Expr`].
    expr: Expr,
  },
  /// A function definition: `f(a) := a * 2`.
  FnDef {
    /// The function [`Ident`].
    ident: Ident,
    /// The function argument [`Ident`]s.
    args: Vec<Ident>,
    /// The function body [`Expr`].
    body: Expr,
  },
}

impl fmt::Display for Stmt {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Error => write!(f, "<error>"),

      Self::Expr(expr) => write!(f, "{}", expr),

      Self::VarDef { ident, expr } => write!(f, "{} := {}", ident, expr),
      Self::FnDef { ident, args, body } => {
        write!(f, "{}(", ident)?;
        args.iter().enumerate().try_for_each(|(i, ident)| {
          write!(f, "{}{}", if i > 0 { ", " } else { "" }, ident)
        })?;
        write!(f, ") := {}", body)
      }
    }
  }
}

/// An expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
  /// An invalid expression.
  Error,

  /// A [`Num`].
  Num(Num),
  /// A variable usage: `a`.
  Var(Ident),
  /// A function usage: `f(a b)`.
  Fn(Ident, Vec<Self>),

  /// A unary operation: `-123`, `-a`.
  Unary(UnOp, Box<Self>),
  /// A binary operation: `a * 2`.
  Binary(BinOp, Box<Self>, Box<Self>),
}

impl fmt::Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Error => write!(f, "<error>"),

      Self::Num(num) => write!(f, "{}", num),
      Self::Var(ident) => write!(f, "{}", ident),
      Self::Fn(ident, args) => {
        write!(f, "{}(", ident)?;
        args.iter().enumerate().try_for_each(|(i, expr)| {
          write!(f, "{}{}", if i > 0 { ", " } else { "" }, expr)
        })?;
        write!(f, ")")
      }

      Self::Unary(op, rhs) => write!(f, "{}{}", op, rhs),
      Self::Binary(op, lhs, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
    }
  }
}

/// A fixed-point literal: `1` or `1.23`.
pub type Num = FixedI128<U64>;

/// An identifier: `f`, `hello`.
pub type Ident = String;

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
  /// The equality (`=`) operator.
  Eql,
}

impl fmt::Display for BinOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Add => write!(f, "+"),
      Self::Sub => write!(f, "-"),
      Self::Mul => write!(f, "*"),
      Self::Div => write!(f, "/"),
      Self::Eql => write!(f, "="),
    }
  }
}

//! Contains the evaluator.

use std::collections::HashMap;

use crate::ast::*;

/// An evaluation context.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
  assignments: HashMap<Stmt, Expr>,
}

impl Stmt {
  ///
  pub fn eval(self, ctx: &mut Context) -> Self {
    match self {
      Self::Expr(expr) => Self::Expr(expr.eval(ctx)),
    }
  }
}

impl Expr {
  ///
  pub fn eval(self, ctx: &mut Context) -> Self {
    match self {
      Self::Error => todo!(),

      Self::Lit(lit) => Self::Lit(lit.eval(ctx)),

      Self::Unary(op, rhs) => match rhs.eval(ctx) {
        Expr::Lit(Lit(rhs)) => match op {
          UnOp::Neg => Expr::Lit(Lit(-rhs)),
        },
        _ => unimplemented!(),
      },
      Self::Binary(op, lhs, rhs) => match (lhs.eval(ctx), rhs.eval(ctx)) {
        (Expr::Lit(Lit(lhs)), Expr::Lit(Lit(rhs))) => match op {
          BinOp::Add => Expr::Lit(Lit(lhs + rhs)),
          BinOp::Sub => Expr::Lit(Lit(lhs - rhs)),
          BinOp::Mul => Expr::Lit(Lit(lhs * rhs)),
          BinOp::Div => Expr::Lit(Lit(lhs / rhs)),
        },
        _ => unimplemented!(),
      },
    }
  }
}

impl Lit {
  ///
  #[inline]
  pub fn eval(self, _ctx: &mut Context) -> Self {
    self
  }
}

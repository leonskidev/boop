//! Contains the evaluator.

use num_rational::BigRational;

use crate::{
  lex::Lexer,
  parse::{BinOp, Expr, Parser, Stmt, UnOp},
};

/// An evaluation engine.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Engine;

impl Engine {
  /// Evaluates a <code>&[str]</code>.
  pub fn eval(input: &str) -> Option<Stmt> {
    Some(Self::eval_stmt(Parser::new(Lexer::new(input)).next()?))
  }

  /// Evaluates a statement.
  pub fn eval_stmt(stmt: Stmt) -> Stmt {
    match stmt {
      Stmt::Expr(expr) => Stmt::Expr(Self::eval_expr(expr)),
    }
  }

  /// Evaluates an expression.
  pub fn eval_expr(expr: Expr) -> Expr {
    match expr {
      Expr::Int(_) => expr,
      Expr::Rat(r) if r.is_integer() => Expr::Int(r.to_integer()),
      Expr::Rat(_) => expr,

      Expr::Unary(op, rhs) => {
        let expr = match Self::eval_expr(*rhs) {
          Expr::Int(i) => match op {
            UnOp::Neg => Expr::Int(-i),
          },
          Expr::Rat(r) => match op {
            UnOp::Neg => Expr::Rat(-r),
          },

          // TODO: handle edge cases
          _ => todo!(),
        };
        Self::eval_expr(expr)
      }
      Expr::Binary(op, exprs) => {
        let expr = match (Self::eval_expr(exprs.0), Self::eval_expr(exprs.1)) {
          (Expr::Int(a), Expr::Int(b)) => match op {
            BinOp::Add => Expr::Int(a + b),
            BinOp::Sub => Expr::Int(a - b),
            BinOp::Mul => Expr::Int(a * b),
            BinOp::Div => Expr::Rat(BigRational::from(a) / b),
          },
          (Expr::Rat(a), Expr::Rat(b)) => match op {
            BinOp::Add => Expr::Rat(a + b),
            BinOp::Sub => Expr::Rat(a - b),
            BinOp::Mul => Expr::Rat(a * b),
            BinOp::Div => Expr::Rat(a / b),
          },
          (Expr::Int(a), Expr::Rat(b)) => match op {
            BinOp::Add => Expr::Rat(BigRational::from(a) + b),
            BinOp::Sub => Expr::Rat(BigRational::from(a) - b),
            BinOp::Mul => Expr::Rat(BigRational::from(a) * b),
            BinOp::Div => Expr::Rat(BigRational::from(a) / b),
          },
          (Expr::Rat(a), Expr::Int(b)) => match op {
            BinOp::Add => Expr::Rat(a + b),
            BinOp::Sub => Expr::Rat(a - b),
            BinOp::Mul => Expr::Rat(a * b),
            BinOp::Div => Expr::Rat(a / b),
          },

          // TODO: handle edge cases
          _ => todo!(),
        };
        Self::eval_expr(expr)
      }

      // TODO: handle edge cases
      _ => todo!(),
    }
  }
}

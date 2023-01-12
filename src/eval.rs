//! Contains the [`eval`].

use std::collections::HashMap;

use chumsky::zero_copy::{prelude::Rich, Parser};
use lasso::{Interner, Rodeo, Spur};

use crate::{
  lex::{self, *},
  parse::{self, *},
};

/// An evaluation context.
#[derive(Debug, PartialEq, Eq, Default)]
pub struct Context {
  interner: Rodeo<Spur>,
  defs: HashMap<Ident, Expr>,
}

impl Context {
  /// Evaluates a statement.
  pub fn eval(&mut self, source: &str) -> Stmt {
    let stmt = lex::lexer::<Rich<_>>()
      .parse_with_state(source, &mut self.interner)
      .into_result()
      .map(|tokens| parse::parser::<Rich<_>>().parse(&tokens).into_result())
      // TODO: handle errors
      .unwrap()
      .unwrap();

    stmt.eval(&mut self.defs)
  }

  /// Returns a pretty string for a statement.
  pub fn display(&self, stmt: &Stmt) -> String {
    display_stmt(stmt, &self.interner)
  }
}

/// Implemented by evaluatable types.
pub trait Evaluate {
  /// Evaluates a type into its simplest form.
  fn eval(self, defs: &mut HashMap<Ident, Expr>) -> Self;
}

impl Evaluate for Stmt {
  fn eval(self, defs: &mut HashMap<Ident, Expr>) -> Self {
    match self {
      Self::Expr(expr) => Self::Expr(expr.eval(defs)),

      Self::Def(ident, body) => {
        defs.insert(ident, body.clone());
        Self::Def(ident, body)
      }
    }
  }
}

impl Evaluate for Expr {
  fn eval(self, defs: &mut HashMap<Ident, Expr>) -> Self {
    match self {
      Self::Lit(lit) => Self::Lit(lit.eval(defs)),

      // TODO: handle errors
      Self::Var(ident) => defs.get(&ident).cloned().unwrap(),
      Self::Call(lhs, args) => match *lhs {
        Self::Lit(Lit::Fn(fn_args, body)) => {
          let mut scope = defs.clone();

          for (ident, expr) in fn_args.into_iter().zip(args.into_iter()) {
            let expr = expr.eval(defs);
            scope.insert(ident, expr);
          }

          body.eval(&mut scope)
        }
        _ => Self::Call(Box::new(lhs.eval(defs)), args).eval(defs),
      },

      Self::Unary(op, rhs) => match rhs.eval(defs) {
        Expr::Lit(Lit::Real(rhs)) => match op {
          UnOp::Neg => Expr::Lit(Lit::Real(-rhs)),
        },
        // TODO: handle errors
        _ => unimplemented!(),
      },
      Self::Binary(op, lhs, rhs) => match (lhs.eval(defs), rhs.eval(defs)) {
        (Expr::Lit(Lit::Real(lhs)), Expr::Lit(Lit::Real(rhs))) => {
          match op {
            BinOp::Add => Expr::Lit(Lit::Real(lhs + rhs)),
            BinOp::Sub => Expr::Lit(Lit::Real(lhs - rhs)),
            BinOp::Mul => Expr::Lit(Lit::Real(lhs * rhs)),
            BinOp::Div => Expr::Lit(Lit::Real(lhs / rhs)),
            BinOp::Pow => Expr::Lit(Lit::Real({
              if rhs.frac().is_zero() {
                let mut tmp = lhs;
                for _ in 0..rhs.int().to_num::<u64>() - 1 {
                  tmp *= lhs;
                }
                tmp
              } else {
                // TODO: handle errors
                unimplemented!()
              }
            })),

            BinOp::Eq => {
              if lhs == rhs {
                Self::Binary(
                  op,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              } else {
                Self::Binary(
                  BinOp::Ne,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              }
            }
            BinOp::Ne => {
              if lhs != rhs {
                Self::Binary(
                  op,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              } else {
                Self::Binary(
                  BinOp::Eq,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              }
            }
            BinOp::Lt => {
              if lhs < rhs {
                Self::Binary(
                  op,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              } else {
                Self::Binary(
                  BinOp::Ge,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              }
            }
            BinOp::Gt => {
              if lhs > rhs {
                Self::Binary(
                  op,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              } else {
                Self::Binary(
                  BinOp::Le,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              }
            }
            BinOp::Le => {
              if lhs <= rhs {
                Self::Binary(
                  op,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              } else {
                Self::Binary(
                  BinOp::Gt,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              }
            }
            BinOp::Ge => {
              if lhs >= rhs {
                Self::Binary(
                  op,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              } else {
                Self::Binary(
                  BinOp::Lt,
                  Box::new(Expr::Lit(Lit::Real(lhs))),
                  Box::new(Expr::Lit(Lit::Real(rhs))),
                )
              }
            }
          }
        }
        // TODO: handle errors
        _ => unimplemented!(),
      },
    }
  }
}

impl Evaluate for Lit {
  fn eval(self, _defs: &mut HashMap<Ident, Expr>) -> Self {
    match self {
      Self::Real(_) | Self::Fn(_, _) => self,
    }
  }
}

/// Creates a pretty string for a statement.
pub fn display_stmt(stmt: &Stmt, interner: &impl Interner<Ident>) -> String {
  match stmt {
    Stmt::Expr(expr) => display_expr(expr, interner),

    Stmt::Def(ident, body) => match body {
      Expr::Lit(Lit::Fn(args, body)) => {
        let args = args
          .iter()
          .map(|arg| interner.resolve(arg))
          .enumerate()
          .fold(String::new(), |mut acc, (i, arg)| {
            acc.push_str(&format!("{}{}", if i > 0 { ", " } else { "" }, arg));
            acc
          });
        format!(
          "let {}({}) = {}",
          interner.resolve(ident),
          args,
          display_expr(body, interner)
        )
      }
      _ => format!(
        "let {} = {}",
        interner.resolve(ident),
        display_expr(body, interner)
      ),
    },
  }
}

/// Creates a pretty string for an experssion.
pub fn display_expr(expr: &Expr, interner: &impl Interner<Ident>) -> String {
  match expr {
    Expr::Lit(lit) => display_lit(lit, interner),

    Expr::Var(ident) => interner.resolve(ident).to_string(),
    Expr::Call(lhs, args) => {
      let args = args
        .iter()
        .map(|arg| display_expr(arg, interner))
        .enumerate()
        .fold(String::new(), |mut acc, (i, arg)| {
          acc.push_str(&format!("{}{}", if i > 0 { ", " } else { "" }, arg));
          acc
        });
      format!("{}({})", display_expr(lhs, interner), args)
    }

    Expr::Unary(op, rhs) => format!("{}{}", op, display_expr(rhs, interner)),
    Expr::Binary(op, lhs, rhs) => format!(
      "{} {} {}",
      display_expr(lhs, interner),
      op,
      display_expr(rhs, interner)
    ),
  }
}

/// Creates a pretty string for a literal.
pub fn display_lit(lit: &Lit, interner: &impl Interner<Ident>) -> String {
  match lit {
    Lit::Real(a) => a.to_string(),
    Lit::Fn(args, body) => {
      let args = args
        .iter()
        .map(|arg| interner.resolve(arg))
        .enumerate()
        .fold(String::new(), |mut acc, (i, arg)| {
          acc.push_str(&format!("{}{}", if i > 0 { ", " } else { "" }, arg));
          acc
        });
      format!("({}) -> {}", args, display_expr(body, interner))
    }
  }
}

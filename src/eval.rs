//! Contains the evaluator.

use std::collections::HashMap;

use crate::ast::*;

/// An evaluation context.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Context {
  vars: HashMap<Ident, Expr>,
  fns: HashMap<Ident, (Vec<Ident>, Expr)>,
}

impl Stmt {
  ///
  pub fn eval(self, ctx: &mut Context) -> Self {
    match self {
      Self::Error => todo!(),

      Self::Expr(expr) => Self::Expr(expr.eval(ctx)),

      Self::VarDef { ident, expr } => {
        let expr = expr.eval(ctx);
        // TODO: maybe use a different return type?
        ctx.vars.insert(ident.clone(), expr.clone());
        Self::VarDef { ident, expr }
      }
      Self::FnDef { ident, args, body } => {
        // TODO: maybe use a different return type?
        ctx.fns.insert(ident.clone(), (args.clone(), body.clone()));
        Self::FnDef { ident, args, body }
      }
    }
  }
}

impl Expr {
  ///
  pub fn eval(self, ctx: &mut Context) -> Self {
    match self {
      Self::Error => todo!(),

      Self::Lit(lit) => Self::Lit(lit.eval(ctx)),
      // TODO: handle errors
      Self::Var(ident) => ctx.vars.get(&ident).cloned().unwrap(),
      Self::Fn(ident, args) => {
        // TODO: handle errors
        let (fn_args, fn_body) = ctx.fns.get(&ident).cloned().unwrap();

        let mut scope = Context {
          vars: ctx.vars.clone(),
          fns: HashMap::default(),
        };

        for (ident, expr) in fn_args.into_iter().zip(args.into_iter()) {
          let expr = expr.eval(ctx);
          scope.vars.insert(ident, expr);
        }

        fn_body.eval(&mut scope)
      }

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

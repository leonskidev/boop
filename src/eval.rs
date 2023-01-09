//! Contains the [`Evaluator`].

use std::collections::HashMap;

use chumsky::{Parser, Stream};

use crate::{ast::*, lex::Token};

/// An evaluation context.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Evaluator {
  vars: HashMap<Ident, Expr>,
  fns: HashMap<Ident, (Vec<Ident>, Expr)>,
}

impl Evaluator {
  /// Performs all the steps needed to evaluate source code.
  pub fn eval(&mut self, source: &str) -> Option<Stmt> {
    let tokens = Token::lexer().parse(source).unwrap();

    if let Some(eoi) = tokens.last().map(|(_, eoi)| eoi).cloned() {
      let stmt = Stmt::parser()
        .parse(Stream::from_iter(eoi, tokens.into_iter()))
        .unwrap();
      Some(stmt.eval(self))
    } else {
      None
    }
  }
}

/// Implemented by types that can be evaluated into simpler forms.
pub trait Evaluate {
  /// Evaluates this type into a simpler form.
  fn eval(self, ctx: &mut Evaluator) -> Self;
}

impl Evaluate for Stmt {
  fn eval(self, ctx: &mut Evaluator) -> Self {
    match self {
      Self::Error => Self::Error,

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

impl Evaluate for Expr {
  fn eval(self, ctx: &mut Evaluator) -> Self {
    match self {
      Self::Error => Self::Error,

      Self::Num(lit) => Self::Num(lit.eval(ctx)),
      // TODO: handle errors
      Self::Var(ident) => ctx.vars.get(&ident).cloned().unwrap(),
      Self::Fn(ident, args) => {
        // TODO: handle errors
        let (fn_args, fn_body) = ctx.fns.get(&ident).cloned().unwrap();

        let mut scope = Evaluator {
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
        Expr::Num(rhs) => match op {
          UnOp::Neg => Expr::Num(-rhs),
        },
        _ => unimplemented!(),
      },
      Self::Binary(op, lhs, rhs) => match (lhs.eval(ctx), rhs.eval(ctx)) {
        (Expr::Num(lhs), Expr::Num(rhs)) => match op {
          BinOp::Add => Expr::Num(lhs + rhs),
          BinOp::Sub => Expr::Num(lhs - rhs),
          BinOp::Mul => Expr::Num(lhs * rhs),
          BinOp::Div => Expr::Num(lhs / rhs),
          BinOp::Eql => {
            if lhs == rhs {
              Self::Binary(
                op,
                Box::new(Expr::Num(lhs)),
                Box::new(Expr::Num(rhs)),
              )
            } else {
              Self::Error
            }
          }
        },
        _ => unimplemented!(),
      },
    }
  }
}

impl Evaluate for Num {
  #[inline]
  fn eval(self, _ctx: &mut Evaluator) -> Self {
    self
  }
}

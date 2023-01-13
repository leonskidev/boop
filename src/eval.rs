//! Contains the evaluator.

use std::collections::HashMap;

use chumsky::zero_copy::{error::Error, Parser};
use lasso::Interner;

use crate::lex::{self, Symbol, Token};
use crate::parse::{self, BinOp, Expr, UnOp};

/// An evaluation engine.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Engine<I: Interner> {
  interner: I,
}

impl<I: Interner> Engine<I> {
  /// Creates a new [`Engine`].
  #[inline]
  pub const fn new(interner: I) -> Self {
    Self { interner }
  }

  /// Returns a reference to the engine interner.
  #[inline]
  pub const fn interner(&self) -> &I {
    &self.interner
  }

  /// Returns a mutable reference to the engine interner.
  #[inline]
  // TODO: make this const when it stablised
  pub fn interner_mut(&mut self) -> &mut I {
    &mut self.interner
  }

  /// Evaluate a <code>&[str](primitive@str)</code>.
  #[inline]
  pub fn eval<LE, PE>(
    &mut self,
    input: &str,
  ) -> CompileResult<EvalResult<Expr>, LE, PE>
  where
    LE: Error<str>,
    PE: Error<[Token]>,
  {
    self.eval_with_scope::<LE, PE>(&mut Scope::default(), input)
  }

  /// Evaluate a <code>&[str](primitive@str)</code> with the provided [`Scope`].
  #[inline]
  pub fn eval_with_scope<LE, PE>(
    &mut self,
    scope: &mut Scope,
    input: &str,
  ) -> CompileResult<EvalResult<Expr>, LE, PE>
  where
    LE: Error<str>,
    PE: Error<[Token]>,
  {
    let expr = self.compile::<LE, PE>(input)?;
    Ok(self.eval_expr_with_scope(scope, expr))
  }

  /// Evaluate an [`Expr`].
  #[inline]
  pub fn eval_expr(&self, expr: Expr) -> EvalResult<Expr> {
    self.eval_expr_with_scope(&mut Scope::default(), expr)
  }

  /// Evaluate an [`Expr`] with the provided [`Scope`].
  pub fn eval_expr_with_scope(
    &self,
    scope: &mut Scope,
    expr: Expr,
  ) -> EvalResult<Expr> {
    match expr {
      Expr::Real(_) | Expr::Fn(_, _) => Ok(expr),
      Expr::Var(symbol) => Ok(
        scope
          .vars()
          .get(&symbol)
          .cloned()
          .unwrap_or(Expr::Var(symbol)),
      ),

      Expr::Call(r#fn, exprs) => {
        match self.eval_expr_with_scope(scope, *r#fn)? {
          Expr::Fn(symbols, body) => {
            let mut fn_scope = scope.clone();
            symbols.into_iter().zip(exprs.into_iter()).try_for_each(
              |(symbol, expr)| {
                let expr = self.eval_expr_with_scope(scope, expr)?;
                fn_scope.vars_mut().insert(symbol, expr);
                Ok(())
              },
            )?;

            self.eval_expr_with_scope(&mut fn_scope, *body)
          }
          r#fn => Ok(Expr::Call(Box::new(r#fn), exprs)),
        }
      }

      Expr::Unary(op, rhs) => match self.eval_expr_with_scope(scope, *rhs)? {
        Expr::Real(rhs) => match op {
          UnOp::Neg => Ok(Expr::Real(-rhs)),
        },
        rhs => Err(EvalError::Unary(op, rhs)),
      },
      Expr::Binary(op, lhs, rhs) => match (
        self.eval_expr_with_scope(scope, *lhs)?,
        self.eval_expr_with_scope(scope, *rhs)?,
      ) {
        (Expr::Real(lhs), Expr::Real(rhs)) => match op {
          BinOp::Add => Ok(Expr::Real(lhs + rhs)),
          BinOp::Sub => Ok(Expr::Real(lhs - rhs)),
          BinOp::Mul => Ok(Expr::Real(lhs * rhs)),
          BinOp::Div => Ok(Expr::Real(lhs / rhs)),

          BinOp::Eq => Ok(
            (lhs == rhs)
              .then(|| {
                Expr::Binary(
                  op,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              })
              .unwrap_or_else(|| {
                Expr::Binary(
                  BinOp::Ne,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              }),
          ),
          BinOp::Ne => Ok(
            (lhs != rhs)
              .then(|| {
                Expr::Binary(
                  op,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              })
              .unwrap_or_else(|| {
                Expr::Binary(
                  BinOp::Eq,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              }),
          ),
          BinOp::Lt => Ok(
            (lhs < rhs)
              .then(|| {
                Expr::Binary(
                  op,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              })
              .unwrap_or_else(|| {
                Expr::Binary(
                  BinOp::Ge,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              }),
          ),
          BinOp::Gt => Ok(
            (lhs > rhs)
              .then(|| {
                Expr::Binary(
                  op,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              })
              .unwrap_or_else(|| {
                Expr::Binary(
                  BinOp::Le,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              }),
          ),
          BinOp::Le => Ok(
            (lhs <= rhs)
              .then(|| {
                Expr::Binary(
                  op,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              })
              .unwrap_or_else(|| {
                Expr::Binary(
                  BinOp::Gt,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              }),
          ),
          BinOp::Ge => Ok(
            (lhs >= rhs)
              .then(|| {
                Expr::Binary(
                  op,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              })
              .unwrap_or_else(|| {
                Expr::Binary(
                  BinOp::Lt,
                  Box::new(Expr::Real(lhs)),
                  Box::new(Expr::Real(rhs)),
                )
              }),
          ),
        },
        (lhs, rhs) => Err(EvalError::Binary(op, lhs, rhs)),
      },

      Expr::Let(symbol, body) => {
        let body = self.eval_expr_with_scope(scope, *body)?;
        scope.vars_mut().insert(symbol, body.clone());
        Ok(Expr::Let(symbol, Box::new(body)))
      }
    }
  }

  /// Compile a <code>&[str](primitive@str)</code>.
  #[inline]
  pub fn compile<LE, PE>(&mut self, input: &str) -> CompileResult<Expr, LE, PE>
  where
    LE: Error<str>,
    PE: Error<[Token]>,
  {
    lex::lexer()
      .parse_with_state(input, &mut self.interner)
      .into_result()
      .map_err(CompileError::Lex)
      .and_then(|tokens| {
        parse::parser()
          .parse(&tokens)
          .into_result()
          .map_err(CompileError::Parse)
      })
  }
}

/// An evaluation scope.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Scope {
  vars: HashMap<Symbol, Expr>,
}

impl Scope {
  /// Creates a [`Scope`].
  #[inline]
  pub const fn new(vars: HashMap<Symbol, Expr>) -> Self {
    Self { vars }
  }

  /// Returns a reference to the scope variables.
  #[inline]
  pub const fn vars(&self) -> &HashMap<Symbol, Expr> {
    &self.vars
  }

  /// Returns a mutable reference to the scope variables.
  #[inline]
  // TODO: make this const when it stablised
  pub fn vars_mut(&mut self) -> &mut HashMap<Symbol, Expr> {
    &mut self.vars
  }
}

/// A compilation result convenience type.
pub type CompileResult<T, LE, PE> = Result<T, CompileError<LE, PE>>;

/// An evaluation result convenience type.
pub type EvalResult<T> = Result<T, EvalError>;

/// A compilation error.
#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error)]
pub enum CompileError<LE, PE>
where
  LE: Error<str>,
  PE: Error<[Token]>,
{
  /// A lexer error.
  #[error("a lexer error")]
  Lex(Vec<LE>),
  /// A parser error.
  #[error("a parser error")]
  Parse(Vec<PE>),
}

/// An evaluation error.
#[derive(Debug, Clone, PartialEq, Eq, Hash, thiserror::Error)]
pub enum EvalError {
  /// An invalid unary operand.
  #[error("an invalid unary operand")]
  Unary(UnOp, Expr),
  /// An invalid binary operand(s).
  #[error("an invalid binary operand(s)")]
  Binary(BinOp, Expr, Expr),
}

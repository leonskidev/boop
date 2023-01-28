//! Contains the parser.

use core::iter::Peekable;

use syntree::{Builder, Error, Tree};

use crate::{lex::Lex, syntax::Syntax};

/// A parser.
#[derive(Debug, Clone)]
pub struct Parser<'a> {
  tokens: Peekable<Lex<'a>>,
  builder: Builder<Syntax>,
}

impl<'a> Parser<'a> {
  /// Creates a [`Parser`].
  #[inline]
  pub fn new(tokens: Lex<'a>) -> Self {
    Self {
      tokens: tokens.peekable(),
      builder: Builder::new(),
    }
  }

  /// Parses a flat [`Syntax`] collection into a tree.
  pub fn parse(mut self) -> Result<Tree<Syntax>, Error> {
    self.builder.open(Syntax::Root)?;
    self.parse_sum()?;
    self.builder.close()?;
    self.builder.build()
  }

  fn parse_sum(&mut self) -> Result<(), Error> {
    self.parse_binary(&[Syntax::Add, Syntax::Sub], Self::parse_product)
  }

  fn parse_product(&mut self) -> Result<(), Error> {
    self.parse_binary(&[Syntax::Mul, Syntax::Div], Self::parse_atom)
  }

  fn parse_atom(&mut self) -> Result<(), Error> {
    match self.peek()? {
      Some(Syntax::Number | Syntax::Symbol) => self.bump()?,
      Some(Syntax::LeftBracket) => {
        self.bump()?;

        let checkpoint = self.builder.checkpoint()?;
        self.parse_sum()?;
        self.builder.close_at(&checkpoint, Syntax::Group)?;

        if self.peek()? != Some(Syntax::RightBracket) {
          self.builder.open(Syntax::Error)?;
          self.bump()?;
          self.builder.close()?;
        } else {
          self.bump()?;
        }
      }
      _ => {
        self.builder.open(Syntax::Error)?;
        self.bump()?;
        self.builder.close()?;
      }
    }
    Ok(())
  }

  fn parse_binary(
    &mut self,
    tokens: &[Syntax],
    next: fn(&mut Self) -> Result<(), Error>,
  ) -> Result<(), Error> {
    let checkpoint = self.builder.checkpoint()?;
    next(self)?;

    while self.peek()?.map_or(false, |token| tokens.contains(&token)) {
      self.bump()?;
      next(self)?;
      self.builder.close_at(&checkpoint, Syntax::Binary)?;
    }

    Ok(())
  }

  fn peek(&mut self) -> Result<Option<Syntax>, Error> {
    while self
      .tokens
      .peek()
      .map_or(false, |&(token, _)| token == Syntax::Whitespace)
    {
      self.bump()?;
    }
    Ok(self.tokens.peek().map(|&(token, _)| token))
  }

  fn bump(&mut self) -> Result<(), Error> {
    if let Some((token, span)) = self.tokens.next() {
      self.builder.token(token, span.len())?;
    }
    Ok(())
  }
}

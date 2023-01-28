//! Contains the lexer.

use core::{
  iter::{self, Enumerate, Peekable},
  ops::Range,
  str::Chars,
};

use crate::syntax::Syntax;

/// A lexer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Lexer<'a> {
  source: &'a str,
}

impl<'a> Lexer<'a> {
  /// Creates a [`Lexer`].
  #[inline]
  pub const fn new(source: &'a str) -> Self {
    Self { source }
  }

  /// Returns the <code>&[str]</code> slice within the input <code>&[str]</code>
  /// that a span refers to.
  #[inline]
  pub fn slice(&self, span: Span) -> Option<&str> {
    self.source.get(span)
  }

  /// Creates a [`Lex`].
  #[inline]
  pub fn lex(&self) -> Lex<'a> {
    Lex::new(self.source)
  }
}

impl<'a> IntoIterator for Lexer<'a> {
  type IntoIter = Lex<'a>;
  type Item = <Lex<'a> as Iterator>::Item;

  #[inline]
  fn into_iter(self) -> Self::IntoIter {
    self.lex()
  }
}

/// A lexer iterator.
#[derive(Debug, Clone)]
pub struct Lex<'a> {
  chars: Peekable<Enumerate<Chars<'a>>>,
}

impl<'a> Lex<'a> {
  /// Creates a [`Lex`].
  #[inline]
  pub fn new(source: &'a str) -> Self {
    Self {
      chars: source.chars().enumerate().peekable(),
    }
  }

  fn take_while(&mut self, f: impl Fn(&char) -> bool) -> Option<usize> {
    iter::from_fn(|| self.chars.next_if(|(_, ch)| f(ch)))
      .last()
      .map(|(i, _)| i + 1)
  }
}

impl<'a> Iterator for Lex<'a> {
  type Item = (Syntax, Span);

  fn next(&mut self) -> Option<Self::Item> {
    let (start, ch) = self.chars.next()?;
    match ch {
      '+' => Some((Syntax::Add, start..start + 1)),
      '-' => Some((Syntax::Sub, start..start + 1)),
      '*' => Some((Syntax::Mul, start..start + 1)),
      '/' => Some((Syntax::Div, start..start + 1)),
      '(' => Some((Syntax::LeftBracket, start..start + 1)),
      ')' => Some((Syntax::RightBracket, start..start + 1)),
      ch if ch.is_whitespace() => {
        let end = self
          .take_while(|ch| ch.is_whitespace())
          .unwrap_or(start + 1);
        Some((Syntax::Whitespace, start..end))
      }
      ch if ch.is_ascii_digit() => {
        let mut end =
          self.take_while(char::is_ascii_digit).unwrap_or(start + 1);

        if self.chars.next_if(|(_, ch)| *ch == '.').is_some() {
          end = self.take_while(char::is_ascii_digit).unwrap_or(end + 1);
        }

        Some((Syntax::Number, start..end))
      }
      ch if ch.is_alphabetic() => {
        let end = self
          .take_while(|ch| ch.is_alphabetic())
          .unwrap_or(start + 1);
        Some((Syntax::Symbol, start..end))
      }
      _ => {
        let end = self
          .take_while(|ch| !ch.is_whitespace())
          .unwrap_or(start + 1);
        Some((Syntax::Error, start..end))
      }
    }
  }
}

/// A span within a source.
pub type Span = Range<usize>;

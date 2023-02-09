//! Contains the lexer.

use core::{
  fmt,
  iter::{self, Enumerate, Peekable},
  ops::Range,
  str::Chars,
};

use num_bigint::BigInt;
use num_rational::BigRational;

/// A lexer which converts a sequence of [`char`]s into [`Token`]s.
#[derive(Debug, Clone)]
pub struct Lexer<'a> {
  input: &'a str,
  chars: Peekable<Enumerate<Chars<'a>>>,
}

impl<'a> Lexer<'a> {
  /// Creates a [`Lexer`].
  #[inline]
  pub fn new(input: &'a str) -> Self {
    Self {
      input,
      chars: input.chars().enumerate().peekable(),
    }
  }

  /// Returns a <code>&[str]</code> within the input at a [`Span`].
  #[inline]
  pub fn slice(&self, span: Span) -> Option<&'a str> {
    self.input.get(span)
  }

  fn take_while(&mut self, f: fn(char) -> bool) -> Option<usize> {
    iter::from_fn(|| self.chars.next_if(|&(_, ch)| f(ch)))
      .last()
      .map(|(end, _)| end + 1)
  }

  fn take_error(&mut self, start: usize) -> Token {
    let end = self
      .take_while(|ch| !ch.is_whitespace() && !"+-*/()".contains(ch))
      .unwrap_or(start + 1);
    Token::new(TokenKind::Error, start..end)
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Token;

  fn next(&mut self) -> Option<Self::Item> {
    self.chars.next().map(|(start, ch)| match ch {
      '+' => Token::new(TokenKind::Plus, start..start + 1),
      '-' => Token::new(TokenKind::Minus, start..start + 1),
      '*' => Token::new(TokenKind::Asterisk, start..start + 1),
      '/' => Token::new(TokenKind::Slash, start..start + 1),
      '(' => Token::new(TokenKind::LeftBracket, start..start + 1),
      ')' => Token::new(TokenKind::RightBracket, start..start + 1),

      ch if ch.is_ascii_digit() => {
        let end = self
          .take_while(|ch| ch.is_ascii_digit())
          .unwrap_or(start + 1);

        if self.chars.next_if(|&(_, ch)| ch == '.').is_some() {
          // TODO: handle demimals by converting them to rationals
          self.take_error(start)
        } else {
          self
            .slice(start..end)
            .unwrap()
            .parse()
            .ok()
            .map(|i| Token::new(TokenKind::Int(i), start..end))
            .unwrap_or(self.take_error(start))
        }
      }

      ch if ch.is_whitespace() => {
        let end = self.take_while(char::is_whitespace).unwrap_or(start + 1);
        Token::new(TokenKind::Whitespace, start..end)
      }
      _ => self.take_error(start),
    })
  }
}

/// Convenience type for spans within an input.
pub type Span = Range<usize>;

/// A token.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
  /// The kind.
  pub kind: TokenKind,
  /// The span.
  pub span: Span,
}

impl Token {
  /// Creates a [`Token`].
  #[inline]
  pub const fn new(kind: TokenKind, span: Span) -> Self {
    Self { kind, span }
  }
}

/// A token kind.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
  /// An integer.
  Int(BigInt),
  /// A rational.
  Rat(BigRational),

  /// The `+` symbol.
  Plus,
  /// The `-` symbol.
  Minus,
  /// The `*` symbol.
  Asterisk,
  /// The `/` symbol.
  Slash,
  /// The `(` symbol.
  LeftBracket,
  /// The `)` symbol.
  RightBracket,

  /// Consecutive whitespace [`char`]s.
  Whitespace,
  /// Consecutive unknown or invalid [`char`]s.
  Error,
}

impl fmt::Display for TokenKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Int(i) => write!(f, "{i}"),
      Self::Rat(r) => write!(f, "{r}"),

      Self::Plus => write!(f, "+"),
      Self::Minus => write!(f, "-"),
      Self::Asterisk => write!(f, "*"),
      Self::Slash => write!(f, "/"),
      Self::LeftBracket => write!(f, "("),
      Self::RightBracket => write!(f, ")"),

      Self::Whitespace => write!(f, " "),
      Self::Error => write!(f, "<error>",),
    }
  }
}

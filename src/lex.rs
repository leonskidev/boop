//! Contains the lexer.

use core::{fmt, ops::Range};

use chumsky::prelude::*;
use fixed::types::extra::U64;
use fixed::FixedI128;

/// Represents a span within a source.
pub type Span = Range<usize>;

/// Represents a valid source token.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
  /// A number literal: `123`, `1.5`.
  Num(FixedI128<U64>),
  /// An identifier: `f`, `hello`.
  Ident(String),

  /// The `let` keyword.
  Let,

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
  /// The `,` symbol.
  Comma,
  /// The `=` symbol.
  Equals,
}

impl Token {
  /// Creates a [`Token`] lexer.
  pub fn lexer(
  ) -> impl Parser<char, Vec<(Self, Span)>, Error = Simple<char>> + Clone {
    let num = text::int(10)
      .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
      .collect::<String>()
      .try_map(|s, span| {
        s.parse()
          .map_err(|e| Simple::custom(span, format!("{}", e)))
      })
      .map(Self::Num);

    let ident = text::ident().map(Self::Ident);

    let keyword = choice((text::keyword("let").to(Self::Let),));

    let symbol = choice((
      just('+').to(Self::Plus),
      just('-').to(Self::Minus),
      just('*').to(Self::Asterisk),
      just('/').to(Self::Slash),
      just('(').to(Self::LeftBracket),
      just(')').to(Self::RightBracket),
      just(',').to(Self::Comma),
      just('=').to(Self::Equals),
    ));

    let token = num.or(keyword).or(ident).or(symbol);

    token
      .map_with_span(|token, span| (token, span))
      .padded()
      .repeated()
      .then_ignore(end().recover_with(skip_then_retry_until([])))
  }
}

impl fmt::Display for Token {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Self::Num(a) => write!(f, "{}", a),
      Self::Ident(a) => write!(f, "{}", a),

      Self::Let => write!(f, "let"),

      Self::Plus => write!(f, "+"),
      Self::Minus => write!(f, "-"),
      Self::Asterisk => write!(f, "*"),
      Self::Slash => write!(f, "/"),
      Self::LeftBracket => write!(f, "("),
      Self::RightBracket => write!(f, ")"),
      Self::Comma => write!(f, ","),
      Self::Equals => write!(f, "="),
    }
  }
}

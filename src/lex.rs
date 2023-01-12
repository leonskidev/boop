//! Contains the lexer.

use chumsky::zero_copy::{error::Error, prelude::*};
use fixed::{types::extra::U64, FixedI128};
use lasso::{Rodeo, Spur};

/// The state provided to a [`lexer`].
pub type State = Rodeo<Spur>;

/// Creates a lexer.
pub fn lexer<'a, E>() -> impl Parser<'a, str, Vec<Token>, E, State>
where
  E: 'a + Error<str>,
{
  let real = text::int::<_, _, E, _>(10)
    .then(just('.').ignore_then(text::digits(10)).or_not())
    .map(|(lhs, rhs)| {
      let mut real = lhs.to_string();
      if let Some(rhs) = rhs {
        real.push('.');
        real.push_str(rhs);
      }
      real
    })
    .from_str()
    // TODO: handle errors
    .unwrapped()
    .map(Token::Real);

  let ident = text::ident::<_, _, _, State>()
    .map_with_state(|s, _, state| state.get_or_intern(s))
    .map(Token::Ident);

  let keyword = text::keyword("let").to(Token::Let);

  let symbol = choice((
    just("->").to(Token::RightArrow),
    just("!=").to(Token::ExclamationEquals),
    just("<=").to(Token::LeftAngleBracketEquals),
    just(">=").to(Token::RightAngleBracketEquals),
    //
    just('+').to(Token::Plus),
    just('-').to(Token::Minus),
    just('*').to(Token::Asterisk),
    just('/').to(Token::Slash),
    just('^').to(Token::Circumflex),
    just(',').to(Token::Comma),
    just('=').to(Token::Equals),
    //
    just('(').to(Token::LeftBracket),
    just(')').to(Token::RightBracket),
    just('<').to(Token::LeftAngleBracket),
    just('>').to(Token::RightAngleBracket),
  ));

  let token = real.or(keyword).or(ident).or(symbol);

  token.padded().repeated().collect().then_ignore(end())
}

/// Represents a source code token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
  /// A real number.
  Real(Real),
  /// An identifier.
  Ident(Ident),

  /// The `let` keyword.
  Let,

  /// The `->` symbol.
  RightArrow,
  /// The `!=` symbol.
  ExclamationEquals,
  /// The `<=` symbol.
  LeftAngleBracketEquals,
  /// The `>=` symbol.
  RightAngleBracketEquals,

  /// The `+` symbol.
  Plus,
  /// The `-` symbol.
  Minus,
  /// The `*` symbol.
  Asterisk,
  /// The `/` symbol.
  Slash,
  /// The `^` symbol.
  Circumflex,
  /// The `,` symbol.
  Comma,
  /// The `=` symbol.
  Equals,

  /// The `(` symbol.
  LeftBracket,
  /// The `)` symbol.
  RightBracket,
  /// The `<` symbol.
  LeftAngleBracket,
  /// The `>` symbol.
  RightAngleBracket,
}

/// Represents a real number.
pub type Real = FixedI128<U64>;

/// Represents an identifier.
pub type Ident = Spur;

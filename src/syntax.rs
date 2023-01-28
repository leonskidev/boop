//! Contains the syntax.

/// A syntax token.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Syntax {
  /// An unknown or invalid syntax.
  Error,
  /// Consecutive whitespace.
  Whitespace,

  /// A number: `123`, `1.23`.
  Number,
  /// A symbol: `x`, `sin`.
  Symbol,

  /// The `+` symbol.
  Add,
  /// The `-` symbol.
  Sub,
  /// The `*` symbol.
  Mul,
  /// The `/` symbol.
  Div,

  /// The `(` symbol.
  LeftBracket,
  /// The `)` symbol.
  RightBracket,

  /// A root.
  Root,
  /// A precedence group.
  Group,
  /// A binary operation.
  Binary,
}

//! Low-level Cobol lexer.
//!
//! The idea with `cobol_lexer` is to make a reusable library,
//! by separating out pure lexing and cobol-specific concerns, like spans,
//! error reporting, and interning. So, `cobol_lexer` operates directly on
//! `&str`, produces simple tokens which are a pair of type-tag and a bit
//! of original text, and does not report errors, instead storing them
//! as flags on the token.
//!
//! Tokens produced by this lexer are not yet ready for parsing the Cobol syntax.
//! TODO: Explain more
//!
//! The purpose of this crate is to convert raw sources into a labeled sequence
//! of well-known token types, so building an actual Cobol token stream will
//! be easier.
//!
//! The main entity of this crate is the [`TokenKind`] enum which represents common
//! lexeme types.

// We want to be able to build this crate with a stable compiler,
// so no `#![feature]` attributes should be added.
#![deny(unstable_features)]

mod cursor;

#[cfg(test)]
mod tests;

pub use crate::cursor::Cursor;

/// Parsed token.
/// It doesn't contain information about data that has been parsed,
/// only the type of the token and its size.
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u32,
}

impl Token {
    fn new(kind: TokenKind, len: u32) -> Token {
        Token { kind, len }
    }
}

/// Enum representing common lexeme types.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    // Multi-char tokens:
    /// "*> comment"
    InlineComment,

    /// Any whitespace character sequence.
    Whitespace,

    /// "ident" or "continue"
    ///
    /// At this step, keywords are  also considered identifiers.
    /// Note that invalid ident with ending `-` is also catched, user
    /// must check this invalid token by themself.
    Ident,

    /// Like the above, but containing invalid unicode codepoints.
    InvalidIdent,

    /// Like  `Ident` but only include number
    NumberIdent,

    /// An unknown prefix, like `foo'`, `foo"`.
    ///
    /// Note that only the
    /// prefix (`foo`) is included in the token, not the separator (which is
    /// lexed as its own distinct token).
    UnknownPrefix,

    /// Examples: `12.0`, `1.0e-40`, `x"123"`.
    ///
    /// See [LiteralKind] for more details.
    Literal { kind: LiteralKind, suffix_start: u32 },

    // One-char tokens:
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// ":"
    Colon,
    /// "$"
    Dollar,
    /// "="
    Eq,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "-"
    Minus,
    /// "+"
    Plus,
    /// "*"
    Star,
    /// "/"
    Slash,

    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown,

    /// End of input.
    Eof,
}

/// Enum representing the literal types supported by the lexer.
///
/// Note that the suffix is *not* considered when deciding the `LiteralKind` in
/// this type. This means that float literals like `1f32` are classified by this
/// type as `Int`. (Compare against `rustc_ast::token::LitKind` and
/// `rustc_ast::ast::LitKind`).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    /// `12`
    Int,
    /// `12.34`, `1.0e3`, `1.0e`
    Float { empty_exponent: bool },
    /// `"abc"`, `"abc`, `'abc'`
    Str { terminated: bool },
    /// `x"ff00"`, `X"1234f`, `x'ff00'`
    HexStr { terminated: bool },
    /// `z"abcxyz"`, `Z"abc0324z`, `z'abc'`
    NullStr { terminated: bool },
    /// `g"<abcxyz>"`, `G"<abc0324z>`, `g'<abc>'`
    DbcsStr { terminated: bool },
    /// `n"<abcxyz>"`, `N"<abc0324z>`, `n'<abc>'`
    NationalOrDbcsStr { terminated: bool },
    /// `nx"<abcxyz>"`, `NX"<abc0324z>`, `nx'<abc>'`
    NationalHexStr { terminated: bool },
    /// `u"abc"`, `U"abc`, `u'abc'`
    Utf8Str { terminated: bool },
    /// `ux"abc"`, `UX"abc`, `ux'abc'`
    Utf8HexStr { terminated: bool },
}

/// Creates an iterator that produces tokens from the input string. It assumes
/// that the input is already split by line.
pub fn tokenize(input: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        let token = cursor.advance_token();
        if token.kind != TokenKind::Eof {
            Some(token)
        } else {
            None
        }
    })
}

/// True if `c` is valid as a whitespace.
pub fn is_whitespace(c: char) -> bool {
    matches!(
        c,
        '\u{0020}' // space
        | '\u{000D}' // \r
        | '\u{000A}' // \n
    )
}

/// True if `c` is valid as a first character of an identifier.
pub fn is_id_start(c: char) -> bool {
    c.is_ascii_alphanumeric()
}

/// True if `c` is valid as a non-first character of an identifier.
pub fn is_id_continue(c: char) -> bool {
    matches!(c, '_' | '-') || is_id_start(c)
}

/// The passed string is lexically an identifier.
pub fn is_ident(string: &str) -> bool {
    let mut chars = string.chars();
    if let Some(start) = chars.next() {
        is_id_start(start) && chars.all(is_id_continue)
    } else {
        false
    }
}

impl Cursor<'_> {
    /// Parses a token from the input string.
    pub fn advance_token(&mut self) -> Token {
        let first_char = match self.bump() {
            Some(c) => c,
            None => return Token::new(TokenKind::Eof, 0),
        };
        let token_kind = match first_char {
            // Slash, comment or block comment.
            '*' => match self.first() {
                '>' => self.inline_comment(),
                _ => TokenKind::Star,
            },

            // Whitespace sequence.
            c if is_whitespace(c) => self.whitespace(),

            // Hex str
            'x' | 'X' => {
                self.string_literal_or_ident(|terminated| LiteralKind::HexStr { terminated })
            }

            // Null str
            'z' | 'Z' => {
                self.string_literal_or_ident(|terminated| LiteralKind::NullStr { terminated })
            }

            // Dbcs G str
            'g' | 'G' => {
                self.string_literal_or_ident(|terminated| LiteralKind::DbcsStr { terminated })
            }

            // Dbcs or National str, National hex str
            'n' | 'N' => match (self.first(), self.second()){
                (c @ ('\'' | '"'), _) => {
                    self.bump();
                    self.string_literal(|terminated| LiteralKind::NationalOrDbcsStr { terminated }, c)
                }
                ('x' | 'X', c @ ('\'' | '"')) => {
                    self.bump();
                    self.bump();
                    self.string_literal(|terminated| LiteralKind::NationalHexStr { terminated }, c)
                }
                _ => self.ident_or_unknown_prefix(),
            }

            // Utf-8 str, Utf-8 hex str
            'u' | 'U' => match (self.first(), self.second()) {
                (c @ ('\'' | '"'), _) => {
                    self.bump();
                    self.string_literal(|terminated| LiteralKind::Utf8Str { terminated }, c)
                }
                ('x' | 'X', c @ ('\'' | '"')) => {
                    self.bump();
                    self.bump();
                    self.string_literal(|terminated| LiteralKind::Utf8HexStr { terminated }, c)
                }
                _ => self.ident_or_unknown_prefix(),
            },

            '0'..='9' => self.number_or_ident(),

            // Identifier (this should be checked after other variant that can
            // start as identifier).
            c if is_id_start(c) => self.ident_or_unknown_prefix(),

            // One-symbol tokens.
            ';' => TokenKind::Semi,
            ',' => TokenKind::Comma,
            '.' => TokenKind::Dot,
            '(' => TokenKind::OpenParen,
            ')' => TokenKind::CloseParen,
            ':' => TokenKind::Colon,
            '$' => TokenKind::Dollar,
            '=' => TokenKind::Eq,
            '<' => TokenKind::Lt,
            '>' => TokenKind::Gt,
            '-' => match self.first() {
                '0'..='9' => {
                    self.bump();
                    self.number()
                }
                _ => TokenKind::Minus,
            },
            '+' => match self.first() {
                '0'..='9' => {
                    self.bump();
                    self.number()
                }
                _ => TokenKind::Plus,
            },

            // String literal.
            c @ ('\'' | '"') => {
                self.string_literal(|terminated| LiteralKind::Str { terminated }, c)
            }
            // Identifier starting with an non-ascii. Only lexed for graceful error recovery.
            c if !c.is_ascii() => TokenKind::InvalidIdent,
            _ => TokenKind::Unknown,
        };
        let res = Token::new(token_kind, self.pos_within_token());
        self.reset_pos_within_token();
        res
    }

    fn inline_comment(&mut self) -> TokenKind {
        debug_assert!(self.prev() == '*' && self.first() == '>');
        self.bump();

        self.eat_while(|c| c != '\n');
        TokenKind::InlineComment
    }

    fn whitespace(&mut self) -> TokenKind {
        debug_assert!(is_whitespace(self.prev()));
        self.eat_while(is_whitespace);
        TokenKind::Whitespace
    }

    fn string_literal_or_ident(&mut self, mk_kind: impl FnOnce(bool) -> LiteralKind) -> TokenKind {
        match self.first() {
            c @ ('\'' | '"') => {
                self.bump();
                self.string_literal(mk_kind, c)
            }
            _ => self.ident_or_unknown_prefix(),
        }
    }

    fn string_literal(
        &mut self,
        mk_kind: impl FnOnce(bool) -> LiteralKind,
        quote_sym: char,
    ) -> TokenKind {
        debug_assert!(matches!(self.prev(), '\'' | '"'));
        let terminated = self.eat_quoted_string(quote_sym);
        let suffix_start = self.pos_within_token();
        if terminated {
            self.eat_literal_suffix();
        }
        TokenKind::Literal { kind: mk_kind(terminated), suffix_start }
    }

    fn ident_or_unknown_prefix(&mut self) -> TokenKind {
        debug_assert!(is_id_start(self.prev()));
        // Start is already eaten, eat the rest of identifier.
        self.eat_while(is_id_continue);
        // Known prefixes must have been handled earlier. So if
        // we see a prefix here, it is definitely an unknown prefix.
        match self.first() {
            '"' | '\'' => TokenKind::UnknownPrefix,
            c if !c.is_ascii() => TokenKind::InvalidIdent,
            _ => TokenKind::Ident,
        }
    }

    fn number_or_ident(&mut self) -> TokenKind {
        debug_assert!('0' <= self.prev() && self.prev() <= '9');
        self.eat_decimal_digits();
        match self.first() {
            c if is_id_continue(c) => self.ident_or_unknown_prefix(),
            '.' => self.number(),
            _ => TokenKind::NumberIdent,
        }
    }

    fn number(&mut self) -> TokenKind {
        debug_assert!('0' <= self.prev() && self.prev() <= '9');
        self.eat_decimal_digits();
        let kind = match self.first() {
            '.' if self.second() != '.' => {
                self.bump();
                let mut empty_exponent = false;
                if self.first().is_ascii_digit() {
                    self.eat_decimal_digits();
                    match self.first() {
                        'e' | 'E' => {
                            self.bump();
                            empty_exponent = !self.eat_float_exponent();
                        }
                        _ => (),
                    }
                }
                LiteralKind::Float { empty_exponent }
            }
            _ => LiteralKind::Int,
        };
        let suffix_start = self.pos_within_token();
        self.eat_literal_suffix();
        TokenKind::Literal { kind, suffix_start }
    }

    /// Eats quoted string and returns true
    /// if string is terminated.
    fn eat_quoted_string(&mut self, quote_sym: char) -> bool {
        debug_assert!(self.prev() == quote_sym && (self.prev() == '"' || self.prev() == '\''));
        while let Some(c) = self.bump() {
            match c {
                '\n' => return false,
                x if x == quote_sym => {
                    if self.first() == quote_sym {
                        self.bump();
                    } else {
                        return true;
                    }
                }
                _ => (),
            }
        }
        // End of file reached.
        false
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        while let '0'..='9' = self.first() {
            has_digits = true;
            self.bump();
        }
        has_digits
    }

    /// Eats the float exponent. Returns true if at least one digit was met,
    /// and returns false otherwise.
    fn eat_float_exponent(&mut self) -> bool {
        debug_assert!(self.prev() == 'e' || self.prev() == 'E');
        if self.first() == '-' || self.first() == '+' {
            self.bump();
        }
        self.eat_decimal_digits()
    }

    // Eats the suffix of the literal, e.g. "u8".
    fn eat_literal_suffix(&mut self) {
        self.eat_identifier();
    }

    // Eats the identifier. Note: succeeds on `_`, which isn't a valid
    // identifier.
    fn eat_identifier(&mut self) {
        if !is_id_start(self.first()) {
            return;
        }
        self.bump();

        self.eat_while(is_id_continue);
    }
}

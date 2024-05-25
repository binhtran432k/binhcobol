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
    /// At this step, keywords and invalid hyphen ending words are
    ///  also considered identifiers.
    Ident,

    /// Like the above, but containing invalid unicode codepoints.
    InvalidIdent,

    /// An unknown prefix, like `foo'`, `foo"`.
    ///
    /// Note that only the
    /// prefix (`foo`) is included in the token, not the separator (which is
    /// lexed as its own distinct token).
    UnknownPrefix,

    /// Examples: `"123"`, `"abc`, `'cde'`, `'23b`.
    Str { terminated: bool },

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
    matches!(c, |'a'..='z'| 'A'..='Z' | '0'..='9')
}

/// True if `c` is valid as a non-first character of an identifier.
pub fn is_id_continue(c: char) -> bool {
    c == '_' || c == '-' || is_id_start(c)
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
            '-' => TokenKind::Minus,
            '+' => TokenKind::Plus,

            // String literal.
            c if c == '\'' || c == '"' => {
                let terminated = self.eat_quoted_string(c);
                TokenKind::Str { terminated }
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
}

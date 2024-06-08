//! Lexing `&str` into a sequence of Cobol tokens.
//!
//! Note that strictly speaking the parser in this crate is not required to work
//! on tokens which originated from text. Macros, eg, can synthesize tokens out
//! of thin air. So, ideally, lexer should be an orthogonal crate. It is however
//! convenient to include a text-based lexer here!
//!
//! Note that these tokens, unlike the tokens we feed into the parser, do
//! include info about comments and whitespace.

use std::ops;

use crate::{
    SyntaxKind::{self, *},
    T,
};

pub struct LexedStr<'a> {
    text: &'a str,
    kind: Vec<SyntaxKind>,
    start: Vec<u32>,
    error: Vec<LexError>,
}

struct LexError {
    msg: String,
    token: u32,
}

impl<'a> LexedStr<'a> {
    pub fn new(text: &'a str) -> LexedStr<'a> {
        let _p = tracing::span!(tracing::Level::INFO, "LexedStr::new").entered();
        let mut conv = Converter::new(text);

        loop {
            let (remain_size, last_ws_size) = if let Some(size) = text[conv.offset..].find('\n') {
                if size > 0 {
                    let last_ws_len =
                        if &text[conv.offset..][size - 1..=size] == "\r\n" { 2 } else { 1 };
                    (size - last_ws_len + 1, last_ws_len)
                } else {
                    (0, 1)
                }
            } else if conv.offset < text.len() {
                (text.len() - conv.offset, 0)
            } else {
                break;
            };

            'end: {
                if remain_size == 0 {
                    break 'end;
                }
                let sequence_number_size = remain_size.min(6);
                conv.extend_token_by_size(SEQUENCE_NUMBER_AREA, sequence_number_size);
                let size = remain_size - sequence_number_size;
                if size == 0 {
                    break 'end;
                }

                let indicator_size =
                    conv.extend_indicator_token(&text[conv.offset..][..size.min(66)]);
                let remain_size = size - indicator_size;
                if remain_size == 0 {
                    break 'end;
                }

                let remain_size = if indicator_size == 1 {
                    let code_size = remain_size.min(65);
                    for token in cobol_lexer::tokenize(&text[conv.offset..][..code_size]) {
                        let token_text = &text[conv.offset..][..token.len as usize];

                        conv.extend_token(&token.kind, token_text);
                    }
                    let remain_size = remain_size - code_size;
                    if remain_size == 0 {
                        break 'end;
                    }
                    remain_size
                } else {
                    remain_size
                };

                let program_identification_size = remain_size.min(8);
                conv.extend_token_by_size(PROGRAM_IDENTIFICATION_AREA, program_identification_size);
                let remain_size = remain_size - program_identification_size;
                if remain_size == 0 {
                    break 'end;
                }

                conv.extend_token_by_size(INVALID_AREA, remain_size);
            }
            if last_ws_size > 0 {
                conv.extend_token_by_size(WHITESPACE, last_ws_size);
            }
        }

        conv.finalize_with_eof()
    }

    pub fn single_token(text: &'a str) -> Option<(SyntaxKind, Option<String>)> {
        if text.is_empty() {
            return None;
        }

        let token = cobol_lexer::tokenize(text).next()?;
        if token.len as usize != text.len() {
            return None;
        }

        let mut conv = Converter::new(text);
        conv.extend_token(&token.kind, text);
        match &*conv.res.kind {
            [kind] => Some((*kind, conv.res.error.pop().map(|it| it.msg))),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        self.text
    }

    pub fn len(&self) -> usize {
        self.kind.len() - 1
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn kind(&self, i: usize) -> SyntaxKind {
        assert!(i < self.len());
        self.kind[i]
    }

    pub fn text(&self, i: usize) -> &str {
        self.range_text(i..i + 1)
    }

    pub fn range_text(&self, r: ops::Range<usize>) -> &str {
        assert!(r.start < r.end && r.end <= self.len());
        let lo = self.start[r.start] as usize;
        let hi = self.start[r.end] as usize;
        &self.text[lo..hi]
    }

    // Naming is hard.
    pub fn text_range(&self, i: usize) -> ops::Range<usize> {
        assert!(i < self.len());
        let lo = self.start[i] as usize;
        let hi = self.start[i + 1] as usize;
        lo..hi
    }
    pub fn text_start(&self, i: usize) -> usize {
        assert!(i <= self.len());
        self.start[i] as usize
    }
    pub fn text_len(&self, i: usize) -> usize {
        assert!(i < self.len());
        let r = self.text_range(i);
        r.end - r.start
    }

    pub fn error(&self, i: usize) -> Option<&str> {
        assert!(i < self.len());
        let err = self.error.binary_search_by_key(&(i as u32), |i| i.token).ok()?;
        Some(self.error[err].msg.as_str())
    }

    pub fn errors(&self) -> impl Iterator<Item = (usize, &str)> + '_ {
        self.error.iter().map(|it| (it.token as usize, it.msg.as_str()))
    }

    fn push(&mut self, kind: SyntaxKind, offset: usize) {
        self.kind.push(kind);
        self.start.push(offset as u32);
    }
}

struct Converter<'a> {
    res: LexedStr<'a>,
    offset: usize,
}

impl<'a> Converter<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            res: LexedStr { text, kind: Vec::new(), start: Vec::new(), error: Vec::new() },
            offset: 0,
        }
    }

    fn finalize_with_eof(mut self) -> LexedStr<'a> {
        self.res.push(EOF, self.offset);
        self.res
    }

    fn push(&mut self, kind: SyntaxKind, len: usize, err: Option<&str>) {
        self.res.push(kind, self.offset);
        self.offset += len;

        if let Some(err) = err {
            let token = self.res.len() as u32;
            let msg = err.to_owned();
            self.res.error.push(LexError { msg, token });
        }
    }

    fn extend_indicator_token(&mut self, token_text: &str) -> usize {
        if let Some(text) = token_text.chars().next() {
            match text {
                '/' | '*' => {
                    self.extend_token_by_size(COMMENT_AREA, token_text.len());
                    token_text.len()
                }
                'd' | 'D' => {
                    self.extend_token_by_size(INDICATOR_AREA, 1);
                    self.extend_token_by_size(DEBUG_AREA, token_text.len() - 1);
                    token_text.len()
                }
                '-' => {
                    self.extend_token_by_size(INDICATOR_AREA, 1);
                    self.extend_token_by_size(CONTINUE_LINE_AREA, token_text.len() - 1);
                    token_text.len()
                }
                ' ' => {
                    self.extend_token_by_size(INDICATOR_AREA, 1);
                    1
                }
                _ => {
                    self.push(INDICATOR_AREA, 1, Some("unknown indicator token"));
                    1
                }
            }
        } else {
            0
        }
    }

    fn extend_token_by_size(&mut self, kind: SyntaxKind, size: usize) {
        self.push(kind, size, None);
    }

    fn extend_token(&mut self, kind: &cobol_lexer::TokenKind, token_text: &str) {
        // A note on an intended tradeoff:
        // We drop some useful information here (see patterns with double dots `..`)
        // Storing that info in `SyntaxKind` is not possible due to its layout requirements of
        // being `u16` that come from `rowan::SyntaxKind`.
        let mut err = "";

        let syntax_kind = {
            match kind {
                cobol_lexer::TokenKind::InlineComment => COMMENT,

                cobol_lexer::TokenKind::Whitespace => WHITESPACE,

                cobol_lexer::TokenKind::Ident => {
                    SyntaxKind::from_keyword(token_text).unwrap_or(IDENT)
                }
                cobol_lexer::TokenKind::NumberIdent => NUMBER_IDENT,
                cobol_lexer::TokenKind::InvalidIdent => {
                    err = "Ident contains invalid characters";
                    IDENT
                }

                cobol_lexer::TokenKind::Literal { kind, .. } => {
                    self.extend_literal(token_text.len(), kind);
                    return;
                }

                cobol_lexer::TokenKind::Semi => T![;],
                cobol_lexer::TokenKind::Comma => T![,],
                cobol_lexer::TokenKind::Dot => T![.],
                cobol_lexer::TokenKind::OpenParen => T!['('],
                cobol_lexer::TokenKind::CloseParen => T![')'],
                cobol_lexer::TokenKind::Colon => T![:],
                cobol_lexer::TokenKind::Dollar => T![$],
                cobol_lexer::TokenKind::Eq => T![=],
                cobol_lexer::TokenKind::Lt => T![<],
                cobol_lexer::TokenKind::Gt => T![>],
                cobol_lexer::TokenKind::Minus => T![-],
                cobol_lexer::TokenKind::Plus => T![+],
                cobol_lexer::TokenKind::Star => T![*],
                cobol_lexer::TokenKind::Slash => T![/],
                cobol_lexer::TokenKind::Unknown => ERROR,
                cobol_lexer::TokenKind::UnknownPrefix if token_text == "builtin" => IDENT,
                cobol_lexer::TokenKind::UnknownPrefix => {
                    err = "unknown literal prefix";
                    IDENT
                }
                cobol_lexer::TokenKind::Eof => EOF,
            }
        };

        let err = if err.is_empty() { None } else { Some(err) };
        self.push(syntax_kind, token_text.len(), err);
    }

    fn extend_literal(&mut self, len: usize, kind: &cobol_lexer::LiteralKind) {
        let mut err = "";

        let syntax_kind = match *kind {
            cobol_lexer::LiteralKind::Int => INT_NUMBER,
            cobol_lexer::LiteralKind::Float { empty_exponent } => {
                if empty_exponent {
                    err = "Missing digits after the exponent symbol";
                }
                FLOAT_NUMBER
            }
            cobol_lexer::LiteralKind::Str { terminated } => {
                if !terminated {
                    err = "Missing trailing `\"` symbol to terminate the string literal";
                }
                STRING
            }
            _ => {
                err = "Unsupported Literal";
                IDENT
            }
        };

        let err = if err.is_empty() { None } else { Some(err) };
        self.push(syntax_kind, len, err);
    }
}

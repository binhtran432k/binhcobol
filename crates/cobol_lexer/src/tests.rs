use crate::tokenize;

use expect_test::{expect, Expect};
use std::fmt::Write;

fn check_lexing(src: &str, expect: Expect) {
    let actual: String = tokenize(src).fold(String::new(), |mut out, token| {
        let _ = writeln!(out, "{:?}", token);
        out
    });
    expect.assert_eq(&actual)
}

#[test]
fn smoke_test() {
    check_lexing(
        r#"
       *>-----------------------
       *> Copyright Contributors to the BinhCOBOL
       *>-----------------------
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           GOBACK.
        "#,
        expect![[r#"
            Token { kind: Whitespace, len: 8 }
            Token { kind: InlineComment, len: 25 }
            Token { kind: Whitespace, len: 8 }
            Token { kind: InlineComment, len: 42 }
            Token { kind: Whitespace, len: 8 }
            Token { kind: InlineComment, len: 25 }
            Token { kind: Whitespace, len: 8 }
            Token { kind: Ident, len: 14 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 8 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 8 }
            Token { kind: Ident, len: 10 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 5 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 8 }
            Token { kind: Ident, len: 9 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Ident, len: 8 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 12 }
            Token { kind: Ident, len: 7 }
            Token { kind: Whitespace, len: 1 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 15 }, len: 15 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 12 }
            Token { kind: Ident, len: 6 }
            Token { kind: Dot, len: 1 }
            Token { kind: Whitespace, len: 9 }
        "#]],
    )
}

#[test]
fn strs() {
    check_lexing(
        r#"
        "abc"
        "abc""xyz"
        "abc'xyz"
        "abc
        "abc""xyz"
        "abc'xyz"
        'abc'
        'abc''xyz'
        'abc"xyz'
        'abc
        "Có 10 con gà nói `Xin chào`"
        "#,
        expect![[r#"
            Token { kind: Whitespace, len: 9 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 5 }, len: 5 }
            Token { kind: Whitespace, len: 9 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 10 }, len: 10 }
            Token { kind: Whitespace, len: 9 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 9 }, len: 9 }
            Token { kind: Whitespace, len: 9 }
            Token { kind: Literal { kind: Str { terminated: false }, suffix_start: 5 }, len: 5 }
            Token { kind: Whitespace, len: 8 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 10 }, len: 10 }
            Token { kind: Whitespace, len: 9 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 9 }, len: 9 }
            Token { kind: Whitespace, len: 9 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 5 }, len: 5 }
            Token { kind: Whitespace, len: 9 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 10 }, len: 10 }
            Token { kind: Whitespace, len: 9 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 9 }, len: 9 }
            Token { kind: Whitespace, len: 9 }
            Token { kind: Literal { kind: Str { terminated: false }, suffix_start: 5 }, len: 5 }
            Token { kind: Whitespace, len: 8 }
            Token { kind: Literal { kind: Str { terminated: true }, suffix_start: 33 }, len: 33 }
            Token { kind: Whitespace, len: 9 }
        "#]],
    )
}

#[test]
fn idents_or_numbers() {
    check_lexing(r#"
        123
        123e10
        abc
        abc123
        abc123a
    "#, expect![[r#"
        Token { kind: Whitespace, len: 9 }
        Token { kind: NumberIdent, len: 3 }
        Token { kind: Whitespace, len: 9 }
        Token { kind: Ident, len: 6 }
        Token { kind: Whitespace, len: 9 }
        Token { kind: Ident, len: 3 }
        Token { kind: Whitespace, len: 9 }
        Token { kind: Ident, len: 6 }
        Token { kind: Whitespace, len: 9 }
        Token { kind: Ident, len: 7 }
        Token { kind: Whitespace, len: 5 }
    "#]])
}

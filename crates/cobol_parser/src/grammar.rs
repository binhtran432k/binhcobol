//! This is the actual "grammar" of the Cobol language.
//!
//! Each function in this module and its children corresponds
//! to a production of the formal grammar. Submodules roughly
//! correspond to different *areas* of the grammar. By convention,
//! each submodule starts with `use super::*` import and exports
//! "public" productions via `pub(super)`.
//!
//! See docs for [`Parser`](super::parser::Parser) to learn about API,
//! available to the grammar, and see docs for [`Event`](super::event::Event)
//! to learn how this actually manages to produce parse trees.
//!
//! Code in this module also contains inline tests, which start with
//! `// test name-of-the-test` comment and look like this:
//!
//! ```
//! // test function_with_zero_parameters
//! // fn foo() {}
//! ```
//!
//! After adding a new inline-test, run `cargo test -p xtask` to
//! extract it as a standalone text-fixture into
//! `crates/syntax/test_data/parser/`, and run `cargo test` once to
//! create the "gold" value.
//!
//! Coding convention: rules like `where_clause` always produce either a
//! node or an error, rules like `opt_where_clause` may produce nothing.
//! Non-opt rules typically start with `assert!(p.at(FIRST_TOKEN))`, the
//! caller is responsible for branching on the first token.

mod items;

use crate::{
    parser::Parser,
    SyntaxKind::{self, *},
};

pub(crate) mod entry {
    use super::*;

    pub(crate) mod top {
        use super::*;

        pub(crate) fn source_file(p: &mut Parser<'_>) {
            let m = p.start();
            m.complete(p, SOURCE_FILE);
        }
    }
}

pub(crate) fn reparser(
    _node: SyntaxKind,
    _first_child: Option<SyntaxKind>,
    _parent: Option<SyntaxKind>,
) -> Option<fn(&mut Parser<'_>)> {
    None
}
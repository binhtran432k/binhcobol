use proc_macro2::{Punct, Spacing};
use quote::{format_ident, quote};
use sourcegen::{add_preamble, ensure_file_contents, project_root, reformat};

use super::ast_src::{KindsSrc, KINDS_SRC};

pub fn generate() {
    let syntax_kinds = generate_syntax_kinds(KINDS_SRC);
    let syntax_kinds_file = project_root().join("crates/cobol_parser/src/syntax_kind/generated.rs");
    ensure_file_contents(syntax_kinds_file.as_path(), &syntax_kinds);
}

fn generate_syntax_kinds(grammar: KindsSrc<'_>) -> String {
    let (single_byte_tokens_values, single_byte_tokens): (Vec<_>, Vec<_>) = grammar
        .punct
        .iter()
        .filter(|(token, _name)| token.len() == 1)
        .map(|(token, name)| (token.chars().next().unwrap(), format_ident!("{}", name)))
        .unzip();

    let punctuation_values = grammar.punct.iter().map(|(token, _name)| {
        if "{}[]()".contains(token) {
            let c = token.chars().next().unwrap();
            quote! { #c }
        } else {
            let cs = token.chars().map(|c| Punct::new(c, Spacing::Joint));
            quote! { #(#cs)* }
        }
    });
    let punctuation =
        grammar.punct.iter().map(|(_token, name)| format_ident!("{}", name)).collect::<Vec<_>>();

    let x = |&name| format_ident!("{}_KW", to_upper_snake_case_from_kebab_case(name));

    let reserved_words_values = grammar.reserved_words;
    let standard_only_words_values = grammar.standard_only_words;
    let standard_only_words = standard_only_words_values.iter().map(x);
    let potential_reserved_words_values = grammar.potential_reserved_words;
    let potential_reserved_words = potential_reserved_words_values.iter().map(x);

    let full_keywords_values = reserved_words_values
        .iter()
        .chain(standard_only_words_values.iter())
        .chain(potential_reserved_words_values.iter())
        .copied()
        .collect::<Vec<_>>();
    let full_keywords = full_keywords_values.iter().map(x);

    let contextual_keywords_values = &grammar.contextual_keywords;
    let contextual_keywords = contextual_keywords_values.iter().map(x);

    let all_keywords_values = full_keywords_values
        .iter()
        .chain(grammar.contextual_keywords.iter())
        .copied()
        .collect::<Vec<_>>();
    let all_keywords_idents = all_keywords_values.iter().map(|kw| {
        if kw.contains('-') {
            quote! { #kw }
        } else {
            let ident = format_ident!("{}", kw);
            quote! { #ident }
        }
    });
    let all_keywords = all_keywords_values.iter().map(x).collect::<Vec<_>>();

    let literals =
        grammar.literals.iter().map(|name| format_ident!("{}", name)).collect::<Vec<_>>();

    let tokens = grammar.tokens.iter().map(|name| format_ident!("{}", name)).collect::<Vec<_>>();

    let nodes = grammar.nodes.iter().map(|name| format_ident!("{}", name)).collect::<Vec<_>>();

    let ast = quote! {
        #![allow(bad_style, missing_docs, unreachable_pub)]
        /// The kind of syntax node, e.g. `IDENT`, `USE_KW`, or `STRUCT`.
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        #[repr(u16)]
        pub enum SyntaxKind {
            // Technical SyntaxKinds: they appear temporally during parsing,
            // but never end up in the final tree
            #[doc(hidden)]
            TOMBSTONE,
            #[doc(hidden)]
            EOF,
            #(#punctuation,)*
            #(#all_keywords,)*
            #(#literals,)*
            #(#tokens,)*
            #(#nodes,)*

            // Technical kind so that we can cast from u16 safely
            #[doc(hidden)]
            __LAST,
        }
        use self::SyntaxKind::*;

        impl SyntaxKind {
            pub fn is_keyword(self) -> bool {
                matches!(self, #(#all_keywords)|*)
            }

            pub fn is_standard_only_word(self) -> bool {
                matches!(self, #(#standard_only_words)|*)
            }

            pub fn is_potential_reserved_word(self) -> bool {
                matches!(self, #(#potential_reserved_words)|*)
            }

            pub fn is_punct(self) -> bool {
                matches!(self, #(#punctuation)|*)
            }

            pub fn is_literal(self) -> bool {
                matches!(self, #(#literals)|*)
            }

            pub fn from_keyword(ident: &str) -> Option<SyntaxKind> {
                let ident = ident.to_ascii_uppercase();
                let kw = match ident.as_str() {
                    #(#full_keywords_values => #full_keywords,)*
                    _ => return None,
                };
                Some(kw)
            }

            pub fn from_contextual_keyword(ident: &str) -> Option<SyntaxKind> {
                let ident = ident.to_ascii_uppercase();
                let kw = match ident.as_str() {
                    #(#contextual_keywords_values => #contextual_keywords,)*
                    _ => return None,
                };
                Some(kw)
            }

            pub fn from_char(c: char) -> Option<SyntaxKind> {
                let tok = match c {
                    #(#single_byte_tokens_values => #single_byte_tokens,)*
                    _ => return None,
                };
                Some(tok)
            }
        }

        #[macro_export]
        macro_rules! T {
            #([#punctuation_values] => { $crate::SyntaxKind::#punctuation };)*
            #([#all_keywords_idents] => { $crate::SyntaxKind::#all_keywords };)*
            [lifetime_ident] => { $crate::SyntaxKind::LIFETIME_IDENT };
            [ident] => { $crate::SyntaxKind::IDENT };
            [shebang] => { $crate::SyntaxKind::SHEBANG };
        }
    };

    add_preamble("sourcegen_ast", reformat(ast.to_string()))
}

fn to_upper_snake_case_from_kebab_case(s: &str) -> String {
    s.chars().map(|c| if c == '-' { '_' } else { c.to_ascii_uppercase() }).collect()
}

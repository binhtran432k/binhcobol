//! Missing batteries for standard libraries.

pub mod panic_context;

#[inline(always)]
pub fn is_ci() -> bool {
    option_env!("CI").is_some()
}

pub fn hash_once<Hasher: std::hash::Hasher + Default>(thing: impl std::hash::Hash) -> u64 {
    std::hash::BuildHasher::hash_one(&std::hash::BuildHasherDefault::<Hasher>::default(), thing)
}

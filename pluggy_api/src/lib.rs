use std::{fmt::Debug, hash::Hash};

mod hir_def;
pub mod lint;
mod span;
mod utils;

pub use hir_def::*;
pub use smartstring::validate;
use smartstring::{LazyCompact, SmartString};
pub use span::{
    DefId, DefIndex, HirId, Ident, ItemId, ItemLocalId, LocalDefId, Span, SpanData,
    Spanned, SyntaxContext, CRATE_DEF_INDEX, DUMMY_SP,
};

pub type Symbol = SmartString<LazyCompact>;

pub trait Context {
    fn def_path_str(&self, id: DefId) -> String;
    fn name(&self) -> &'static str;
    fn warn(&self, s: &str, lint: lint::Lint);
}

/// Represents some newtyped `usize` wrapper.
///
/// Purpose: avoid mixing indexes for different bitvector domains.
pub trait Idx: Copy + 'static + Ord + Debug + Hash {
    fn new(idx: usize) -> Self;

    fn index(self) -> usize;

    fn increment_by(&mut self, amount: usize) { *self = self.plus(amount); }

    fn plus(self, amount: usize) -> Self { Self::new(self.index() + amount) }
}

impl Idx for usize {
    #[inline]
    fn new(idx: usize) -> Self { idx }
    #[inline]
    fn index(self) -> usize { self }
}

impl Idx for u32 {
    #[inline]
    fn new(idx: usize) -> Self {
        assert!(idx <= u32::MAX as usize);
        idx as u32
    }
    #[inline]
    fn index(self) -> usize { self as usize }
}

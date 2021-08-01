mod hir_def;
pub mod lint;
mod span;
mod utils;

use std::{cell::RefCell, fmt::Debug, hash::Hash};

pub use hir_def::*;
pub use span::{
    DefId, DefIndex, HirId, Ident, Interner, ItemId, ItemLocalId, LocalDefId, Span,
    SpanData, Spanned, Symbol, SymbolStr, SyntaxContext, CRATE_DEF_INDEX, DUMMY_SP,
};

thread_local! {
    pub(crate) static INTERN: RefCell<Interner> = RefCell::new(Interner::generate());
}

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

use std::{fmt::Debug, hash::Hash};

pub use smartstring::validate;
use smartstring::{LazyCompact, SmartString};

mod hir_def;
pub mod lint;
mod span;
mod utils;

pub use hir_def::*;
pub use span::{
    BodyId, DefId, DefIndex, HirId, Ident, ItemId, ItemLocalId, LocalDefId, Span,
    SpanData, Spanned, SyntaxContext, CRATE_DEF_INDEX, DUMMY_SP,
};
pub use utils::{Abi, LangItem};

pub type Symbol = SmartString<LazyCompact>;

pub trait Context {
    fn def_path_str(&self, id: DefId) -> String;
    fn warn(&self, s: &str, lint: &lint::Lint);
    fn warn_span(&self, s: &str, lint: &lint::Lint, sp: Span);
}

pub trait LintMarker {}

pub trait LateLintImpl: LintMarker {
    fn check_item(&mut self, _cx: &dyn Context, _item: Item) {}
    fn check_ty(&mut self, _cx: &dyn Context, _ty: Ty) {}
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

/// Declares a static `LintArray` and return it as an expression.
#[macro_export]
macro_rules! lint_array {
    ($( $lint:expr ),* ,) => { lint_array!( $($lint),* ) };
    ($( $lint:expr ),*) => {{
        vec![$($lint),*]
    }}
}

pub type LintArray = Vec<&'static lint::Lint>;

/// Implements `LintPass for $ty` with the given list of `Lint` statics.
#[macro_export]
macro_rules! impl_lint_pass {
    ($ty:ident => [$($lint:expr),* $(,)?])=> {
        impl $crate::LintMarker for $ty {}
        #[no_mangle]
        pub fn get_lint_impl() -> *mut Box<$crate::LateLintImpl>{
            Box::into_raw(Box::new(Box::new($ty)))
        }
        #[no_mangle]
        pub fn get_lints() -> $crate::LintArray {
            vec![$($lint),*]
        }
    };
    ($ty:ident::$init:ident => [$($lint:expr),* $(,)?])=> {
        impl $crate::LintMarker for $ty {}
        #[no_mangle]
        pub fn get_lint_impl() -> *mut Box<$crate::LateLintImpl> {
            Box::into_raw(Box::new(Box::new($ty::$init())))
        }
        #[no_mangle]
        pub fn get_lints() -> $crate::LintArray {
            vec![$($lint),*]
        }
    };
}

/// Declares a type named `$name` which implements `LintPass`.
/// To the right of `=>` a comma separated list of `Lint` statics is given.
#[macro_export]
macro_rules! declare_lint_pass {
    ($(#[$m:meta])* $name:ident => [$($lint:expr),* $(,)?]) => {
        $(#[$m])* #[derive(Copy, Clone)] pub struct $name;
        $crate::impl_lint_pass!($name => [$($lint),*]);
    };
}

#[macro_export]
macro_rules! declare_tool_lint {
    (
        $(#[$attr:meta])* $vis:vis $NAME:ident, $lvl:ident, $desc:expr
    ) => (
        $crate::declare_tool_lint!{$(#[$attr])* $vis $NAME, $lvl, $desc, false}
    );
    (
        $(#[$attr:meta])* $vis:vis $NAME:ident, $lvl:ident, $desc:expr,
        report_in_external_macro: $rep:expr
    ) => (
         $crate::declare_tool_lint!{$(#[$attr])* $vis $tool::$NAME, $lvl, $desc, $rep}
    );
    (
        $(#[$attr:meta])* $vis:vis $NAME:ident, $lvl:ident, $desc:expr,
        $external:expr
    ) => (
        $(#[$attr])*
        $vis static $NAME: &$crate::lint::Lint = &$crate::lint::Lint {
            name: &concat!("pluggy::", stringify!($NAME)),
            default_level: $crate::lint::Level::$lvl,
            desc: $desc,
            edition_lint_opts: None,
            report_in_external_macro: $external,
            future_incompatible: None,
            is_plugin: true,
            feature_gate: None,
            crate_level_only: false,
        };
    );
}

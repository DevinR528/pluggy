use std::cmp;

use crate::Symbol;

/// Dummy span, both position and length are zero, syntax context is zero as well.
pub const DUMMY_SP: Span = Span { base_or_index: 0, len_or_tag: 0, ctxt_or_zero: 0 };

const LEN_TAG: u16 = 0b1000_0000_0000_0000;
const MAX_LEN: u32 = 0b0111_1111_1111_1111;
const MAX_CTXT: u32 = 0b1111_1111_1111_1111;

pub type BytePos = u32;

/// A `SyntaxContext` represents a chain of pairs `(ExpnId, Transparency)` named "marks".
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SyntaxContext(pub u32);

impl SyntaxContext {
    #[inline]
    pub const fn root() -> Self { SyntaxContext(0) }

    #[inline]
    pub fn as_u32(self) -> u32 { self.0 }

    #[inline]
    pub fn from_u32(raw: u32) -> SyntaxContext { SyntaxContext(raw) }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Span {
    base_or_index: u32,
    len_or_tag: u16,
    ctxt_or_zero: u16,
}

/// Represents a span.
///
/// Spans represent a region of code, used for error reporting. Positions in spans
/// are *absolute* positions from the beginning of the [`SourceMap`], not positions
/// relative to [`SourceFile`]s. Methods on the `SourceMap` can be used to relate spans
/// back to the original source.
///
/// You must be careful if the span crosses more than one file, since you will not be
/// able to use many of the functions on spans in source_map and you cannot assume
/// that the length of the span is equal to `span.hi - span.lo`; there may be space in the
/// [`BytePos`] range between files.
///
/// `SpanData` is public because `Span` uses a thread-local interner and can't be
/// sent to other threads, but some pieces of performance infra run in a separate thread.
/// Using `Span` is generally preferred.
#[derive(Clone, Copy, Hash, PartialEq, Eq, Ord, PartialOrd)]
pub struct SpanData {
    pub lo: BytePos,
    pub hi: BytePos,
    /// Information about where the macro came from, if this piece of
    /// code was created by a macro expansion.
    pub ctxt: SyntaxContext,
}

impl Span {
    #[inline]
    pub fn new(mut lo: BytePos, mut hi: BytePos, ctxt: SyntaxContext) -> Self {
        if lo > hi {
            std::mem::swap(&mut lo, &mut hi);
        }

        let (base, len, ctxt2) = (lo, hi - lo, ctxt);

        if len <= MAX_LEN && ctxt2.0 <= MAX_CTXT {
            // Inline format.
            Span {
                base_or_index: base,
                len_or_tag: len as u16,
                ctxt_or_zero: ctxt2.0 as u16,
            }
        } else {
            // Interned format.
            // let index = with_span_interner(|interner| interner.intern(&SpanData { lo,
            // hi, ctxt })); Span { base_or_index: index, len_or_tag: LEN_TAG,
            // ctxt_or_zero: 0 }
            panic!()
        }
    }

    #[inline]
    pub fn data(self) -> SpanData {
        if self.len_or_tag != LEN_TAG {
            // Inline format.
            debug_assert!(self.len_or_tag as u32 <= MAX_LEN);
            SpanData {
                lo: self.base_or_index,
                hi: self.base_or_index + self.len_or_tag as u32,
                ctxt: SyntaxContext::from_u32(self.ctxt_or_zero as u32),
            }
        } else {
            // Interned format.
            // debug_assert!(self.ctxt_or_zero == 0);
            // let index = self.base_or_index;
            // with_span_interner(|interner| interner.spans[index as usize])
            panic!()
        }
    }
    #[inline]
    pub fn lo(self) -> BytePos { self.data().lo }
    #[inline]
    pub fn with_lo(self, lo: BytePos) -> Span { self.data().with_lo(lo) }
    #[inline]
    pub fn hi(self) -> BytePos { self.data().hi }
    #[inline]
    pub fn with_hi(self, hi: BytePos) -> Span { self.data().with_hi(hi) }
    #[inline]
    pub fn ctxt(self) -> SyntaxContext { self.data().ctxt }
    /// Returns `true` if this is a dummy span with any hygienic context.
    #[inline]
    pub fn is_dummy(self) -> bool {
        let span = self.data();
        span.lo == 0 && span.hi == 0
    }
    #[inline]
    /// Returns `true` if `hi == lo`.
    pub fn is_empty(&self) -> bool {
        let span = self.data();
        span.hi == span.lo
    }
    /// Returns a `Span` that would enclose both `self` and `end`.
    ///
    /// ```text
    ///     ____             ___
    ///     self lorem ipsum end
    ///     ^^^^^^^^^^^^^^^^^^^^
    /// ```
    pub fn to(self, end: Span) -> Span {
        let span_data = self.data();
        let end_data = end.data();
        // FIXME(jseyfried): `self.ctxt` should always equal `end.ctxt` here (cf. issue
        // #23480). Return the macro span on its own to avoid weird diagnostic
        // output. It is preferable to have an incomplete span than a completely
        // nonsensical one.
        if span_data.ctxt != end_data.ctxt {
            if span_data.ctxt == SyntaxContext::root() {
                return end;
            } else if end_data.ctxt == SyntaxContext::root() {
                return self;
            }
            // Both spans fall within a macro.
            // FIXME(estebank): check if it is the *same* macro.
        }
        Span::new(
            cmp::min(span_data.lo, end_data.lo),
            cmp::max(span_data.hi, end_data.hi),
            if span_data.ctxt == SyntaxContext::root() {
                end_data.ctxt
            } else {
                span_data.ctxt
            },
        )
    }

    /// Returns a `Span` between the end of `self` to the beginning of `end`.
    ///
    /// ```text
    ///     ____             ___
    ///     self lorem ipsum end
    ///         ^^^^^^^^^^^^^
    /// ```
    pub fn between(self, end: Span) -> Span {
        let span = self.data();
        let end = end.data();
        Span::new(
            span.hi,
            end.lo,
            if end.ctxt == SyntaxContext::root() { end.ctxt } else { span.ctxt },
        )
    }

    /// Returns a `Span` from the beginning of `self` until the beginning of `end`.
    ///
    /// ```text
    ///     ____             ___
    ///     self lorem ipsum end
    ///     ^^^^^^^^^^^^^^^^^
    /// ```
    pub fn until(self, end: Span) -> Span {
        let span = self.data();
        let end = end.data();
        Span::new(
            span.lo,
            end.lo,
            if end.ctxt == SyntaxContext::root() { end.ctxt } else { span.ctxt },
        )
    }
}

impl SpanData {
    #[inline]
    pub fn span(&self) -> Span { Span::new(self.lo, self.hi, self.ctxt) }
    #[inline]
    pub fn with_lo(&self, lo: BytePos) -> Span { Span::new(lo, self.hi, self.ctxt) }
    #[inline]
    pub fn with_hi(&self, hi: BytePos) -> Span { Span::new(self.lo, hi, self.ctxt) }
    #[inline]
    pub fn with_ctxt(&self, ctxt: SyntaxContext) -> Span {
        Span::new(self.lo, self.hi, ctxt)
    }
}

#[derive(Clone, Debug, Copy, Hash)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, node: T) -> Self { Self { node, span } }
}

crate::newtype_index! {
    /// A DefIndex is an index into the hir-map for a crate, identifying a
    /// particular definition. It should really be considered an interned
    /// shorthand for a particular DefPath.
    pub struct DefIndex {
        DEBUG_FORMAT = "DefIndex({})",
        /// The crate root is always assigned index 0 by the AST Map code,
        /// thanks to `NodeCollector::new`.
        const CRATE_DEF_INDEX = 0,
    }
}

#[derive(Clone, PartialEq, Debug, Eq, PartialOrd, Ord, Hash, Copy)]
pub struct DefId {
    pub krate: u32,
    pub index: DefIndex,
}

/// A LocalDefId is equivalent to a DefId with `krate == LOCAL_CRATE`. Since
/// we encode this information in the type, we can ensure at compile time that
/// no DefIds from upstream crates get thrown into the mix. There are quite a
/// few cases where we know that only DefIds from the local crate are expected
/// and a DefId from a different crate would signify a bug somewhere. This
/// is when LocalDefId comes in handy.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LocalDefId {
    pub local_def_index: DefIndex,
}

impl LocalDefId {
    pub fn from_u32(raw: u32) -> Self {
        Self { local_def_index: DefIndex::from_u32(raw) }
    }
}

crate::newtype_index! {
    /// An `ItemLocalId` uniquely identifies something within a given "item-like";
    /// that is, within a `hir::Item`, `hir::TraitItem`, or `hir::ImplItem`. There is no
    /// guarantee that the numerical value of a given `ItemLocalId` corresponds to
    /// the node's position within the owning item in any way, but there is a
    /// guarantee that the `LocalItemId`s within an owner occupy a dense range of
    /// integers starting at zero, so a mapping that maps all or most nodes within
    /// an "item-like" to something else can be implemented by a `Vec` instead of a
    /// tree or hash map.
    pub struct ItemLocalId { .. }
}

/// Uniquely identifies a node in the HIR of the current crate. It is
/// composed of the `owner`, which is the `LocalDefId` of the directly enclosing
/// `hir::Item`, `hir::TraitItem`, or `hir::ImplItem` (i.e., the closest "item-like"),
/// and the `local_id` which is unique within the given owner.
///
/// This two-level structure makes for more stable values: One can move an item
/// around within the source code, or add or remove stuff before it, without
/// the `local_id` part of the `HirId` changing, which is a very useful property in
/// incremental compilation where we have to persist things through changes to
/// the code base.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub struct HirId {
    pub owner: LocalDefId,
    pub local_id: ItemLocalId,
}

impl HirId {
    pub fn from_raw(owner: u32, local_id: u32) -> Self {
        Self {
            owner: LocalDefId::from_u32(owner),
            local_id: ItemLocalId::from_u32(local_id),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BodyId {
    pub hir_id: HirId,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct ItemId {
    pub def_id: LocalDefId,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    #[inline]
    /// Constructs a new identifier from a symbol and a span.
    pub fn new(name: &str, span: Span) -> Ident {
        Ident { name: Symbol::from(name), span }
    }

    /// Constructs a new identifier with a dummy span.
    #[inline]
    pub fn with_dummy_span(name: &str) -> Ident { Ident::new(name, DUMMY_SP) }

    #[inline]
    pub fn invalid() -> Ident { Ident::with_dummy_span("") }

    /// Maps a string and a span to an identifier.
    pub fn from_str_and_span(string: &str, span: Span) -> Ident {
        Ident::new(string, span)
    }
}

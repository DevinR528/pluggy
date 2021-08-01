use std::{cmp, fmt};

use fxhash::FxHashMap;
use typed_arena::Arena;

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

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    #[inline]
    /// Constructs a new identifier from a symbol and a span.
    pub const fn new(name: Symbol, span: Span) -> Ident { Ident { name, span } }

    /// Constructs a new identifier with a dummy span.
    #[inline]
    pub const fn with_dummy_span(name: Symbol) -> Ident { Ident::new(name, DUMMY_SP) }

    #[inline]
    pub fn invalid() -> Ident { Ident::with_dummy_span(Symbol::intern("")) }

    /// Maps a string to an identifier with a dummy span.
    pub fn from_str(string: &str) -> Ident {
        Ident::with_dummy_span(Symbol::intern(string))
    }

    /// Maps a string and a span to an identifier.
    pub fn from_str_and_span(string: &str, span: Span) -> Ident {
        Ident::new(Symbol::intern(string), span)
    }
}

crate::newtype_index! {
    pub struct SymbolIndex { .. }
}

/// An interned string.
///
/// Internally, a `Symbol` is implemented as an index, and all operations
/// (including hashing, equality, and ordering) operate on that index. The use
/// of `rustc_index::newtype_index!` means that `Option<Symbol>` only takes up 4 bytes,
/// because `rustc_index::newtype_index!` reserves the last 256 values for tagging
/// purposes.
///
/// Note that `Symbol` cannot directly be a `rustc_index::newtype_index!` because it
/// implements `fmt::Debug`, `Encodable`, and `Decodable` in special ways.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(SymbolIndex);

impl Symbol {
    const fn new(id: u32) -> Self { Self(SymbolIndex::from_u32(id)) }

    pub fn intern(s: &str) -> Self { with_interner(|intern| intern.intern(s)) }

    /// Convert to a `SymbolStr`. This is a slowish operation because it
    /// requires locking the symbol interner.
    pub fn as_str(self) -> SymbolStr {
        with_interner(|interner| unsafe {
            SymbolStr { string: std::mem::transmute::<&str, &str>(interner.get(self)) }
        })
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.as_str(), f)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.as_str(), f)
    }
}

/// An alternative to [`Symbol`], useful when the chars within the symbol need to
/// be accessed. It deliberately has limited functionality and should only be
/// used for temporary values.
///
/// Because the interner outlives any thread which uses this type, we can
/// safely treat `string` which points to interner data, as an immortal string,
/// as long as this type never crosses between threads.
//
// FIXME: ensure that the interner outlives any thread which uses `SymbolStr`,
// by creating a new thread right after constructing the interner.
#[derive(Clone, Eq, PartialOrd, Ord)]
pub struct SymbolStr {
    string: &'static str,
}

// This impl allows a `SymbolStr` to be directly equated with a `String` or
// `&str`.
impl<T: std::ops::Deref<Target = str>> std::cmp::PartialEq<T> for SymbolStr {
    fn eq(&self, other: &T) -> bool { self.string == other.deref() }
}

/// This impl means that if `ss` is a `SymbolStr`:
/// - `*ss` is a `str`;
/// - `&*ss` is a `&str` (and `match &*ss { ... }` is a common pattern).
/// - `&ss as &str` is a `&str`, which means that `&ss` can be passed to a function
///   expecting a `&str`.
impl std::ops::Deref for SymbolStr {
    type Target = str;
    #[inline]
    fn deref(&self) -> &str { self.string }
}

impl fmt::Debug for SymbolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.string, f)
    }
}

impl fmt::Display for SymbolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.string, f)
    }
}

// The `&'static str`s in this type actually point into the arena.
//
// The `FxHashMap`+`Vec` pair could be replaced by `FxIndexSet`, but #75278
// found that to regress performance up to 2% in some cases. This might be
// revisited after further improvements to `indexmap`.
#[derive(Default)]
pub struct Interner {
    arena: Arena<u8>,
    names: FxHashMap<&'static str, Symbol>,
    strings: Vec<&'static str>,
}

impl Interner {
    fn prefill(init: &[&'static str]) -> Self {
        Interner {
            strings: init.into(),
            names: init.iter().copied().zip((0..).map(Symbol::new)).collect(),
            ..Default::default()
        }
    }

    #[inline]
    pub fn intern(&mut self, string: &str) -> Symbol {
        if let Some(&name) = self.names.get(string) {
            return name;
        }

        let name = Symbol::new(self.strings.len() as u32);

        let string = self.arena.alloc_str(string);
        // It is safe to extend the arena allocation to `'static` because we only access
        // these while the arena is still alive.
        let string: &'static str = unsafe { &*(string as *const str) };
        self.strings.push(string);
        self.names.insert(string, name);
        name
    }

    // Get the symbol as a string. `Symbol::as_str()` should be used in
    // preference to this function.
    pub fn get(&self, symbol: Symbol) -> &str {
        self.strings.get(symbol.0.as_usize()).unwrap_or(&"")
    }

    pub(crate) fn generate() -> Self {
        Self::prefill(&[
            "", "{{root}}", "$crate", "_", "as", "break", "const", "continue", "crate",
            "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in", "let",
            "loop", "match", "mod", "move", "mut", "pub", "ref", "return", "self",
            "Self", "static", "struct", "super", "trait", "true", "type", "unsafe",
            "use", "where", "while",
        ])
    }
}

#[inline]
fn with_interner<T, F: FnOnce(&mut Interner) -> T>(f: F) -> T {
    crate::INTERN.with(|locked| f(&mut *locked.borrow_mut()))
}

use crate::{
    span::{BodyId, DefId, HirId, Ident, ItemId, LocalDefId, Span, Spanned, DUMMY_SP},
    utils::{Abi, LangItem},
    Context, Symbol,
};

#[derive(Clone, Debug, Hash)]
pub struct Lifetime {
    pub hir_id: HirId,
    pub span: Span,

    /// Either "`'a`", referring to a named lifetime definition,
    /// or "``" (i.e., `kw::Empty`), for elision placeholders.
    ///
    /// HIR lowering inserts these placeholders in type paths that
    /// refer to type definitions needing lifetime parameters,
    /// `&T` and `&mut T`, and trait objects without `... + 'a`.
    pub name: LifetimeName,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ParamName {
    /// Some user-given name like `T` or `'x`.
    Plain(Ident),

    /// Synthetic name generated when user elided a lifetime in an impl header.
    ///
    /// E.g., the lifetimes in cases like these:
    ///
    ///     impl Foo for &u32
    ///     impl Foo for u32
    ///
    /// in that case, we rewrite to
    ///
    ///     impl<'f> Foo for &'f u32
    ///     impl<'f> Foo<'f> for u32
    ///
    /// where `'f` is something like `Fresh(0)`. The indices are
    /// unique per impl, but not necessarily continuous.
    Fresh(usize),

    /// Indicates an illegal name was given and an error has been
    /// reported (so we should squelch other derived errors). Occurs
    /// when, e.g., `'_` is used in the wrong place.
    Error,
}

impl ParamName {
    pub fn ident(&self) -> Ident {
        match self {
            ParamName::Plain(ident) => ident.clone(),
            ParamName::Fresh(_) | ParamName::Error => Ident::with_dummy_span("_"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum LifetimeName {
    /// User-given names or fresh (synthetic) names.
    Param(ParamName),

    /// User wrote nothing (e.g., the lifetime in `&u32`).
    Implicit,

    /// Implicit lifetime in a context like `dyn Foo`. This is
    /// distinguished from implicit lifetimes elsewhere because the
    /// lifetime that they default to must appear elsewhere within the
    /// enclosing type.  This means that, in an `impl Trait` context, we
    /// don't have to create a parameter for them. That is, `impl
    /// Trait<Item = &u32>` expands to an opaque type like `type
    /// Foo<'a> = impl Trait<Item = &'a u32>`, but `impl Trait<item =
    /// dyn Bar>` expands to `type Foo = impl Trait<Item = dyn Bar +
    /// 'static>`. The latter uses `ImplicitObjectLifetimeDefault` so
    /// that surrounding code knows not to create a lifetime
    /// parameter.
    ImplicitObjectLifetimeDefault,

    /// Indicates an error during lowering (usually `'_` in wrong place)
    /// that was already reported.
    Error,

    /// User wrote specifies `'_`.
    Underscore,

    /// User wrote `'static`.
    Static,
}

/// A `Path` is essentially Rust's notion of a name; for instance,
/// `std::cmp::PartialEq`. It's represented as a sequence of identifiers,
/// along with a bunch of supporting information.
#[derive(Clone, Debug, Hash)]
pub struct Path {
    pub span: Span,
    /// The resolution for the path.
    pub res: Res,
    /// The segments in the path: the things separated by `::`.
    pub segments: Vec<PathSegment>,
}

impl Path {
    pub fn is_global(&self) -> bool {
        !self.segments.is_empty() && self.segments[0].ident.name == "{{root}}"
    }
}

/// A segment of a path: an identifier, an optional lifetime, and a set of
/// types.
#[derive(Clone, Debug, Hash)]
pub struct PathSegment {
    /// The identifier portion of this path segment.
    pub ident: Ident,
    // `id` and `res` are optional. We currently only use these in save-analysis,
    // any path segments without these will not have save-analysis info and
    // therefore will not have 'jump to def' in IDEs, but otherwise will not be
    // affected. (In general, we don't bother to get the defs for synthesized
    // segments, only for segments which have come from the AST).
    pub hir_id: Option<HirId>,
    pub res: Option<Res>,

    /// Type/lifetime parameters attached to this path. They come in
    /// two flavors: `Path<A,B,C>` and `Path(A,B) -> C`. Note that
    /// this is more than just simple syntactic sugar; the use of
    /// parens affects the region binding rules, so we preserve the
    /// distinction.
    pub args: Option<GenericArgs>,

    /// Whether to infer remaining type parameters, if any.
    /// This only applies to expression and pattern paths, and
    /// out of those only the segments with no type parameters
    /// to begin with, e.g., `Vec::new` is `<Vec<..>>::new::<..>`.
    pub infer_args: bool,
}

impl PathSegment {
    /// Converts an identifier to the corresponding segment.
    pub fn from_ident(ident: Ident) -> PathSegment {
        PathSegment { ident, hir_id: None, res: None, infer_args: true, args: None }
    }

    pub fn args(&self) -> &GenericArgs {
        if let Some(args) = &self.args {
            args
        } else {
            const DUMMY: &GenericArgs = &GenericArgs::none();
            DUMMY
        }
    }
}

/// What kind of definition something is; e.g., `mod` vs `struct`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum DefKind {
    // Type namespace
    Mod,
    /// Refers to the struct itself, [`DefKind::Ctor`] refers to its constructor if it
    /// exists.
    Struct,
    Union,
    Enum,
    /// Refers to the variant itself, [`DefKind::Ctor`] refers to its constructor if it
    /// exists.
    Variant,
    Trait,
    /// Type alias: `type Foo = Bar;`
    TyAlias,
    /// Type from an `extern` block.
    ForeignTy,
    /// Trait alias: `trait IntIterator = Iterator<Item = i32>;`
    TraitAlias,
    /// Associated type: `trait MyTrait { type Assoc; }`
    AssocTy,
    /// Type parameter: the `T` in `struct Vec<T> { ... }`
    TyParam,

    // Value namespace
    Fn,
    Const,
    /// Constant generic parameter: `struct Foo<const N: usize> { ... }`
    ConstParam,
    Static,
    /// Refers to the struct or enum variant's constructor.
    ///
    /// The reason `Ctor` exists in addition to [`DefKind::Struct`] and
    /// [`DefKind::Variant`] is because structs and enum variants exist
    /// in the *type* namespace, whereas struct and enum variant *constructors*
    /// exist in the *value* namespace.
    ///
    /// You may wonder why enum variants exist in the type namespace as opposed
    /// to the value namespace. Check out [RFC 2593] for intuition on why that is.
    ///
    /// [RFC 2593]: https://github.com/rust-lang/rfcs/pull/2593
    Ctor(/* CtorOf, CtorKind */),
    /// Associated function: `impl MyStruct { fn associated() {} }`
    AssocFn,
    /// Associated constant: `trait MyTrait { const ASSOC: usize; }`
    AssocConst,

    // Macro namespace
    Macro(/* MacroKind */),

    // Not namespaced (or they are, but we don't treat them so)
    ExternCrate,
    Use,
    /// An `extern` block.
    ForeignMod,
    /// Anonymous constant, e.g. the `1 + 2` in `[u8; 1 + 2]`, or `const { 1 + 2}`
    AnonConst,
    /// Opaque type, aka `impl Trait`.
    OpaqueTy,
    Field,
    /// Lifetime parameter: the `'a` in `struct Foo<'a> { ... }`
    LifetimeParam,
    /// A use of [`global_asm!`].
    GlobalAsm,
    Impl,
    Closure,
    Generator,
}

/// The resolution of a path or export.
///
/// For every path or identifier in Rust, the compiler must determine
/// what the path refers to. This process is called name resolution,
/// and `Res` is the primary result of name resolution.
///
/// For example, everything prefixed with `/* Res */` in this example has
/// an associated `Res`:
///
/// ```
/// fn str_to_string(s: & /* Res */ str) -> /* Res */ String {
///     /* Res */ String::from(/* Res */ s)
/// }
///
/// /* Res */ str_to_string("hello");
/// ```
///
/// The associated `Res`s will be:
///
/// - `str` will resolve to [`Res::PrimTy`];
/// - `String` will resolve to [`Res::Def`], and the `Res` will include the [`DefId`] for
///   `String` as defined in the standard library;
/// - `String::from` will also resolve to [`Res::Def`], with the [`DefId`] pointing to
///   `String::from`;
/// - `s` will resolve to [`Res::Local`];
/// - the call to `str_to_string` will resolve to [`Res::Def`], with the [`DefId`]
///   pointing to the definition of `str_to_string` in the current crate.
//
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[non_exhaustive]
pub enum Res<Id = HirId> {
    /// Definition having a unique ID (`DefId`), corresponds to something defined in user
    /// code.
    ///
    /// **Not bound to a specific namespace.**
    Def(DefKind, DefId),
    // Type namespace
    /// A primitive type such as `i32` or `str`.
    ///
    /// **Belongs to the type namespace.**
    PrimTy(PrimTy),
    /// The `Self` type, optionally with the trait it is associated with
    /// and optionally with the [`DefId`] of the impl it is associated with.
    ///
    /// **Belongs to the type namespace.**
    ///
    /// For example, the `Self` in
    ///
    /// ```
    /// trait Foo {
    ///     fn foo() -> Box<Self>;
    /// }
    /// ```
    ///
    /// would have the [`DefId`] of `Foo` associated with it. The `Self` in
    ///
    /// ```
    /// struct Bar;
    ///
    /// impl Bar {
    ///     fn new() -> Self { Bar }
    /// }
    /// ```
    ///
    /// would have the [`DefId`] of the impl associated with it. Finally, the `Self` in
    ///
    /// ```
    /// impl Foo for Bar {
    ///     fn foo() -> Box<Self> { Box::new(Bar) }
    /// }
    /// ```
    ///
    /// would have both the [`DefId`] of `Foo` and the [`DefId`] of the impl
    /// associated with it.
    ///
    /// *See also [`Res::SelfCtor`].*
    ///
    /// -----
    ///
    /// HACK(min_const_generics): impl self types also have an optional requirement to
    /// **not** mention any generic parameters to allow the following with
    /// `min_const_generics`: ```
    /// impl Foo { fn test() -> [u8; std::mem::size_of::<Self>()] { todo!() } }
    /// ```
    /// We do however allow `Self` in repeat expression even if it is generic to not
    /// break code which already works on stable while causing the
    /// `const_evaluatable_unchecked` future compat lint.
    ///
    /// FIXME(lazy_normalization_consts): Remove this bodge once that feature is stable.
    SelfTy(
        /// Optionally, the trait associated with this `Self` type.
        Option<DefId>,
        /// Optionally, the impl associated with this `Self` type.
        Option<(DefId, bool)>,
    ),
    /// A tool attribute module; e.g., the `rustfmt` in `#[rustfmt::skip]`.
    ///
    /// **Belongs to the type namespace.**
    ToolMod,
    // Value namespace
    /// The `Self` constructor, along with the [`DefId`]
    /// of the impl it is associated with.
    ///
    /// **Belongs to the value namespace.**
    ///
    /// *See also [`Res::SelfTy`].*
    SelfCtor(DefId),
    /// A local variable or function parameter.
    ///
    /// **Belongs to the value namespace.**
    Local(Id),
    // Macro namespace
    /// An attribute that is *not* implemented via macro.
    /// E.g., `#[inline]` and `#[rustfmt::skip]`, which are essentially directives,
    /// as opposed to `#[test]`, which is a builtin macro.
    ///
    /// **Belongs to the macro namespace.**
    NonMacroAttr(/* NonMacroAttrKind */),
    // All namespaces
    /// Name resolution failed. We use a dummy `Res` variant so later phases
    /// of the compiler won't crash and can instead report more errors.
    ///
    /// **Not bound to a specific namespace.**
    Err,
}

/// A constant (expression) that's not an item or associated item,
/// but needs its own `DefId` for type-checking, const-eval, etc.
/// These are usually found nested inside types (e.g., array lengths)
/// or expressions (e.g., repeat counts), and also used to define
/// explicit discriminant values for enum variants.
///
/// You can check if this anon const is a default in a const param
/// `const N: usize = { ... }` with `tcx.hir().opt_const_param_default_param_hir_id(..)`
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
#[non_exhaustive]
pub struct AnonConst {
    pub hir_id: HirId,
    pub body: BodyId,
}

pub struct AnonConstBuilder {
    pub hir_id: HirId,
    pub body: BodyId,
}

impl AnonConstBuilder {
    pub fn into_with_ctx(self, _: &dyn Context) -> AnonConst {
        AnonConst { hir_id: self.hir_id, body: self.body }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Copy)]
#[non_exhaustive]
pub enum Mutability {
    Mut,
    Not,
}

// N.B., if you change this, you'll probably want to change the corresponding
// type structure in middle/ty.rs as well.
#[derive(Clone, Debug, Hash)]
pub struct MutTy {
    pub ty: Ty,
    pub mutbl: Mutability,
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
#[non_exhaustive]
pub enum FnRetTy {
    /// Return type is not specified.
    ///
    /// Functions default to `()` and
    /// closures default to inference. Span points to where return
    /// type would be inserted.
    DefaultReturn(Span),
    /// Everything else.
    Return(Ty),
}

#[derive(Copy, Clone, Debug, Hash)]
#[non_exhaustive]
pub enum ImplicitSelfKind {
    /// Represents a `fn x(self);`.
    Imm,
    /// Represents a `fn x(mut self);`.
    Mut,
    /// Represents a `fn x(&self);`.
    ImmRef,
    /// Represents a `fn x(&mut self);`.
    MutRef,
    /// Represents when a function does not have a self argument or
    /// when a function has a `self: X` argument.
    None,
}

#[derive(Clone, Debug, Hash)]
pub struct FnDecl {
    /// The types of the function's parameters.
    ///
    /// Additional argument data is stored in the function's [body](Body::params).
    pub inputs: Vec<Ty>,
    pub output: FnRetTy,
    pub c_variadic: bool,
    /// Does the function have an implicit self?
    pub implicit_self: ImplicitSelfKind,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[non_exhaustive]
pub enum IsAsync {
    Async,
    NotAsync,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[non_exhaustive]
pub enum Unsafety {
    Unsafe,
    Normal,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[non_exhaustive]
pub enum Constness {
    Const,
    NotConst,
}

#[derive(Copy, Clone, Debug, Hash)]
pub struct FnHeader {
    pub unsafety: Unsafety,
    pub constness: Constness,
    pub asyncness: IsAsync,
    pub abi: Abi,
}

/// Represents a function's signature in a trait declaration,
/// trait implementation, or a free function.
#[derive(Clone, Debug, Hash)]
pub struct FnSig {
    pub header: FnHeader,
    pub decl: FnDecl,
    pub span: Span,
}

#[derive(Clone, Debug, Hash)]
pub struct ConstArg {
    pub value: AnonConst,
    pub span: Span,
}

#[derive(Copy, Clone, Debug, Hash)]
#[non_exhaustive]
pub enum InferKind {
    Const,
    Type,
}

#[derive(Clone, Debug, Hash)]
pub struct InferArg {
    pub hir_id: HirId,
    pub kind: InferKind,
    pub span: Span,
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum GenericArg {
    Lifetime(Lifetime),
    Type(Ty),
    Const(ConstArg),
    Infer(InferArg),
}

impl GenericArg {
    pub fn span(&self) -> Span {
        match self {
            GenericArg::Lifetime(l) => l.span,
            GenericArg::Type(t) => t.span,
            GenericArg::Const(c) => c.span,
            GenericArg::Infer(i) => i.span,
        }
    }

    pub fn id(&self) -> HirId {
        match self {
            GenericArg::Lifetime(l) => l.hir_id,
            GenericArg::Type(t) => t.hir_id,
            GenericArg::Const(c) => c.value.hir_id,
            GenericArg::Infer(i) => i.hir_id,
        }
    }
}

#[derive(Clone, Debug, Hash)]
pub struct GenericArgs {
    /// The generic arguments for this path segment.
    pub args: Vec<GenericArg>,
    /// Bindings (equality constraints) on associated types, if present.
    /// E.g., `Foo<A = Bar>`.
    pub bindings: Vec<TypeBinding>,
    /// Were arguments written in parenthesized form `Fn(T) -> U`?
    /// This is required mostly for pretty-printing and diagnostics,
    /// but also for changing lifetime elision rules to be "function-like".
    pub parenthesized: bool,
    /// The span encompassing arguments and the surrounding brackets `<>` or `()`
    ///       Foo<A, B, AssocTy = D>           Fn(T, U, V) -> W
    ///          ^^^^^^^^^^^^^^^^^^^             ^^^^^^^^^
    /// Note that this may be:
    /// - empty, if there are no generic brackets (but there may be hidden lifetimes)
    /// - dummy, if this was generated while desugaring
    pub span_ext: Span,
}

impl GenericArgs {
    pub const fn none() -> Self {
        Self { args: vec![], bindings: vec![], parenthesized: false, span_ext: DUMMY_SP }
    }

    /// The span encompassing the text inside the surrounding brackets.
    /// It will also include bindings if they aren't in the form `-> Ret`
    /// Returns `None` if the span is empty (e.g. no brackets) or dummy
    pub fn span(&self) -> Option<Span> {
        let span_ext = self.span_ext()?;
        Some(span_ext.with_lo(span_ext.lo() + 1).with_hi(span_ext.hi() - 1))
    }

    /// Returns span encompassing arguments and their surrounding `<>` or `()`
    pub fn span_ext(&self) -> Option<Span> {
        Some(self.span_ext).filter(|span| !span.is_empty())
    }

    pub fn is_empty(&self) -> bool { self.args.is_empty() }
}

/// A modifier on a bound, currently this is only used for `?Sized`, where the
/// modifier is `Maybe`. Negative bounds should also be handled here.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[non_exhaustive]
pub enum TraitBoundModifier {
    None,
    Maybe,
    MaybeConst,
}

/// The AST represents all type param bounds as types.
/// `typeck::collect::compute_bounds` matches these against
/// the "special" built-in traits (see `middle::lang_items`) and
/// detects `Copy`, `Send` and `Sync`.
#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum GenericBound {
    Trait(PolyTraitRef, TraitBoundModifier),
    // FIXME(davidtwco): Introduce `PolyTraitRef::LangItem`
    LangItemTrait(LangItem, Span, HirId, GenericArgs),
    Outlives(Lifetime),
}

impl GenericBound {
    pub fn trait_ref(&self) -> Option<&TraitRef> {
        match self {
            GenericBound::Trait(data, _) => Some(&data.trait_ref),
            _ => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            GenericBound::Trait(t, ..) => t.span,
            GenericBound::LangItemTrait(_, span, ..) => *span,
            GenericBound::Outlives(l) => l.span,
        }
    }
}

pub type GenericBounds = Vec<GenericBound>;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
#[non_exhaustive]
pub enum LifetimeParamKind {
    // Indicates that the lifetime definition was explicitly declared (e.g., in
    // `fn foo<'a>(x: &'a u8) -> &'a u8 { x }`).
    Explicit,

    // Indicates that the lifetime definition was synthetically added
    // as a result of an in-band lifetime usage (e.g., in
    // `fn foo(x: &'a u8) -> &'a u8 { x }`).
    InBand,

    // Indication that the lifetime was elided (e.g., in both cases in
    // `fn foo(x: &u8) -> &'_ u8 { x }`).
    Elided,

    // Indication that the lifetime name was somehow in error.
    Error,
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum GenericParamKind {
    /// A lifetime definition (e.g., `'a: 'b + 'c + 'd`).
    Lifetime {
        kind: LifetimeParamKind,
    },
    Type {
        default: Option<Ty>,
        synthetic: Option<SyntheticTyParamKind>,
    },
    Const {
        ty: Ty,
        /// Optional default value for the const generic param
        default: Option<AnonConst>,
    },
}

#[derive(Clone, Debug, Hash)]
pub struct GenericParam {
    pub hir_id: HirId,
    pub name: ParamName,
    pub bounds: GenericBounds,
    pub span: Span,
    pub pure_wrt_drop: bool,
    pub kind: GenericParamKind,
}

impl GenericParam {
    pub fn bounds_span(&self) -> Option<Span> {
        self.bounds.iter().fold(None, |span, bound| {
            let span = span.map(|s| s.to(bound.span())).unwrap_or_else(|| bound.span());

            Some(span)
        })
    }
}

/// Synthetic type parameters are converted to another form during lowering; this allows
/// us to track the original form they had, and is useful for error messages.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[non_exhaustive]
pub enum SyntheticTyParamKind {
    ImplTrait,
    // Created by the `#[rustc_synthetic]` attribute.
    FromAttr,
}

/// A where-clause in a definition.
#[derive(Clone, Debug, Hash)]
pub struct WhereClause {
    pub predicates: Vec<WherePredicate>,
    // Only valid if predicates aren't empty.
    pub span: Span,
}

impl WhereClause {
    pub fn span(&self) -> Option<Span> {
        if self.predicates.is_empty() { None } else { Some(self.span) }
    }
}

/// A single predicate in a where-clause.
#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum WherePredicate {
    /// A type binding (e.g., `for<'c> Foo: Send + Clone + 'c`).
    BoundPredicate(WhereBoundPredicate),
    /// A lifetime predicate (e.g., `'a: 'b + 'c`).
    RegionPredicate(WhereRegionPredicate),
    /// An equality predicate (unsupported).
    EqPredicate(WhereEqPredicate),
}

/// A type bound (e.g., `for<'c> Foo: Send + Clone + 'c`).
#[derive(Clone, Debug, Hash)]
pub struct WhereBoundPredicate {
    pub span: Span,
    /// Any generics from a `for` binding.
    pub bound_generic_params: Vec<GenericParam>,
    /// The type being bounded.
    pub bounded_ty: Ty,
    /// Trait and lifetime bounds (e.g., `Clone + Send + 'static`).
    pub bounds: GenericBounds,
}

/// A lifetime predicate (e.g., `'a: 'b + 'c`).
#[derive(Clone, Debug, Hash)]
pub struct WhereRegionPredicate {
    pub span: Span,
    pub lifetime: Lifetime,
    pub bounds: GenericBounds,
}

/// An equality predicate (e.g., `T = int`); currently unsupported.
#[derive(Clone, Debug, Hash)]
pub struct WhereEqPredicate {
    pub hir_id: HirId,
    pub span: Span,
    pub lhs_ty: Ty,
    pub rhs_ty: Ty,
}

/// Represents lifetimes and type parameters attached to a declaration
/// of a function, enum, trait, etc.
#[derive(Clone, Debug, Hash)]
pub struct Generics {
    pub params: Vec<GenericParam>,
    pub where_clause: WhereClause,
    pub span: Span,
}

impl Generics {
    pub const fn empty() -> Generics {
        Generics {
            params: vec![],
            where_clause: WhereClause { predicates: vec![], span: DUMMY_SP },
            span: DUMMY_SP,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug, Hash)]
#[non_exhaustive]
pub enum UseKind {
    /// One import, e.g., `use foo::bar` or `use foo::bar as baz`.
    /// Also produced for each element of a list `use`, e.g.
    /// `use foo::{a, b}` lowers to `use foo::a; use foo::b;`.
    Single,

    /// Glob import, e.g., `use foo::*`.
    Glob,

    /// Degenerate list import, e.g., `use foo::{a, b}` produces
    /// an additional `use foo::{}` for performing checks such as
    /// unstable feature gating. May be removed in the future.
    ListStem,
}

/// References to traits in impls.
///
/// `resolve` maps each `TraitRef`'s `ref_id` to its defining trait; that's all
/// that the `ref_id` is for. Note that `ref_id`'s value is not the `HirId` of the
/// trait being referred to but just a unique `HirId` that serves as a key
/// within the resolution map.
#[derive(Clone, Debug, Hash)]
pub struct TraitRef {
    pub path: Path,
    // Don't hash the `ref_id`. It is tracked via the thing it is used to access.
    pub hir_ref_id: HirId,
}

impl TraitRef {
    /// Gets the `DefId` of the referenced trait. It _must_ actually be a trait or trait
    /// alias.
    pub fn trait_def_id(&self) -> Option<DefId> {
        match self.path.res {
            Res::Def(DefKind::Trait | DefKind::TraitAlias, did) => Some(did),
            Res::Err => None,
            _ => unreachable!(),
        }
    }
}

#[derive(Clone, Debug, Hash)]
pub struct PolyTraitRef {
    /// The `'a` in `for<'a> Foo<&'a T>`.
    pub bound_generic_params: Vec<GenericParam>,

    /// The `Foo<&'a T>` in `for<'a> Foo<&'a T>`.
    pub trait_ref: TraitRef,

    pub span: Span,
}

#[derive(Copy, Clone, Debug, Hash)]
#[non_exhaustive]
pub enum CrateSugar {
    /// Source is `pub(crate)`.
    PubCrate,
    /// Source is (just) `crate`.
    JustCrate,
}

pub type Visibility = Spanned<VisibilityKind>;

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum VisibilityKind {
    Public,
    Crate(CrateSugar),
    Restricted { path: Path, hir_id: HirId },
    Inherited,
}

#[derive(Clone, Debug, Hash)]
pub struct Mod {
    /// A span from the first token past `{` to the last token until `}`.
    /// For `mod foo;`, the inner span ranges from the first token
    /// to the last token in the external file.
    pub inner: Span,
    pub item_ids: Vec<ItemId>,
}

#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum ItemKind {
    ExternCrate(Option<Symbol>),
    /// `use foo::bar::*;` or `use foo::bar::baz as quux;`
    ///
    /// or just
    ///
    /// `use foo::bar::baz;` (with `as baz` implicitly on the right).
    Use(Path, UseKind),
    /// A `static` item.
    Static(Ty, Mutability, BodyId),
    /// A `const` item.
    Const(Ty, BodyId),
    /// A function declaration.
    Fn(FnSig, Generics, BodyId),
    /// A module.
    Mod(Mod),

    /// An external module, e.g. `extern { .. }`.
    // ForeignMod {
    //     abi: Abi,
    //     items: Vec<ForeignItemRef>,
    // },

    /// Module-level inline assembly (from `global_asm!`).
    // GlobalAsm(InlineAsm),

    /// A type alias, e.g., `type Foo = Bar<u8>`.
    TyAlias(Ty, Generics),
    /// An opaque `impl Trait` type alias, e.g., `type Foo = impl Bar;`.
    // OpaqueTy(OpaqueTy),

    /// An enum definition, e.g., `enum Foo<A, B> {C<A>, D<B>}`.
    // Enum(EnumDef, Generics),

    /// A struct definition, e.g., `struct Foo<A> {x: A}`.
    // Struct(VariantData, Generics),

    /// A union definition, e.g., `union Foo<A, B> {x: A, y: B}`.
    // Union(VariantData, Generics),

    /// A trait definition.
    // Trait(IsAuto, Unsafety, Generics, GenericBounds, Vec<TraitItemRef]>,

    /// A trait alias.
    // TraitAlias(Generics, GenericBounds),

    /// An implementation, e.g., `impl<A> Trait for Foo { .. }`.
    // Impl(Impl),
    LetTheDocsBeValid,
}

/// An item
///
/// The name might be a dummy name in case of anonymous items
#[derive(Clone, Debug, Hash)]
pub struct ItemBuilder {
    pub ident: Ident,
    pub def_id: LocalDefId,
    pub kind: ItemKind,
    pub vis: Visibility,
    pub span: Span,
}

impl ItemBuilder {
    pub fn into_with_ctx(self, _: &dyn Context) -> Item {
        Item {
            ident: self.ident,
            def_id: self.def_id,
            kind: self.kind,
            vis: self.vis,
            span: self.span,
        }
    }
}

/// An item
///
/// The name might be a dummy name in case of anonymous items
#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub struct Item {
    pub ident: Ident,
    pub def_id: LocalDefId,
    pub kind: ItemKind,
    pub vis: Visibility,
    pub span: Span,
}

/// Syntax used to declare a trait object.
#[derive(Clone, Copy, PartialEq, Debug, Hash)]
#[non_exhaustive]
pub enum TraitObjectSyntax {
    Dyn,
    None,
}

/// Not represented directly in the AST; referred to by name through a `ty_path`.
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[non_exhaustive]
pub enum PrimTy {
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
    Str,
    Bool,
    Char,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[non_exhaustive]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[non_exhaustive]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
#[non_exhaustive]
pub enum FloatTy {
    F32,
    F64,
}

/// Bind a type to an associated type (i.e., `A = Foo`).
///
/// Bindings like `A: Debug` are represented as a special type `A =
/// $::Debug` that is understood by the astconv code.
///
/// FIXME(alexreg): why have a separate type for the binding case,
/// wouldn't it be better to make the `ty` field an enum like the
/// following?
///
/// ```
/// enum TypeBindingKind {
///    Equals(...),
///    Binding(...),
/// }
/// ```
#[derive(Clone, Debug, Hash)]
pub struct TypeBinding {
    pub hir_id: HirId,
    pub ident: Ident,
    pub gen_args: GenericArgs,
    pub kind: TypeBindingKind,
    pub span: Span,
}

// Represents the two kinds of type bindings.
#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum TypeBindingKind {
    /// E.g., `Foo<Bar: Send>`.
    Constraint { bounds: Vec<GenericBound> },
    /// E.g., `Foo<Bar = ()>`.
    Equality { ty: Ty },
}

impl TypeBinding {
    pub fn ty(&self) -> &Ty {
        match &self.kind {
            TypeBindingKind::Equality { ty } => ty,
            _ => panic!("expected equality type binding for parenthesized generic args"),
        }
    }
}

/// Represents an optionally `Self`-qualified value/type path or associated extension.
///
/// To resolve the path to a `DefId`, call [`qpath_res`].
///
/// [`qpath_res`]: ../rustc_middle/ty/struct.TypeckResults.html#method.qpath_res
#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum QPath {
    /// Path to a definition, optionally "fully-qualified" with a `Self`
    /// type, if the path points to an associated item in a trait.
    ///
    /// E.g., an unqualified path like `Clone::clone` has `None` for `Self`,
    /// while `<Vec<T> as Clone>::clone` has `Some(Vec<T>)` for `Self`,
    /// even though they both have the same two-segment `Clone::clone` `Path`.
    Resolved(Option<Ty>, Path),

    /// Type-related paths (e.g., `<T>::default` or `<T>::Output`).
    /// Will be resolved by type-checking to an associated item.
    ///
    /// UFCS source paths can desugar into this, with `Vec::new` turning into
    /// `<Vec>::new`, and `T::X::Y::method` into `<<<T>::X>::Y>::method`,
    /// the `X` and `Y` nodes each being a `TyKind::Path(QPath::TypeRelative(..))`.
    TypeRelative(Ty, PathSegment),

    /// Reference to a `#[lang = "foo"]` item.
    LangItem(LangItem, Span),
}

#[derive(Clone, Debug, Hash)]
pub struct BareFnTy {
    pub unsafety: Unsafety,
    pub abi: Abi,
    pub generic_params: Vec<GenericParam>,
    pub decl: FnDecl,
    pub param_names: Vec<Ident>,
}

/// The various kinds of types recognized by the compiler.
#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub enum TyKind {
    /// A variable length slice (i.e., `[T]`).
    Slice(Ty),
    /// A fixed length array (i.e., `[T; n]`).
    Array(Ty, AnonConst),
    /// A raw pointer (i.e., `*const T` or `*mut T`).
    Ptr(MutTy),
    /// A reference (i.e., `&'a T` or `&'a mut T`).
    Rptr(Lifetime, MutTy),
    /// A bare function (e.g., `fn(usize) -> bool`).
    BareFn(BareFnTy),
    /// The never type (`!`).
    Never,
    /// A tuple (`(A, B, C, D, ...)`).
    Tup(Vec<Ty>),
    /// A path to a type definition (`module::module::...::Type`), or an
    /// associated type (e.g., `<Vec<T> as Trait>::Type` or `<T>::Target`).
    ///
    /// Type parameters may be stored in each `PathSegment`.
    Path(QPath),
    /// A opaque type definition itself. This is currently only used for the
    /// `opaque type Foo: Trait` item that `impl Trait` in desugars to.
    ///
    /// The generic argument list contains the lifetimes (and in the future
    /// possibly parameters) that are actually bound on the `impl Trait`.
    OpaqueDef(ItemId, Vec<GenericArg>),
    /// A trait object type `Bound1 + Bound2 + Bound3`
    /// where `Bound` is a trait or a lifetime.
    TraitObject(Vec<PolyTraitRef>, Lifetime, TraitObjectSyntax),
    /// Unused for now.
    Typeof(AnonConst),
    /// `TyKind::Infer` means the type should be inferred instead of it having been
    /// specified. This can appear anywhere in a type.
    Infer,
    /// Placeholder for a type that has failed to be defined.
    Err,
}

/// Represents any type.
#[derive(Clone, Debug, Hash)]
#[non_exhaustive]
pub struct Ty {
    hir_id: HirId,
    kind: Box<TyKind>,
    span: Span,
}

impl Ty {
    /// Get the `HirId` of this type.
    pub fn hir_id(&self) -> HirId { self.hir_id }

    /// Get the `TyKind` of this type.
    pub fn kind(&self) -> &TyKind { self.kind.as_ref() }

    /// Get the `Span` for this type.
    pub fn span(&self) -> Span { self.span }
}

#[derive(Clone, Debug, Hash)]
pub struct TyBuilder {
    pub hir_id: HirId,
    pub kind: Box<TyKind>,
    pub span: Span,
}

impl TyBuilder {
    // `&dyn Context` is used to make sure that if fields change we have access to enough
    // info to fill the field.
    pub fn into_with_ctx(self, _: &dyn Context) -> Ty {
        Ty { hir_id: self.hir_id, kind: self.kind, span: self.span }
    }
}

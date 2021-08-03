#![allow(unused)]

use std::path::PathBuf;

use libloading::{library_filename, Library};
use pluggy_api as stable;
use rustc_ast::{CrateSugar, FloatTy, IntTy, UintTy};
use rustc_hir::{
    def::{DefKind, Res},
    def_id::DefId,
    AnonConst, BodyId, Constness, Crate, FnDecl, FnHeader, FnRetTy, FnSig, GenericArgs,
    GenericBound, GenericParam, GenericParamKind, Generics, HirId, ImplicitSelfKind,
    IsAsync, Item, ItemId, ItemKind, Lifetime, LifetimeName, LifetimeParamKind, MutTy,
    Mutability, ParamName, Path, PathSegment, PolyTraitRef, PrimTy, QPath,
    SyntheticTyParamKind, TraitBoundModifier, Ty, TyKind, Unsafety, UseKind,
    VisibilityKind, WhereBoundPredicate, WhereClause, WhereEqPredicate, WherePredicate,
    WhereRegionPredicate,
};
use rustc_lint::{
    FutureIncompatibleInfo, LateContext, LateLintPass, Level, Lint, LintContext, LintId,
    LintPass, LintStore,
};
use rustc_lint_defs::FutureIncompatibilityReason;
use rustc_session::{declare_tool_lint, Session};
use rustc_span::{
    def_id::LocalDefId, edition::Edition, BytePos, Pos, Span, Symbol, SyntaxContext,
};
use rustc_target::spec::abi::Abi;

type FetchLint = fn() -> Vec<&'static stable::lint::Lint>;
type LintImpl = fn() -> *mut Box<dyn stable::LateLintImpl>;

trait IntoStable {
    type Out;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out;
}

trait FromStable {
    type Out;
    fn as_rustc(&self) -> Self::Out;
}

struct Ctxt<'a, 'hir>(&'a LateContext<'hir>);

impl<'hir> stable::Context for Ctxt<'_, 'hir> {
    fn def_path_str(&self, id: stable::DefId) -> String {
        self.0.tcx.def_path_str(id.as_rustc())
    }
    fn warn(&self, msg: &str, lint: &stable::lint::Lint) {
        self.0.lint(Box::leak(box lint.as_rustc()), |diag| {
            let mut diag = diag.build(msg);
            diag.emit();
        })
    }

    fn warn_span(&self, msg: &str, lint: &stable::lint::Lint, sp: stable::Span) {
        let span = sp.as_rustc();
        self.0.struct_span_lint(Box::leak(box lint.as_rustc()), span, |diag| {
            let mut diag = diag.build(msg);
            diag.emit();
        })
    }
}

impl FromStable for stable::DefId {
    type Out = DefId;
    fn as_rustc(&self) -> Self::Out {
        DefId { krate: self.krate.into(), index: self.index.as_u32().into() }
    }
}

impl IntoStable for DefId {
    type Out = stable::DefId;
    fn as_stable(&self, _: &dyn stable::Context) -> Self::Out {
        stable::DefId { krate: self.krate.into(), index: self.index.as_u32().into() }
    }
}

impl FromStable for stable::ItemId {
    type Out = ItemId;
    fn as_rustc(&self) -> Self::Out {
        ItemId {
            def_id: LocalDefId {
                local_def_index: self.def_id.local_def_index.as_u32().into(),
            },
        }
    }
}

impl IntoStable for ItemId {
    type Out = stable::ItemId;
    fn as_stable(&self, _: &dyn stable::Context) -> Self::Out {
        stable::ItemId {
            def_id: stable::LocalDefId::from_u32(self.def_id.local_def_index.as_u32()),
        }
    }
}

impl FromStable for stable::HirId {
    type Out = HirId;
    fn as_rustc(&self) -> Self::Out {
        HirId {
            owner: LocalDefId {
                local_def_index: self.owner.local_def_index.as_u32().into(),
            },
            local_id: self.local_id.as_u32().into(),
        }
    }
}

impl IntoStable for HirId {
    type Out = stable::HirId;
    fn as_stable(&self, _: &dyn stable::Context) -> Self::Out {
        stable::HirId::from_raw(
            self.owner.local_def_index.as_u32(),
            self.local_id.as_u32(),
        )
    }
}

impl FromStable for stable::BodyId {
    type Out = BodyId;
    fn as_rustc(&self) -> Self::Out { BodyId { hir_id: self.hir_id.as_rustc() } }
}

impl IntoStable for BodyId {
    type Out = stable::BodyId;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::BodyId { hir_id: self.hir_id.as_stable(cx) }
    }
}

impl FromStable for stable::Span {
    type Out = Span;
    fn as_rustc(&self) -> Self::Out {
        Span::new(BytePos(self.lo()), BytePos(self.hi()), unsafe {
            std::mem::transmute_copy(&self.ctxt().as_u32())
        })
    }
}

impl IntoStable for Span {
    type Out = stable::Span;
    fn as_stable(&self, _: &dyn stable::Context) -> Self::Out {
        stable::Span::new(
            self.lo().to_u32(),
            self.hi().to_u32(),
            stable::SyntaxContext::from_u32(unsafe {
                std::mem::transmute_copy(&self.ctxt())
            }),
        )
    }
}

impl FromStable for stable::lint::Level {
    type Out = Level;
    fn as_rustc(&self) -> Self::Out {
        match self {
            stable::lint::Level::Allow => Level::Allow,
            stable::lint::Level::Warn => Level::Warn,
            stable::lint::Level::Deny => Level::Deny,
            stable::lint::Level::Forbid => Level::Forbid,
            _ => Level::Allow,
        }
    }
}

impl FromStable for stable::lint::Edition {
    type Out = Edition;
    fn as_rustc(&self) -> Self::Out {
        match self {
            stable::lint::Edition::Edition2015 => Edition::Edition2015,
            stable::lint::Edition::Edition2018 => Edition::Edition2018,
            stable::lint::Edition::Edition2021 => Edition::Edition2021,
            _ => Edition::Edition2021,
        }
    }
}

impl FromStable for stable::lint::FutureIncompatibleInfo {
    type Out = FutureIncompatibleInfo;
    fn as_rustc(&self) -> Self::Out {
        FutureIncompatibleInfo {
            reference: self.reference,
            reason: match self.reason {
                stable::lint::FutureIncompatibilityReason::FutureReleaseError => {
                    FutureIncompatibilityReason::FutureReleaseError
                }
                stable::lint::FutureIncompatibilityReason::FutureReleaseErrorReportNow => {
                    FutureIncompatibilityReason::FutureReleaseErrorReportNow
                }
                stable::lint::FutureIncompatibilityReason::EditionError(ed) => {
                    FutureIncompatibilityReason::EditionError(ed.as_rustc())
                }
                stable::lint::FutureIncompatibilityReason::EditionSemanticsChange(ed) => {
                    FutureIncompatibilityReason::EditionSemanticsChange(ed.as_rustc())
                }
                _ => todo!("semver broke"),
            },
            explain_reason: self.explain_reason,
        }
    }
}

impl FromStable for stable::lint::Lint {
    type Out = Lint;
    fn as_rustc(&self) -> Self::Out {
        Lint {
            name: self.name,
            default_level: self.default_level.as_rustc(),
            desc: self.desc,
            edition_lint_opts: self
                .edition_lint_opts
                .map(|(ed, lvl)| (ed.as_rustc(), lvl.as_rustc())),
            report_in_external_macro: self.report_in_external_macro,
            future_incompatible: self.future_incompatible.map(|incomp| incomp.as_rustc()),
            is_plugin: self.is_plugin,
            feature_gate: self.feature_gate.map(|s| Symbol::intern(s)),
            crate_level_only: self.crate_level_only,
        }
    }
}

impl IntoStable for DefKind {
    type Out = stable::DefKind;
    fn as_stable(&self, _: &dyn stable::Context) -> Self::Out {
        match self {
            DefKind::Mod => stable::DefKind::Mod,
            DefKind::Struct => stable::DefKind::Struct,
            DefKind::Union => stable::DefKind::Union,
            DefKind::Enum => stable::DefKind::Enum,
            DefKind::Variant => stable::DefKind::Variant,
            DefKind::Trait => stable::DefKind::Trait,
            DefKind::TyAlias => stable::DefKind::TyAlias,
            DefKind::ForeignTy => stable::DefKind::ForeignTy,
            DefKind::TraitAlias => stable::DefKind::TraitAlias,
            DefKind::AssocTy => stable::DefKind::AssocTy,
            DefKind::TyParam => stable::DefKind::TyParam,
            DefKind::Fn => stable::DefKind::Fn,
            DefKind::Const => stable::DefKind::Const,
            DefKind::ConstParam => stable::DefKind::ConstParam,
            DefKind::Static => stable::DefKind::Static,
            DefKind::Ctor(_, _) => stable::DefKind::Ctor(),
            DefKind::AssocFn => stable::DefKind::AssocFn,
            DefKind::AssocConst => stable::DefKind::AssocConst,
            DefKind::Macro(_) => stable::DefKind::Macro(),
            DefKind::ExternCrate => stable::DefKind::ExternCrate,
            DefKind::Use => stable::DefKind::Use,
            DefKind::ForeignMod => stable::DefKind::ForeignMod,
            DefKind::AnonConst => stable::DefKind::AnonConst,
            DefKind::OpaqueTy => stable::DefKind::OpaqueTy,
            DefKind::Field => stable::DefKind::Field,
            DefKind::LifetimeParam => stable::DefKind::LifetimeParam,
            DefKind::GlobalAsm => stable::DefKind::GlobalAsm,
            DefKind::Impl => stable::DefKind::Impl,
            DefKind::Closure => stable::DefKind::Closure,
            DefKind::Generator => stable::DefKind::Generator,
        }
    }
}

impl IntoStable for PrimTy {
    type Out = stable::PrimTy;
    fn as_stable(&self, _: &dyn stable::Context) -> Self::Out {
        match self {
            PrimTy::Int(int) => stable::PrimTy::Int(match int {
                IntTy::I8 => stable::IntTy::I8,
                IntTy::I16 => stable::IntTy::I16,
                IntTy::I32 => stable::IntTy::I32,
                IntTy::I64 => stable::IntTy::I64,
                IntTy::I128 => stable::IntTy::I128,
                IntTy::Isize => stable::IntTy::Isize,
            }),
            PrimTy::Uint(uint) => stable::PrimTy::Uint(match uint {
                UintTy::U8 => stable::UintTy::U8,
                UintTy::U16 => stable::UintTy::U16,
                UintTy::U32 => stable::UintTy::U32,
                UintTy::U64 => stable::UintTy::U64,
                UintTy::U128 => stable::UintTy::U128,
                UintTy::Usize => stable::UintTy::Usize,
            }),
            PrimTy::Float(float) => stable::PrimTy::Float(match float {
                FloatTy::F32 => stable::FloatTy::F32,
                FloatTy::F64 => stable::FloatTy::F64,
            }),
            PrimTy::Str => stable::PrimTy::Str,
            PrimTy::Bool => stable::PrimTy::Bool,
            PrimTy::Char => stable::PrimTy::Char,
        }
    }
}

impl IntoStable for Res {
    type Out = stable::Res;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        match self {
            Res::Def(kind, id) => stable::Res::Def(kind.as_stable(cx), id.as_stable(cx)),
            Res::PrimTy(prim) => stable::Res::PrimTy(prim.as_stable(cx)),
            Res::SelfTy(_, _) => todo!(),
            Res::ToolMod => stable::Res::ToolMod,
            Res::SelfCtor(_) => todo!(),
            Res::Local(_) => todo!(),
            Res::NonMacroAttr(_) => todo!(),
            Res::Err => stable::Res::Err,
        }
    }
}

impl IntoStable for PathSegment<'_> {
    type Out = stable::PathSegment;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::PathSegment {
            ident: stable::Ident::new(
                &self.ident.name.as_str(),
                self.ident.span.as_stable(cx),
            ),
            hir_id: self.hir_id.map(|id| id.as_stable(cx)),
            res: self.res.map(|res| res.as_stable(cx)),
            args: self.args.map(|args| args.as_stable(cx)),
            infer_args: self.infer_args,
        }
    }
}

impl IntoStable for Path<'_> {
    type Out = stable::Path;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::Path {
            span: self.span.as_stable(cx),
            res: self.res.as_stable(cx),
            segments: self
                .segments
                .iter()
                .map(|seg| seg.as_stable(cx))
                .collect::<Vec<_>>(),
        }
    }
}

impl IntoStable for GenericArgs<'_> {
    type Out = stable::GenericArgs;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::GenericArgs {
            args: vec![],
            bindings: vec![],
            parenthesized: self.parenthesized,
            span_ext: self.span_ext.as_stable(cx),
        }
    }
}

impl IntoStable for LifetimeParamKind {
    type Out = stable::LifetimeParamKind;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        match self {
            LifetimeParamKind::Explicit => stable::LifetimeParamKind::Explicit,
            LifetimeParamKind::InBand => stable::LifetimeParamKind::InBand,
            LifetimeParamKind::Elided => stable::LifetimeParamKind::Elided,
            LifetimeParamKind::Error => stable::LifetimeParamKind::Error,
        }
    }
}

impl IntoStable for GenericParam<'_> {
    type Out = stable::GenericParam;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::GenericParam {
            hir_id: self.hir_id.as_stable(cx),
            name: self.name.as_stable(cx),
            kind: match self.kind {
                GenericParamKind::Lifetime { kind } => {
                    stable::GenericParamKind::Lifetime { kind: kind.as_stable(cx) }
                }
                GenericParamKind::Type { default, synthetic } => {
                    stable::GenericParamKind::Type {
                        default: default.map(|d| d.as_stable(cx)),
                        synthetic: synthetic.map(|s| match s {
                            SyntheticTyParamKind::ImplTrait => {
                                stable::SyntheticTyParamKind::ImplTrait
                            }
                            SyntheticTyParamKind::FromAttr => {
                                stable::SyntheticTyParamKind::FromAttr
                            }
                        }),
                    }
                }
                GenericParamKind::Const { ty, default } => {
                    stable::GenericParamKind::Const {
                        ty: ty.as_stable(cx),
                        default: default.map(|d| d.as_stable(cx)),
                    }
                }
            },
            span: self.span.as_stable(cx),
            bounds: self.bounds.iter().map(|b| b.as_stable(cx)).collect(),
            pure_wrt_drop: self.pure_wrt_drop,
        }
    }
}

impl IntoStable for GenericBound<'_> {
    type Out = stable::GenericBound;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        match self {
            GenericBound::Trait(_, _) => todo!(),
            GenericBound::LangItemTrait(_, _, _, _) => todo!(),
            GenericBound::Outlives(_) => todo!(),
        }
    }
}

impl IntoStable for WhereBoundPredicate<'_> {
    type Out = stable::WhereBoundPredicate;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::WhereBoundPredicate {
            bound_generic_params: self
                .bound_generic_params
                .iter()
                .map(|b| b.as_stable(cx))
                .collect(),
            bounded_ty: self.bounded_ty.as_stable(cx),
            span: self.span.as_stable(cx),
            bounds: self.bounds.iter().map(|b| b.as_stable(cx)).collect(),
        }
    }
}

impl IntoStable for WhereRegionPredicate<'_> {
    type Out = stable::WhereRegionPredicate;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::WhereRegionPredicate {
            lifetime: self.lifetime.as_stable(cx),
            bounds: self.bounds.iter().map(|b| b.as_stable(cx)).collect(),
            span: self.span.as_stable(cx),
        }
    }
}

impl IntoStable for WhereEqPredicate<'_> {
    type Out = stable::WhereEqPredicate;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::WhereEqPredicate {
            hir_id: self.hir_id.as_stable(cx),
            lhs_ty: self.lhs_ty.as_stable(cx),
            rhs_ty: self.rhs_ty.as_stable(cx),
            span: self.span.as_stable(cx),
        }
    }
}

impl IntoStable for WhereClause<'_> {
    type Out = stable::WhereClause;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::WhereClause {
            predicates: self
                .predicates
                .iter()
                .map(|pred| match pred {
                    WherePredicate::BoundPredicate(bound) => {
                        stable::WherePredicate::BoundPredicate(bound.as_stable(cx))
                    }
                    WherePredicate::RegionPredicate(reg) => {
                        stable::WherePredicate::RegionPredicate(reg.as_stable(cx))
                    }
                    WherePredicate::EqPredicate(eq) => {
                        stable::WherePredicate::EqPredicate(eq.as_stable(cx))
                    }
                })
                .collect(),
            span: self.span.as_stable(cx),
        }
    }
}

impl IntoStable for Generics<'_> {
    type Out = stable::Generics;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::Generics {
            params: vec![],
            where_clause: self.where_clause.as_stable(cx),
            span: self.span.as_stable(cx),
        }
    }
}

impl IntoStable for FnHeader {
    type Out = stable::FnHeader;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::FnHeader {
            unsafety: match self.unsafety {
                Unsafety::Normal => stable::Unsafety::Normal,
                Unsafety::Unsafe => stable::Unsafety::Unsafe,
            },
            constness: match self.constness {
                Constness::Const => stable::Constness::Const,
                Constness::NotConst => stable::Constness::NotConst,
            },
            asyncness: match self.asyncness {
                IsAsync::Async => stable::IsAsync::Async,
                IsAsync::NotAsync => stable::IsAsync::NotAsync,
            },
            abi: match self.abi {
                Abi::Rust => stable::Abi::Rust,
                Abi::C { unwind } => stable::Abi::C { unwind },
                Abi::Cdecl => stable::Abi::Cdecl,
                Abi::Stdcall { unwind } => stable::Abi::Stdcall { unwind },
                Abi::Fastcall => stable::Abi::Fastcall,
                Abi::Vectorcall => stable::Abi::Vectorcall,
                Abi::Thiscall { unwind } => stable::Abi::Thiscall { unwind },
                Abi::Aapcs => stable::Abi::Aapcs,
                Abi::Win64 => stable::Abi::Win64,
                Abi::SysV64 => stable::Abi::SysV64,
                Abi::PtxKernel => stable::Abi::PtxKernel,
                Abi::Msp430Interrupt => stable::Abi::Msp430Interrupt,
                Abi::X86Interrupt => stable::Abi::X86Interrupt,
                Abi::AmdGpuKernel => stable::Abi::AmdGpuKernel,
                Abi::EfiApi => stable::Abi::EfiApi,
                Abi::AvrInterrupt => stable::Abi::AvrInterrupt,
                Abi::AvrNonBlockingInterrupt => stable::Abi::AvrNonBlockingInterrupt,
                Abi::CCmseNonSecureCall => stable::Abi::CCmseNonSecureCall,
                Abi::Wasm => stable::Abi::Wasm,
                Abi::System { unwind } => stable::Abi::System { unwind },
                Abi::RustIntrinsic => stable::Abi::RustIntrinsic,
                Abi::RustCall => stable::Abi::RustCall,
                Abi::PlatformIntrinsic => stable::Abi::PlatformIntrinsic,
                Abi::Unadjusted => stable::Abi::Unadjusted,
            },
        }
    }
}

impl IntoStable for FnRetTy<'_> {
    type Out = stable::FnRetTy;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        match self {
            FnRetTy::DefaultReturn(sp) => {
                stable::FnRetTy::DefaultReturn(sp.as_stable(cx))
            }
            FnRetTy::Return(ty) => stable::FnRetTy::Return(ty.as_stable(cx)),
        }
    }
}

impl IntoStable for FnDecl<'_> {
    type Out = stable::FnDecl;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::FnDecl {
            inputs: self.inputs.iter().map(|i| i.as_stable(cx)).collect(),
            output: self.output.as_stable(cx),
            c_variadic: self.c_variadic,
            implicit_self: match self.implicit_self {
                ImplicitSelfKind::Imm => stable::ImplicitSelfKind::Imm,
                ImplicitSelfKind::Mut => stable::ImplicitSelfKind::Mut,
                ImplicitSelfKind::ImmRef => stable::ImplicitSelfKind::ImmRef,
                ImplicitSelfKind::MutRef => stable::ImplicitSelfKind::MutRef,
                ImplicitSelfKind::None => stable::ImplicitSelfKind::None,
            },
        }
    }
}

impl IntoStable for FnSig<'_> {
    type Out = stable::FnSig;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::FnSig {
            header: self.header.as_stable(cx),
            decl: self.decl.as_stable(cx),
            span: self.span.as_stable(cx),
        }
    }
}

impl<'a> IntoStable for Item<'a> {
    type Out = stable::Item;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::ItemBuilder {
            ident: stable::Ident::new(
                &self.ident.name.as_str(),
                self.ident.span.as_stable(cx),
            ),
            def_id: stable::LocalDefId::from_u32(self.def_id.local_def_index.as_u32()),
            kind: match &self.kind {
                ItemKind::ExternCrate(name) => stable::ItemKind::ExternCrate(
                    name.map(|n| stable::Symbol::from(&*n.as_str())),
                ),
                ItemKind::Use(path, kind) => stable::ItemKind::Use(
                    path.as_stable(cx),
                    match kind {
                        UseKind::Glob => stable::UseKind::Glob,
                        UseKind::ListStem => stable::UseKind::ListStem,
                        UseKind::Single => stable::UseKind::Single,
                    },
                ),
                // ItemKind::Static(_, _, _) => todo!(),
                // ItemKind::Const(_, _) => todo!(),
                ItemKind::Fn(sig, gen, body) => stable::ItemKind::Fn(
                    sig.as_stable(cx),
                    gen.as_stable(cx),
                    body.as_stable(cx),
                ),
                ItemKind::Mod(m) => stable::ItemKind::Mod(stable::Mod {
                    inner: m.inner.as_stable(cx),
                    item_ids: m.item_ids.iter().map(|id| id.as_stable(cx)).collect(),
                }),
                // ItemKind::ForeignMod { abi, items } => todo!(),
                // ItemKind::GlobalAsm(_) => todo!(),
                // ItemKind::TyAlias(_, _) => todo!(),
                // ItemKind::OpaqueTy(_) => todo!(),
                // ItemKind::Enum(_, _) => todo!(),
                // ItemKind::Struct(_, _) => todo!(),
                // ItemKind::Union(_, _) => todo!(),
                // ItemKind::Trait(_, _, _, _, _) => todo!(),
                // ItemKind::TraitAlias(_, _) => todo!(),
                // ItemKind::Impl(_) => todo!(),
                x => panic!("{:?}", x),
            },
            vis: stable::Spanned::new(
                self.vis.span.as_stable(cx),
                match self.vis.node {
                    VisibilityKind::Public => stable::VisibilityKind::Public,
                    VisibilityKind::Inherited => stable::VisibilityKind::Inherited,
                    VisibilityKind::Crate(sug) => {
                        stable::VisibilityKind::Crate(match sug {
                            CrateSugar::PubCrate => stable::CrateSugar::PubCrate,
                            CrateSugar::JustCrate => stable::CrateSugar::JustCrate,
                        })
                    }
                    VisibilityKind::Restricted { path, hir_id } => {
                        stable::VisibilityKind::Restricted {
                            path: path.as_stable(cx),
                            hir_id: hir_id.as_stable(cx),
                        }
                    }
                },
            ),
            span: self.span.as_stable(cx),
        }
        .into_with_ctx(cx)
    }
}

impl IntoStable for ParamName {
    type Out = stable::ParamName;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        match self {
            ParamName::Plain(id) => stable::ParamName::Plain(stable::Ident::new(
                &id.name.as_str(),
                id.span.as_stable(cx),
            )),
            ParamName::Fresh(num) => stable::ParamName::Fresh(*num),
            ParamName::Error => stable::ParamName::Error,
        }
    }
}

impl IntoStable for Lifetime {
    type Out = stable::Lifetime;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::Lifetime {
            hir_id: self.hir_id.as_stable(cx),
            span: self.span.as_stable(cx),
            name: match self.name {
                LifetimeName::Param(param) => {
                    stable::LifetimeName::Param(param.as_stable(cx))
                }
                LifetimeName::Implicit => stable::LifetimeName::Implicit,
                LifetimeName::ImplicitObjectLifetimeDefault => {
                    stable::LifetimeName::ImplicitObjectLifetimeDefault
                }
                LifetimeName::Error => stable::LifetimeName::Error,
                LifetimeName::Underscore => stable::LifetimeName::Underscore,
                LifetimeName::Static => stable::LifetimeName::Static,
            },
        }
    }
}

impl IntoStable for MutTy<'_> {
    type Out = stable::MutTy;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::MutTy {
            ty: self.ty.as_stable(cx),
            mutbl: match self.mutbl {
                Mutability::Mut => stable::Mutability::Mut,
                Mutability::Not => stable::Mutability::Not,
            },
        }
    }
}

impl IntoStable for AnonConst {
    type Out = stable::AnonConst;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::AnonConstBuilder {
            hir_id: self.hir_id.as_stable(cx),
            body: self.body.as_stable(cx),
        }
        .into_with_ctx(cx)
    }
}

impl<'a> IntoStable for TyKind<'a> {
    type Out = stable::TyKind;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        match self {
            TyKind::Slice(ty) => stable::TyKind::Slice(ty.as_stable(cx)),
            TyKind::Array(ty, konst) => {
                stable::TyKind::Array(ty.as_stable(cx), konst.as_stable(cx))
            }
            TyKind::Ptr(p) => stable::TyKind::Ptr(p.as_stable(cx)),
            TyKind::Rptr(lt, p) => {
                stable::TyKind::Rptr(lt.as_stable(cx), p.as_stable(cx))
            }
            TyKind::BareFn(_) => stable::TyKind::BareFn(todo!()),
            TyKind::Never => stable::TyKind::Never,
            TyKind::Tup(tys) => {
                stable::TyKind::Tup(tys.iter().map(|t| t.as_stable(cx)).collect())
            }
            TyKind::Path(qpath) => stable::TyKind::Path(match qpath {
                QPath::Resolved(ty, path) => stable::QPath::Resolved(
                    ty.map(|t| t.as_stable(cx)),
                    path.as_stable(cx),
                ),
                QPath::TypeRelative(ty, path) => {
                    stable::QPath::TypeRelative(ty.as_stable(cx), path.as_stable(cx))
                }
                QPath::LangItem(ty, path) => {
                    stable::QPath::LangItem(todo!(), path.as_stable(cx))
                }
            }),
            TyKind::OpaqueDef(_, _) => todo!(),
            TyKind::TraitObject(_, _, _) => todo!(),
            TyKind::Typeof(_) => stable::TyKind::Typeof(todo!()),
            TyKind::Infer => stable::TyKind::Infer,
            TyKind::Err => stable::TyKind::Err,
        }
    }
}

impl<'a> IntoStable for Ty<'a> {
    type Out = stable::Ty;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        stable::TyBuilder {
            hir_id: self.hir_id.as_stable(cx),
            kind: box self.kind.as_stable(cx),
            span: self.span.as_stable(cx),
        }
        .into_with_ctx(cx)
    }
}

// This needs to do more than just get one lint.
fn fetch_lint() -> Library {
    let lib_name = library_filename("test_lint");
    let mut path: PathBuf = "./target/debug/deps/".into();
    path.push(lib_name);

    unsafe { Library::new(path) }.unwrap()
}

pub fn register_lints(_sess: &Session, store: &mut LintStore) {
    // store.register_lints(&[REG]);
    // store.register_group(false, "all", None, vec![LintId::of(REG)]);
    // store.register_late_pass(move || box FooLint { lints: fetch_lint() });
    let lib = fetch_lint();
    let lints = unsafe { lib.get::<FetchLint>(b"get_lints").unwrap()() };

    let leaked_lints: Vec<&'static Lint> = lints
        .iter()
        .map(|l| {
            let s: &'static Lint = Box::leak(box l.as_rustc());
            s
        })
        .collect::<Vec<_>>();
    store.register_lints(&leaked_lints);
    store.register_group(
        false,
        "all",
        None,
        leaked_lints.into_iter().map(LintId::of).collect(),
    );

    store.register_late_pass(move || {
        Box::new(LintWrapper(*unsafe {
            let lint_impl = lib.get::<LintImpl>(b"get_lint_impl").unwrap()();
            Box::from_raw(lint_impl)
        }))
    });
}

struct LintWrapper(Box<dyn stable::LateLintImpl>);

impl LintPass for LintWrapper {
    fn name(&self) -> &'static str { "Lint" }
}

impl<'tcx> LateLintPass<'tcx> for LintWrapper {
    fn check_item(&mut self, cx: &LateContext<'tcx>, item: &'tcx Item<'tcx>) {
        let context = &Ctxt(cx) as &dyn stable::Context;
        self.0.check_item(context, item.as_stable(context));
    }

    fn check_ty(&mut self, cx: &LateContext<'tcx>, ty: &'tcx Ty<'tcx>) {
        let context = &Ctxt(cx) as &dyn stable::Context;
        self.0.check_ty(context, ty.as_stable(context));
    }
}

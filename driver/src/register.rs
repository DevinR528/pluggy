use std::path::PathBuf;

use libloading::{library_filename, Library};
use pluggy_api as stable;
use rustc_ast::{CrateSugar, FloatTy, IntTy, UintTy};
use rustc_hir::{
    def::{DefKind, Res},
    def_id::DefId,
    BodyId, Crate, GenericArgs, HirId, Item, ItemId, ItemKind, Path, PathSegment,
    PolyTraitRef, PrimTy, TraitBoundModifier, Ty, TyKind, UseKind, VisibilityKind,
};
use rustc_lint::{
    FutureIncompatibleInfo, LateContext, LateLintPass, Level, Lint, LintContext, LintId,
    LintPass, LintStore,
};
use rustc_lint_defs::FutureIncompatibilityReason;
use rustc_session::{declare_tool_lint, Session};
use rustc_span::{def_id::LocalDefId, edition::Edition, Pos, Span, Symbol};

type LintFunc = for<'a> fn(&'a dyn stable::Context, stable::Node);

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
    fn name(&self) -> &'static str { "hello fool" }
    fn warn(&self, msg: &str, lint: stable::lint::Lint) {
        self.0.lint(Box::leak(box lint.as_rustc()), |diag| {
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

impl<'a> IntoStable for Item<'a> {
    type Out = stable::Node;
    fn as_stable(&self, cx: &dyn stable::Context) -> Self::Out {
        let item = stable::ItemBuilder {
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
                // ItemKind::Fn(_, _, _) => todo,
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
        };
        stable::Node::Item(item.into_with_ctx(cx))
    }
}

impl<'a> IntoStable for Ty<'a> {
    type Out = stable::Node;
    fn as_stable(&self, _: &dyn stable::Context) -> Self::Out {
        stable::Node::Ty(todo!())
    }
}

pub struct FooLint {
    lints: Library,
}

declare_tool_lint! {
    pub linter::REG, Allow, "lint stuff", true
}

impl LintPass for FooLint {
    fn name(&self) -> &'static str { "Lint" }
}

impl FooLint {
    pub fn get_lints() -> Vec<&'static Lint> { vec![REG] }
}

impl<'tcx> LateLintPass<'tcx> for FooLint {
    fn check_item(&mut self, cx: &LateContext<'tcx>, item: &'tcx Item<'tcx>) {
        if matches!(item.kind, ItemKind::ExternCrate(_) | ItemKind::Use(_, _)) {
            let call = unsafe { *self.lints.get::<LintFunc>(b"lint_plugin").unwrap() };

            let context = &Ctxt(cx) as &dyn stable::Context;
            call(context, item.as_stable(context));
        }
    }

    fn check_ty(&mut self, cx: &LateContext<'tcx>, ty: &'tcx Ty<'tcx>) {
        let call = unsafe { *self.lints.get::<LintFunc>(b"lint_plugin").unwrap() };

        let context = &Ctxt(cx) as &dyn stable::Context;
        call(context, ty.as_stable(context));
    }
}

pub fn fetch_lint() -> Library {
    println!("FETCH_LINT");
    let lib_name = library_filename("test_lint");
    let mut path: PathBuf = "./target/debug/deps/".into();
    path.push(lib_name);

    unsafe { Library::new(path) }.unwrap()
}

pub fn register_lints(_sess: &Session, store: &mut LintStore) {
    store.register_lints(&[REG]);
    store.register_group(false, "all", None, vec![LintId::of(REG)]);
    store.register_late_pass(move || box FooLint { lints: fetch_lint() });
}

use pluggy_api::{
    declare_tool_lint, impl_lint_pass, Context, Item, ItemKind, LateLintImpl,
};

declare_tool_lint! {
    /// Stuff and things.
    pub LINT_PLUGIN,
    Warn,
    "test that lint plugin works"
}

declare_tool_lint! {
    /// We all need more generics.
    pub MORE_GENERICS,
    Warn,
    "give me more generics"
}

#[derive(Clone, Copy, Default)]
pub struct LintTest(());

impl_lint_pass! {
    LintTest::default => [LINT_PLUGIN]
}

impl LateLintImpl for LintTest {
    fn check_item(&mut self, cx: &dyn Context, item: Item) {
        match item.kind {
            ItemKind::ExternCrate(_) => {}
            ItemKind::Use(p, _k) => cx.warn(
                &format!(
                    "{:?}",
                    p.segments
                        .iter()
                        .map(|s| s.ident.name.to_string())
                        .collect::<Vec<_>>()
                ),
                LINT_PLUGIN,
            ),
            ItemKind::Static(_, _, _) => todo!(),
            ItemKind::Const(_, _) => todo!(),
            ItemKind::Fn(sig, gen, _body) => {
                if gen.params.is_empty() {
                    cx.warn_span("no generics found, add some", MORE_GENERICS, sig.span)
                }
            }
            ItemKind::Mod(_) => todo!(),
            ItemKind::TyAlias(_, _) => todo!(),
            ItemKind::LetTheDocsBeValid => todo!(),
            _ => {}
        }
    }
}

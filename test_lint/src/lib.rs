use pluggy_api::{lint::Lint, Context, ItemKind, Node};

#[no_mangle]
pub fn lint_plugin(cx: &dyn Context, node: Node) {
    if let Node::Item(item) = node {
        match item.kind {
            ItemKind::ExternCrate(sym) => {
                cx.warn(&format!("{:?}", sym), Lint::new("test_lint", "hello from dylib"))
            }
            ItemKind::Use(p, _k) => cx.warn(
                &format!(
                    "{:?}",
                    p.segments
                        .iter()
                        .map(|s| s.ident.name.to_string())
                        .collect::<Vec<_>>()
                ),
                Lint::new("test_lint", "hello from dylib"),
            ),
            ItemKind::Static(_, _, _) => todo!(),
            ItemKind::Const(_, _) => todo!(),
            ItemKind::Fn(_, _, _) => todo!(),
            ItemKind::Mod(_) => todo!(),
            ItemKind::TyAlias(_, _) => todo!(),
            ItemKind::LetTheDocsBeValid => todo!(),
            _ => {}
        }
    }
}

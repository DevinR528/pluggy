## My thinking so far

Use the lint driver to convert from rustc types to our stable types and call the lints via `libloading::Library`. Currently, the set up is as follows

- [Context trait object](https://github.com/DevinR528/pluggy/blob/b6be59349051fb80037c01ef1c780f8eac6c918b/driver/src/register.rs#L45) for all querying needs
- [wrapped hir types](https://github.com/DevinR528/pluggy/blob/b6be59349051fb80037c01ef1c780f8eac6c918b/pluggy_api/src/hir_def.rs)
- [a bunch of conversions](https://github.com/DevinR528/pluggy/blob/b6be59349051fb80037c01ef1c780f8eac6c918b/driver/src/register.rs#L33-L41)
- [register the lints](https://github.com/DevinR528/pluggy/blob/b6be59349051fb80037c01ef1c780f8eac6c918b/driver/src/register.rs#L761) and call the lints [when needed](https://github.com/DevinR528/pluggy/blob/b6be59349051fb80037c01ef1c780f8eac6c918b/driver/src/register.rs#L797)
- [write a lint](https://github.com/DevinR528/pluggy/blob/b6be59349051fb80037c01ef1c780f8eac6c918b/test_lint/src/lib.rs) similar to how writing lints for Clippy works.
- [macro generates extern fns](https://github.com/DevinR528/pluggy/blob/b6be59349051fb80037c01ef1c780f8eac6c918b/pluggy_api/src/lib.rs#L76)

I have marked all the enums `non_exhaustive` and tried [this](https://github.com/DevinR528/pluggy/blob/b6be59349051fb80037c01ef1c780f8eac6c918b/pluggy_api/src/hir_def.rs#L1088-L1107) for structs but I'm not sure what the best way to go about it is.

## 🤷

## My thinking so far

Use the lint driver to convert from rustc types to our stable types and call the lints via `libloading::Library`. Currently, the set up is as follows

[Context trait object]() for all querying needs
[wrapped hir types](https://github.com/DevinR528/pluggy/blob/main/pluggy_api/src/hir_def.rs)
[a bunch of conversions]()
[register the driver]() and call the lints [when needed]()

I have marked all the enums `non_exhaustive` and tried [this]() for structs but I'm not sure what the best way to go about it is.

## 🤷

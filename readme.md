## My thinking so far

Use the lint driver to convert from rustc types to our stable types and call the lints via `libloading::Library`. Currently, the set up is as follows

- [Context trait object](https://github.com/DevinR528/pluggy/blob/482035a7d328047788a00c8386b9dc948e3057ed/driver/src/register.rs#L34) for all querying needs
- [wrapped hir types](https://github.com/DevinR528/pluggy/blob/482035a7d328047788a00c8386b9dc948e3057ed/pluggy_api/src/hir_def.rs)
- [a bunch of conversions](https://github.com/DevinR528/pluggy/blob/482035a7d328047788a00c8386b9dc948e3057ed/driver/src/register.rs#L22-L30)
- [register the driver](https://github.com/DevinR528/pluggy/blob/482035a7d328047788a00c8386b9dc948e3057ed/driver/src/register.rs#L428) and call the lints [when needed](https://github.com/DevinR528/pluggy/blob/482035a7d328047788a00c8386b9dc948e3057ed/driver/src/register.rs#L404)

I have marked all the enums `non_exhaustive` and tried [this](https://github.com/DevinR528/pluggy/blob/482035a7d328047788a00c8386b9dc948e3057ed/pluggy_api/src/hir_def.rs#L872-L903) for structs but I'm not sure what the best way to go about it is.

## 🤷
https://github.com/DevinR528/pluggy/blob//pluggy_api/src/hir_def.rs#L23

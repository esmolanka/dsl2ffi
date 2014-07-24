# dsl2ffi

It was implemented in 20 minutes and just for fun.

There is toy AST which compiles to C and gets `foreign import`ed back
to Haskell module. All in Template Haskell.

Run `make` to build.

Slightly more real-world-ish example would use real prety printing library,
some clever monadic syntax as an eDSL, type checking, some code transformations,
more clever interface for generating C files, so user can define more than
one "DSL2FFI" function. But most likely that would never happen in this toy example.

For something interesting in this area (though it's not a DSL) see
https://hackage.haskell.org/package/language-c-inline

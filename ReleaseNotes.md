### version x.x.x (future release)


### version 25.05.16

**Breaking changes**

- Using ftype for `define-foreign-struct` and `define-foreign-union` on Chez
  This means, `alignment` keyword doesn't work on Chez.
- Introducing `struct` keyword on struct member to distinguish primitive
  and struct.
- Foreign types are now wrapped or ftype (on Chez), instead of symbols.

**New features**

- `define-type-alias` is introduced, similar usage as `typedef` in C.
- `boolean` support for Scheme boolean.
- Supporting array foreign variable.
- Supporting `(* type)` pointer form for foreign variable.
- Empty struct, i.e. `(define-foreign-struct foo)`, is supported
- Supporting `wchar_t` as Scheme character.

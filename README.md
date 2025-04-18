R6RS Portable Foreign Function Interface
========================================

PFFI is a portable foreign function interface for R6RS Scheme implementations.

## Changes

### version x.x.x (next release)

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
- Empty struct, i.e. `(define-foreign-struct foo)`, is supported

## Example

Suppose we have the following C file and will be compiled to `libfoo.so`

```c
int add(int a, int b)
{
  return a + b;
}
```

Now we want to use the above shared object from Scheme.

```scheme
#!r6rs
(import (rnrs) (pffi))

(define shared-object (open-shared-object "libfoo.so"))

(define foo (foreign-procedure shared-object int add (int int)))

(foo 1 2) ;; => 3

```

See `examples/` directory for more examples.

## APIs

### Foreign procedures and variables

This layer of the APIs wraps implementations specific foreign object
accessing.


#### [Procedure] `open-shared-object` _shared-object-name_

Returns shared object.

#### [Macro] `foreign-procedure` _shared-object_ _return-type_ _symbol-name_ (_types_ ...)
#### [Macro] `foreign-procedure` _shared-object_ (_conv_ ...) _return-type_ _symbol-name_ (_types_ ...)

Lookup foreign procedure _symbol-name_ from given _shared-object_ and returns
foreign procedure. A foreign procedure is a mere procedure so users can just
call as if it's a Scheme procedure.

If the second form is used, then _conv_ must be implementation specific 
calling conventions. For example `__cdecl` for Chez Scheme.

#### [Macro] `c-callback` _return-type_ (_types_ ...) _proc_

Creates a callback. Callback is a mechanism that makes foreign procedure
call Scheme procedure. The given _proc_ is the procedure called from
foreign procedure.

#### [Procedure] `free-c-callback` _callback_

Release allocated callback if needed.

Callback object may not be released automatically so it is user's responsibilty
to make sure to release it.

#### [Macro] `define-foreign-variable` _shared-object_ _type_ _symbol-name_ [_scheme-name_]
#### [Macro] `define-foreign-variable` _shared-object_ (array _type_) _symbol-name_ [_scheme-name_]

Lookup foreign variable _symbol-name_ from given _shared-object_ and binds it
to _scheme-name_. If _scheme-name_ is not given, then it is generated from
_symbol-name_ with following rules:

- converting to lower case
- converting `_` to `-`

_type_ must be a type which has `pointer-ref-c-*` and `pointer-set-c-*` 
procedure.

The bound variable is settable, thus `set!` syntax can change the value
if it's allowed.

If the second form is used, then the it creates an reference to an array
pointer, and the _scheme-name_ will be a macro of 3 patterns:

`_scheme-name_`: to return the raw pointer of the array.  
`(_scheme-name_ n)`: to refer the `n`th element of the array.  
`(set! _scheme-name_ (n v))`: to set `v` to the `n`th element of the array.

#### [Macro] `define-type-alias` _name_ _alias_
#### [Macro] `define-type-alias` _name_ (* _alias_)

Defines a type alias. Similar mechanism as `typedef` in C.

If the second form is used, then _name_ will be an alias of the `pointer`.

### Foreign types

Implementations may have own bindings for foreign types. This layer absorbs
the difference. Currently following types are supported.

- `char`
- `unsigned-char`
- `short`
- `unsigned-short`
- `int`
- `unsigned-int`
- `long`
- `unsigned-long`
- `float`
- `double`
- `int8_t`
- `uint8_t`
- `int16_t`
- `uint16_t`
- `int32_t`
- `uint32_t`
- `int64_t`
- `uint64_t`
- `pointer`
- `boolean`
- `void`
- `callback`

Above types are all variable except `callback`. Callback is a procedure
which is called from foreign world to Scheme world. Thus it may need to
have foreign types.

#### Foreign type size

The foreign type size can be retrieved from the variable, whose name
is `size-of-{type}`, e.g. `size-of-char`, except `void` and `callback`.

NOTE: `size-of-boolean` may differs depending on the implementation.
It can be either, 1 or 4.

#### Variadic arguments

C's variadic arguments (i.e. argument specified by `...`) can be written by
specifying `___`. For example, suppose we have `sum` C function which takes
an int as the number of the variadic arguments, and variadic arguments.
To specify this, you can write like this

```scheme
(foreign-procedure lib int sum (int ___))
```

The `___` must be the last and must not appear more than once.

### Pointer operations

#### [Procedure] `pointer?` _o_

Returns #t if given _o_ is a pointer object.

#### [Procedure] `null-pointer?` _pointer_

Returns #t if given _pointer_ value is 0.

#### [Procedure] `bytevector->pointer` _bv_

Converts given bytevector _bv_ to implementation dependent pointer object.

#### [Procedure] `pointer->bytevector` _p_ _len_

Converts given pointer to bytevector whose length is _len_ and elements are
derefered values of the pointer _p_.

#### [Procedure] `integer->pointer` _i_

Converts given integer _i_ to a pointer object. The given integer _i_ is
the address of returning pointer.

#### [Procedure] `pointer->integer` _p_

Converts given pointer _p_ to an integer. The returning integer represents
the address of the pointer _p_.


#### [Procedure] `pointer-ref-c-${type}` _p_ _offset_

_${type}_ must be one of the following types:

- `uint8`
- `int8`
- `uint16`
- `int16`
- `uint32`
- `int32`
- `uint64`
- `int64`
- `unsigned-char`
- `char`
- `unsigned-short`
- `short`
- `unsigned-int`
- `int`
- `unsigned-long`
- `long`
- `float`
- `double`
- `pointer`

Returns corresponding type value form give pointer _p_. The _offset_ is
byte offset of the given _p_ not aligned value.

#### [Procedure] `pointer-set-c-${type}!` _p_ _offset_ _value_

_${type}_ must be the same as `pointer-ref-c-${type}`.

Sets given _value_ which is converted to corresponding type to pointer _p_
on _offset_ location. _offset_ is byte offset of the given _p_.

### Foreign structure

#### [Macro] `define-foreign-struct` _name_ _spec ..._
#### [Macro] `define-foreign-struct` (_name_ _ctr_ _pred_) _spec ..._

Defines a structure. The macro creates constructor, predicate, size-of
variable and accessors.

_ctr_ is the constructor which returns newly allocated bytevector whose
size is the size of this struct.

_pred_ is the predicate, which simply check if the givn object is a
bytevector and it has enough size for this structure. It doesn't distinguish
2 bytevectors created by 2 different ways as long as it has enough size.

Size-of variable is created adding `size-of-` prefix to _name_. This
variable contains the size of this structure.

_spec_ can be one of the followings:

- (`fields` _field spec ..._)
- (`protocol` _proc_)
- (`parent` _parent-structure_)
- (`alignment` _alignment_)

The same clause can only appear once. If there are more than one the same
clause, it raises `&syntax`.

_field spec_ can be one the followings:

- (`fields` (_type_ _field_))
- (`fields` (_type_ _field_ _getter_))
- (`fields` (_type_ _field_ _getter_ _setter_))

_type_ must be a type listed in _Foreign types_ section except `callback`,
or `(struct _struct-name_)` for foreigin struct.

_field_ is the field name. This is used for generating _getter_ and _setter_.
In other words, it doesn't have to be meaningful name as long as _getter_
and _setter_ is specified.

_getter_ is an accessor to retrieve the structure field value. If this is not
specified, then it is created by adding `_name_-` prefix to _field_.

_setter_ is an accessor to set the structure field value. If this is not
specified, then it is created by adding `_name_-` prefix and `-set!` suffix
to  _field_.

_proc_ is a procedure which is the same usage as `define-record-type`'s one.

_parent-structure_ must be a foreign structure defined by this macro. There
is no actual hierarchy but just putting specified structure in front of
this structure so that it seems it has a hierarchy. For example:

_alignment_ must be an integer or integer variable of `1`, `2`, `4`, `8`
or `16`. This specifies the struct alignment size. This is equivalent of
`#pragma pack(n)`.

```scheme
(define-foreign-struct p
  (fields (int count)))

(define-foreign-struct c
  (fields (pointer elements))
  (parent p))

(make-c 0 (integer->pointer 0))
;; 32 bits -> #vu8(0 0 0 0 0 0 0 0)
;; 64 bits -> #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
```

is the same as the following

```scheme
(define-foreign-struct p
  (fields (int count)))

(define-foreign-struct c
  (fields (p p)
          (pointer elements)))

(make-c (make-p 0) (integer->pointer 0))
;; 32 bits -> #vu8(0 0 0 0 0 0 0 0)
;; 64 bits -> #vu8(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
```

If the first form is used, then _ctr_ and _pred_ are created by adding `make-`
prefix and `?` suffix respectively, like `define-record-type`.

#### [Macro] `define-foreign-union` _name_ _spec ..._

Defines a union structure. The macro creates constructor, predicate, size-of
variable and accessors. The auto generating convension is the same as
`define-foreign-struct` unless its specified.

The _spec_ can be one of the followings:

- (`fields` _field spec ..._)
- (`protocol` _proc_)

The `fields` is the same as `define-foreign-struct`.

The _proc_ of `protocol` should look like this:

```scheme
(lambda (p)
  (lamba (f)
    (p 'f f)))
```
The _p_ takes 0 or 2 arguments. If the first form is used, then it creates
0 padded bytevector. If the second form is used, then it searches the
field setter named *f* and sets the value _f_. If it's not found, then
it behave as if nothing is passed.


## Supporting implementations

- Sagittarius (0.9.12 or later)
- Racket (plt-r6rs v8.16 or later)
- Guile (3.0.10 or later)
- Chez Scheme (v10.0.0 or later)

The below implementations are no loger supported due to the inactiveness or
officially declared to be archived.

- ~~Larceny (v0.98)~~
- ~~Vicare (0.3d7)~~
- ~~Mosh (0.2.7)~~


## Limitation per implementations

### Vicare

Vicare doesn't support bytevector to pointer convertion whom converted
pointer is shared with source bytevector. So this behaviour is emulated
on this library. This emulation doesn't work on NULL pointer. So the
following situation doesn't work:
Suppose a shared object set a pointer value to NULL, then initialise it
on a function. Scheme code first loads the pointer, then call the
initialisation function, however the loaded pointer still indicates NULL.

### Larceny

On Larceny, GC may move pointers so converting bytevector uses wrapper
technique the same as Vicare. Thus the same limitation is applied to it.

## Misc (Memo)

### Why no Ypsilon

The latest released version of Ypsilon has very limited FFI interface.
The biggest problem is `c-function` is defined as a macro which I think
very limited.

Trunk repository version has far more APIs but it's not released nor
maintained. Thus it is hard for me to make portable layer for it.

### Why no IronScheme

.Net makes a bit things harder. And its FFI support is very limited. (e.g.
it doesn't work on Mono)

To support above non supported implementations, your pull request is
the fastest way :)

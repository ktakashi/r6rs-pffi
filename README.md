R6RS Portable Foreign Function Interface
========================================

PFFI is a portable foreign function interface for R6RS Scheme implementations.

## Example

Suppose we have the following C file and will be compiled to `libfoo.so`

```
int add(int a, int b)
{
  return a + b;
}
```

Now we want to use the above shared object from Scheme.

```
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

Lookup foreign procedure _symbol-name_ from given _shared-object_ and returns
foreign procedure. A foreign procedure is a mere procedure so users can just
call as if it's a Scheme procedure.

#### [Macro] `c-callback` _return-type_ (_types_ ...) _proc_
Creates a callback. Callback is a mechanism that makes foreign procedure
call Scheme procedure. The given _proc_ is the procedure called from
foreign procedure.

#### [Procedure] `free-c-callback` _callback_
Release allocated callback if needed.

Callback object may not be released automatically so it is user's responsibilty
to make sure to release it.

#### [Macro] `define-foreign-variable` _shared-object_ _type_ _symbol-name_ [_scheme-name_]

Lookup foreign variable _symbol-name_ from given _shared-object_ and binds it
to _scheme-name_. If _scheme-name_ is not given, then it is generated from
_symbol-name_ with following rules:

- converting to lower case
- converting `_` to `-`

_type_ must be specified properly. Currently _type_ can only be numerical.

The bound variable is settable, thus `set!` syntax can change the value
if it's allowed.


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
- `void`
- `callback`

Above types are all variable except `callback`. Callback is a procedure
which is called from foreign world to Scheme world. Thus it may need to
have foreign types.

### Pointer operations

#### [Procedure] `pointer?` _o_

Returns #t if given _o_ is a pointer object.

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

#### [Macro] `define-foreign-struct` _name_ _specs ..._
#### [Macro] `define-foreign-struct` (_name_ _ctr_ _pred_) _spec ..._

Defines structure. The macro creates constructor, predicate, size-of 
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
- (`parent` _parent-structure-)

The same clause can only appear once. If there are more than one the same
clause, it raises `&syntax`.

_field spec_ can be one the followings:

- (`fields` (_type_ _field_))
- (`fields` (_type_ _field_ _getter_))
- (`fields` (_type_ _field_ _getter_ _setter_))

_type_ must be a type listed in _Foreign types_ section except `callback`.

_field_ is the field name. This is used for generating _getter_ and _setter_.
In other words, it doesn't have to be meaningful name as long as _getter_
and _setter_ is specified.

_getter_ is an accessor to retrieve the structure field value. If this is not
specified, then it is created by adding `_name_-` prefix to _field_.

_setter_ is an accessor to set the structure field value. If this is not
specified, then it is created by adding `_name_-` prefix and `-set!` suffix
to  _field_.

The first form is creates _ctr_ and _pred_ adding `make-`
prefix and `?` suffix respectively, like `define-record-type`. 


## Supporting implementations

- Sagittarius (0.6.4)
- Vicare (0.3d7)
- Mosh (0.2.7 only NMosh)
- Racket (plt-r6rs 6.1.1)
- Guile (2.0.11)


## Limitation per implementations

### Mosh

Mosh has 2 flavours of macro expansion style, psyntax and SRFI-72. The
first one doesn't expose `bytevector-pointer` so currently this library
can't support it. Only SRFI-72 flavoured one, called NMosh, is supported.

### Sagittarius

32 bit version of Sagittarius (0.6.4) has a bug on `uinteger->bytevector`.
So if a pointer address is greater than 0x7FFFFFFF, then it doesn't work
properly.


## Misc (Memo)

### Why no Chez?

I don't have Chez Scheme but Petite Chez Scheme. And Petite doesn't have
FFI available (it's clearly said on the Manual).

### Why no Ypsilon

The latest released version of Ypsilon has very limited FFI interface.
The biggest problem is `c-function` is defined as a macro which I think
very limited.

Trunk repository version has far more APIs but it's not released nor
maintained. Thus it is hard for me to make portable layer for it.


To support above non supported implementations, your pull request is
the fastest way :)

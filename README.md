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

See `example/` directory for more examples.

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

#### Pointer mutations

TBD


## Supporting implementations

- Sagittarius (0.6.4)
- Vicare (0.3d7)
- Mosh (0.2.7)
- Racket (plt-r6rs 6.1.1)
- Guile (2.0.11)

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

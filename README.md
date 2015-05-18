R6RS Portable Foreign Function Interface
========================================

PFFI is a portable foreign function interface for R6RS Scheme implementations.


## APIs


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


### Primitive types

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

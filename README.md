R6RS Portable Foreign Function Interface
========================================

PFFI is a portable foreign function interface for R6RS Scheme implementations.


## APIs


[Procedure] `open-shared-object` _shared-object-name_
------------------------------------------------------
Returns shared object.

[Macro] `foreign-procedure` _shared-object_ _return-type_ _symbol-name_ (_types_ ...)
-------------------------------------------------------------------------------------
Lookup foreign procedure _symbol-name_ from given _shared-object_ and returns
foreign procedure. A foreign procedure is a mere procedure so users can just
call as if it's a Scheme procedure.

[Macro] `c-callback` _return-type_ (_types ...) _proc_
-------------------------------------------------------
Creates a callback. Callback is a mechanism that makes foreign procedure
call Scheme procedure. The given _proc_ is the procedure called from
foreign procedure.

### Primitive types

TBD


## Supported implementations

- Sagittarius (0.6.4)
- Vicare (0.3d7)

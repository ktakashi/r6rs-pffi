(library (types)
    (export ppp)
    (import (only (pffi) define-type-alias pointer))
;; limitation of Chez...
(define-type-alias ppp pointer)
)

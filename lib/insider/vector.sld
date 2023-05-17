(define-library (insider vector)
  (import (insider syntax) (insider basic-procedures) (insider numeric)
          (insider list) (insider control)
          (only (insider internal)
                vector list->vector vector->list vector-append vector->list
                vector-ref vector-set! make-vector vector-length))
  (export vector make-vector list->vector vector->list vector-append
          vector-length vector-ref vector-set! vector? vector-tabulate
          vector-copy vector-copy! vector->list vector-fill! vector-for-each
          vector-map)
  (include "vector.scm"))

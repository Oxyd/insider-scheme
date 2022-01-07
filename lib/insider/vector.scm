(library (insider vector))
(import (insider syntax) (insider basic-procedures)
        (except (insider internal) define let))
(export vector make-vector list->vector vector->list vector-append vector-length vector-ref vector-set!
        vector-tabulate)

(define (vector-tabulate proc len)
  (do ((i 0 (+ i 1))
       (result (make-vector len)))
      ((= i len) result)
    (vector-set! result i (proc i))))

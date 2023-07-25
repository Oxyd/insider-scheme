(define-library (insider struct)
  (import (scheme base) (insider syntax) (scheme write) (insider string)
          (insider basic-procedures)
          (only (insider internal)
                make-record-type make-record-instance record-set! record-ref
                record-type))
  (import (insider format)) ;; debug
  (export struct)
  (include "struct.scm"))

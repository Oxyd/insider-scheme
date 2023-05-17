(define-library (insider record)
  (import (insider syntax) (insider error) (insider list)
          (insider basic-procedures) (insider numeric)
          (only (insider internal)
                eq? type make-record-type make-record-instance record-set!
                record-ref record-type meta))
  (export define-record-type)
  (include "record.scm"))

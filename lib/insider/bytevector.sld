(define-library (insider bytevector)
  (import (insider syntax) (insider basic-procedures) (insider list)
          (insider numeric)
          (only (insider internal)
                bytevector bytevector-length bytevector-u8-ref
                bytevector-u8-set! make-bytevector))
  (export bytevector? make-bytevector bytevector bytevector-length
          bytevector-u8-ref bytevector-u8-set! bytevector-copy bytevector-copy!
          bytevector-append)
  (include "bytevector.scm"))

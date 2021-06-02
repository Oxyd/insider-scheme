(library (insider control))
(import (insider base-scheme)
        (only (insider internal) capture-stack replace-stack!))
(export call-with-current-continuation call/cc)

(define (call-with-current-continuation f)
  (capture-stack
   (lambda (stack)
     (f (lambda (value)
          (replace-stack! stack value))))))

(define call/cc call-with-current-continuation)

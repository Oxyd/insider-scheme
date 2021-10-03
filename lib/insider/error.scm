(library (insider error))
(import (insider base-scheme)
        (only (insider internal)
              dynamic-wind with-exception-handler raise raise-continuable
              make-error error-message error-irritants uncaught-exception-inner-exception))
(export dynamic-wind with-exception-handler raise raise-continuable
        error error-object-message error-object-irritants error?)

(define (error message . irritants)
  (raise (make-error message irritants)))

(define (error? x)
  (and (memq (type x) '(insider::error insider::uncaught_exception insider::cxx_exception))
       #t))

(define (error-object-message e)
  (case (type e)
    ((insider::error)
     (error-message e))
    ((insider::uncaught_exception)
     "Unhandled exception")
    (else
     (error "Not an error object" e))))

(define (error-object-irritants e)
  (case (type e)
    ((insider::error)
     (error-irritants e))
    ((insider::uncaught_exception)
     (list (uncaught-exception-inner-exception e)))
    (else
     (error "Not an error object" e))))

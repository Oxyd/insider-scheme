(define (error message . irritants)
  (raise (make-error message irritants)))

(define (error-object? x)
  (and (memq (type x)
             '(insider::error
               insider::uncaught_exception
               insider::file_error
               insider::read_error::scheme_error
               insider::cxx_exception))
       #t))

(define (file-error? e)
  (eq? (type e) 'insider::file_error))

(define (read-error? e)
  (eq? (type e) 'insider::read_error::scheme_error))

(define (error-object-message e)
  (case (type e)
    ((insider::error)
     (error-message e))
    ((insider::uncaught_exception)
     "Unhandled exception")
    ((insider::file_error)
     (file-error-message e))
    ((insider::read_error::scheme_error)
     (read-error-message e))
    ((insider::cxx_exception)
     (cxx-exception-message e))
    (else
     (error "Not an error object" e))))

(define (error-object-irritants e)
  (case (type e)
    ((insider::error)
     (error-irritants e))
    ((insider::uncaught_exception)
     (list (uncaught-exception-inner-exception e)))
    ((insider::file_error insider::read_error::scheme_error)
     '())
    (else
     (error "Not an error object" e))))

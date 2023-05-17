(define-library (insider error)
  (import (insider syntax) (insider basic-procedures) (insider list)
          (only (insider internal)
                dynamic-wind with-exception-handler raise raise-continuable
                make-error error-message error-irritants
                uncaught-exception-inner-exception file-error-message
                read-error-message cxx-exception-message))
  (export dynamic-wind with-exception-handler raise raise-continuable
          error error-object-message error-object-irritants error-object?
          file-error? read-error?)
  (include "error.scm"))

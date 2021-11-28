(library (scheme case-lambda))
(import (insider syntax) (insider basic-procedures) (insider error) (insider control) (insider list))
(export case-lambda)

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (params0 body0 ...) ...)
     (lambda args
       (let ((len (length args)))
         (letrec-syntax
             ((clause (syntax-rules --- ()
                        ((clause)
                         (error "case-lambda: No matching clause"))

                        ((clause ((params ---) . body) . rest)
                         (if (= len (length '(params ---)))
                             (apply (lambda (params ---) . body) args)
                             (clause . rest)))

                        ((clause ((params --- . tail) . body) . rest)
                         (if (>= len (length '(params ---)))
                             (apply (lambda (params --- . tail) . body) args)
                             (clause . rest))))))

           (clause (params0 body0 ...) ...)))))))

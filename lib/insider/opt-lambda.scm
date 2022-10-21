(library (insider opt-lambda))
(import (insider syntax) (insider list) (insider numeric) (insider error) (insider control))
(export opt-lambda)

(define-syntax make-opt-lambda
  (syntax-rules ()
    ((make-opt-lambda (required ...) (optional ...) . lambda-body)
     (lambda (required ... . tail)
       (let ((len (length tail)))
         (if (> len (length '(optional ...)))
             (error "opt-lambda: Too many arguments")
             (letrec-syntax
                 ((clause (syntax-rules --- ()
                            ((clause () ((opt-name opt-default) ---) . body)
                             (let* ((opt-name opt-default) ---) . body))

                            ((clause ((provided-names provided-defaults) --- (provided-name provided-default))
                                     ((opt-name opt-default) ---)
                                     . body)
                             (if (= len (length '(provided-names --- provided-name)))
                                 (apply (lambda (provided-names --- provided-name)
                                          (let* ((opt-name opt-default) ---) . body))
                                        tail)
                                 (clause ((provided-names provided-defaults) ---)
                                         ((provided-name provided-default) (opt-name opt-default) ---)
                                         . body))))))

               (clause (optional ...) () . lambda-body))))))))

(define-syntax collect-args
  (syntax-rules ()
    ((collect-args () required optional . body)
     (make-opt-lambda required optional . body))

    ((collect-args ((name default) rest ...) required (optionals ...) . body)
     (collect-args (rest ...) required (optionals ... (name default)) . body))

    ((collect-args (name rest ...) (required ...) () . body)
     (collect-args (rest ...) (required ... name) () . body))

    ((collect-args (name rest ...) (required ...) (optional optionals ...) . body)
     (syntax-error "opt-lambda: Required argument after optional"))))

(define-syntax opt-lambda
  (syntax-rules ()
    ((opt-lambda args . body)
     (collect-args args () () . body))))

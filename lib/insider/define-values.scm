(define-type-predicate values-tuple? insider::values_tuple)

(define (values-length t)
  (if (values-tuple? t)
      (values-tuple-length t)
      1))

(define-syntax define-values
  (lambda (stx)
    (define (emit-define-tuple tuple expr expected-length length-predicate)
      #`(define #,tuple
          (let ((t #,expr))
            (let ((actual-length (values-length t)))
              (unless (#,length-predicate #,expected-length actual-length)
                (error "Expression evaluated to wrong number of values"))
              t))))

    (define (emit-values-definitions tuple names)
      (let loop ((index 0) (names names) (accum '()))
        (if (null? names)
            accum
            (loop (+ index 1)
                  (cdr names)
                  (cons #`(define #,(car names) (values-tuple-ref #,tuple #,index))
                        accum)))))

    (syntax-match stx ()
      ((_ (name) expr)
       #`(begin
           (define #,name
             (let ((value #,expr))
               (when (values-tuple? value)
                 (error "Expression evaluated to wrong number of values"))
               value))))

      ((_ (names ...) expr)
       (let ((expected-length (length names)))
         #`(begin
             #,(emit-define-tuple #'tuple expr expected-length =)
             #,@(emit-values-definitions #'tuple names))))

      ((_ (names ... . tail) expr)
       (let ((minimal-length (length names)))
         #`(begin
             #,(emit-define-tuple #'tuple expr minimal-length <=)
             #,@(emit-values-definitions #'tuple names)
             (define #,tail (list-tail (call-with-values (lambda () tuple) list) #,minimal-length))))))))

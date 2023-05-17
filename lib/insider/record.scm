(meta
  (define (make-field-indices fields)
    (let loop ((accum '()) (index 0) (fields fields))
      (if (null? fields)
          accum
          (syntax-match (car fields) ()
            ((name _ ...)
             (loop (cons (cons (syntax-expression name) index) accum)
                   (+ index 1)
                   (cdr fields))))))))

(meta
  (define (field-index name-stx indices)
    (let ((name (syntax-expression name-stx)))
      (cond ((assq name indices) => cdr)
            (else (error "Invalid field" name))))))

(meta
  (define (record-size fields)
    (length (syntax->list fields))))

(meta
  (define (make-record-constructor record-type constructor-stx field-indices)
    (syntax-match constructor-stx ()
      ((constructor-name constructor-fields ...)
       #`(define (#,constructor-name #,@constructor-fields)
           (let ((result (make-record-instance #,record-type)))
             #,@(let loop ((accum '()) (constructor-fields constructor-fields))
                  (if (null? constructor-fields)
                      (reverse accum)
                      (loop
                       (cons #`(record-set!
                                result
                                #,(field-index (car constructor-fields)
                                               field-indices)
                                #,(car constructor-fields))
                             accum)
                       (cdr constructor-fields))))
             result))))))

(meta
  (define (make-field-getter field-name getter-name indices)
    #`(define (#,getter-name x)
        (record-ref x #,(field-index field-name indices)))))

(meta
  (define (make-field-setter field-name setter-name indices)
    #`(define (#,setter-name x value)
        (record-set! x #,(field-index field-name indices) value))))

(meta
  (define (make-accessors-for-field field-stx indices)
    (syntax-match field-stx ()
      ((name getter)
       (make-field-getter name getter indices))

      ((name getter setter)
       #`(begin
           #,(make-field-getter name getter indices)
           #,(make-field-setter name setter indices))))))

(meta
  (define (make-field-accessors fields indices)
    #`#,@(let loop ((accum '()) (fields fields))
           (if (null? fields)
               (reverse accum)
               (loop (cons (make-accessors-for-field (car fields) indices)
                           accum)
                     (cdr fields))))))

(define-syntax define-record-type
  (lambda (stx)
    (syntax-match stx ()
      ((define-record-type name constructor predicate fields ...)
       (let ((indices (make-field-indices fields)))
         #`(begin
             (define #,name (make-record-type #,(record-size fields) '#,name))
             #,(make-record-constructor name constructor indices)
             (define (#,predicate x)
               (and (eq? (type x) 'insider::record_instance)
                    (eq? (record-type x) #,name)))
             #,@(make-field-accessors fields indices)))))))

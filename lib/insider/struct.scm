(meta
  (define (make-field name kw index has-default? default-value mutable?
                      getter-name setter-name)
    (vector name kw index has-default? default-value mutable?
            getter-name setter-name)))

(meta
  (define (field-name f)
    (vector-ref f 0)))

(meta
  (define (field-keyword f)
    (vector-ref f 1)))

(meta
  (define (field-index f)
    (vector-ref f 2)))

(meta
  (define (field-has-default? f)
    (vector-ref f 3)))

(meta
  (define (field-default-value f)
    (vector-ref f 4)))

(meta
  (define (field-mutable? f)
    (vector-ref f 5)))

(meta
  (define (field-getter-name f)
    (vector-ref f 6)))

(meta
  (define (field-setter-name f)
    (vector-ref f 7)))

(meta
  (define (id->string name)
    (symbol->string (syntax->datum name))))

(meta
  (define (string->id name s)
    (datum->syntax name (string->symbol s))))

(meta
  (define (default-struct-type-name name)
    (string->id name (string-append "<" (id->string name) ">"))))

(meta
  (define (default-struct-predicate-name name)
    (string->id name (string-append (id->string name)
                                    "?"))))

(meta
  (define (default-field-getter-name field-name struct-name)
    (string->id struct-name (string-append (id->string struct-name)
                                           "-"
                                           (symbol->string field-name)))))

(meta
  (define (default-field-setter-name field-name struct-name)
    (string->id struct-name (string-append (id->string struct-name)
                                           "-"
                                           (symbol->string field-name)
                                           "-set!"))))

(meta
  (define (parse-field-options field-name struct-name options)
    (let ((mutable? #f)
          (getter-name (default-field-getter-name field-name struct-name))
          (setter-name (default-field-setter-name field-name struct-name)))
      (let loop ((options options))
        (syntax-match options ()
          (()
           (values mutable? getter-name setter-name))
          ((#:mutable . rest)
           (set! mutable? #t)
           (loop rest))
          ((#:getter-name name . rest)
           (set! getter-name name)
           (loop rest))
          ((#:setter-name name . rest)
           (set! setter-name name)
           (loop rest))
          ((opt-name . _)
           (error "Invalid struct field option" (syntax->datum opt-name))))))))

(meta
  (define (make-field-record field kw index struct-name)
    (define (make name has-default? default-value options)
      (let ((field-name (syntax-expression name)))
        (let-values (((mutable? getter-name setter-name)
                      (parse-field-options field-name struct-name options)))
          (make-field field-name kw index has-default? default-value mutable?
                      getter-name setter-name))))

    (syntax-match field ()
      ((name default . opts)
       #:when (not (keyword? (syntax-expression default)))
       (make name #t default opts))
      ((name . opts)
       (make name #f #f opts))
      (name
       (make name #f #f '())))))

(meta
  (define (make-field-records struct-name fields)
    (let loop ((accum '()) (index 0) (kw #f) (fields fields))
      (cond ((null? fields)
             (reverse accum))
            ((keyword? (syntax-expression (car fields)))
             (loop accum index (syntax-expression (car fields)) (cdr fields)))
            (else
             (let ((f (make-field-record (car fields) kw index struct-name)))
               (loop (cons f accum)
                     (+ index 1)
                     #f
                     (cdr fields))))))))

(meta
  (define (field-name->keyword field)
    (string->keyword (symbol->string (field-name field)))))

(meta
  (define (make-parameter-for-field field)
    (let ((f (if (field-has-default? field)
                 (list (field-name field) (field-default-value field))
                 (field-name field))))
      (if (field-keyword field)
          (list (field-keyword field) f)
          (list f)))))

(meta
  (define (make-struct-constructor-parameters field-records)
    (let loop ((fields '()) (records field-records))
      (cond ((null? records)
             (apply append (reverse fields)))
            (else
             (let ((field (car records)))
               (loop (cons (make-parameter-for-field field) fields)
                     (cdr records))))))))

(meta
  (define (make-struct-constructor name type-name field-records)
    #`(define (#,name #,@(make-struct-constructor-parameters field-records))
        (let ((result (make-record-instance #,type-name)))
          #,@(let loop ((accum '()) (fields field-records))
               (cond
                ((null? fields)
                 (reverse accum))
                (else
                 (let ((field (car fields)))
                   (loop (cons #`(record-set! result
                                              #,(field-index field)
                                              #,(field-name field))
                               accum)
                         (cdr fields))))))
          result))))

(meta
  (define (make-field-getter field-record struct-name)
    #`(define (#,(field-getter-name field-record) x)
        (record-ref x #,(field-index field-record)))))

(meta
  (define (make-field-setter field-record struct-name)
    #`(define (#,(field-setter-name field-record) x value)
        (record-set! x #,(field-index field-record) value))))

(meta
  (define (make-field-accessors field-records struct-name)
    (let loop ((accum '()) (fields field-records))
      (if (null? fields)
          (reverse accum)
          (let ((field (car fields)))
            (let* ((accum* (cons (make-field-getter field struct-name)
                                 accum))
                   (accum** (if (field-mutable? field)
                                (cons (make-field-setter field struct-name)
                                      accum*)
                                accum*)))
              (loop accum** (cdr fields))))))))

(meta
  (define (parse-struct-options name options)
    (let ((constructor-name name)
          (type-name (default-struct-type-name name))
          (predicate-name (default-struct-predicate-name name)))
      (let loop ((options options))
        (syntax-match options ()
          (()
           (values constructor-name type-name predicate-name))
          ((#:constructor-name ctor-name . rest)
           (set! constructor-name ctor-name)
           (loop rest))
          ((#:type-name t-name . rest)
           (set! type-name t-name)
           (loop rest))
          ((#:predicate-name pred-name . rest)
           (set! predicate-name pred-name)
           (loop rest))
          ((opt-name . _)
           (error "Invalid struct option" (syntax->datum opt-name))))))))

(define-syntax struct
  (lambda (stx)
    (syntax-match stx ()
      ((_ name (fields ...) options ...)
       (let-values (((field-records) (make-field-records name fields))
                    ((constructor-name type-name predicate-name)
                     (parse-struct-options name options)))
         #`(begin
             (define #,type-name
               (make-record-type #,(length field-records)
                                 '#,name))
             #,(make-struct-constructor constructor-name type-name field-records)
             (define (#,predicate-name x)
               (and (eq? (type x) 'insider::record_instance)
                    (eq? (record-type x) #,type-name)))
             #,@(make-field-accessors field-records name)))))))

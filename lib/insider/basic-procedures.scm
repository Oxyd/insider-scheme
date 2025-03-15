(define-syntax define-type-predicate
  (syntax-rules ()
    ((define-type-predicate name expected-type)
     (define (name x)
       (eq? (type x) 'expected-type)))))

(define-type-predicate box? insider::box)
(define-type-predicate syntax? insider::syntax)
(define-type-predicate scheme-procedure? insider::procedure)
(define-type-predicate native-procedure? insider::native_procedure)
(define-type-predicate procedure-prototype? insider::procedure_prototype)
(define-type-predicate symbol? insider::symbol)
(define-type-predicate string? insider::string)
(define-type-predicate boolean? insider::boolean)
(define-type-predicate keyword? insider::keyword)
(define-type-predicate pair? insider::pair)
(define-type-predicate vector? insider::vector)
(define-type-predicate bytevector? insider::bytevector)
(define-type-predicate char? insider::character)

;;> @procedure
;;> @name[string?]
;;> @arg[s]
(define-type-predicate string? insider::string)

;;> @procedure
;;> @name[string-cursor?]
;;> @arg[c]
(define-type-predicate string-cursor? insider::string_cursor)

(define-type-predicate textual-input-port? insider::textual_input_port)
(define-type-predicate binary-input-port? insider::binary_input_port)
(define-type-predicate textual-output-port? insider::textual_output_port)
(define-type-predicate binary-output-port? insider::binary_output_port)

(define-type-predicate values-tuple? insider::values_tuple)

(define (procedure? x)
  (or (scheme-procedure? x) (native-procedure? x)))

(define (not x)
  (if x #f #t))

(define (all-eq? first second . rest)
  (and (eq? first second)
       (or (eq? rest '())
           (apply all-eq? second rest))))

(define boolean=? all-eq?)
(define symbol=? all-eq?)

(define-syntax define-type-predicate
  (syntax-rules ()
    ((define-type-predicate name expected-type)
     (define (name x)
       (eq? (type x) 'expected-type)))))

;;> @procedure
;;> @name[box?]
;;> @arg[x]
;;> @in-group[type-predicates]
;;>
;;> These procedures test whether the given parameter is of the indicated
;;> type.
(define-type-predicate box? insider::box)

;;> @procedure
;;> @name[syntax?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate syntax? insider::syntax)

;;> @procedure
;;> @name[scheme-procedure?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate scheme-procedure? insider::procedure)

;;> @procedure
;;> @name[native-procedure?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate native-procedure? insider::native_procedure)

;;> @procedure
;;> @name[procedure-prototype?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate procedure-prototype? insider::procedure_prototype)

;;> @procedure
;;> @name[symbol?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate symbol? insider::symbol)

;;> @procedure
;;> @name[string?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate string? insider::string)

;;> @procedure
;;> @name[boolean?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate boolean? insider::boolean)

;;> @procedure
;;> @name[keyword?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate keyword? insider::keyword)

;;> @procedure
;;> @name[pair?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate pair? insider::pair)

;;> @procedure
;;> @name[vector?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate vector? insider::vector)

;;> @procedure
;;> @name[bytevector?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate bytevector? insider::bytevector)

;;> @procedure
;;> @name[char?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate char? insider::character)

;;> @procedure
;;> @name[string?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate string? insider::string)

;;> @procedure
;;> @name[string-cursor?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate string-cursor? insider::string_cursor)

;;> @procedure
;;> @name[textual-input-port?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate textual-input-port? insider::textual_input_port)

;;> @procedure
;;> @name[binary-input-port?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate binary-input-port? insider::binary_input_port)

;;> @procedure
;;> @name[textual-output-port?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate textual-output-port? insider::textual_output_port)

;;> @procedure
;;> @name[binary-output-port?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate binary-output-port? insider::binary_output_port)

;;> @procedure
;;> @name[values-tuple?]
;;> @arg[x]
;;> @in-group[type-predicates]
(define-type-predicate values-tuple? insider::values_tuple)

;;> @in-group[type-predicates]
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

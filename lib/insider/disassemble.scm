(library (insider disassemble))
(import (insider syntax) (insider basic-procedures) (insider io)
        (insider string) (insider list) (insider vector) (insider numeric)
        (only (insider internal)
              procedure-prototype-bytecode procedure-prototype-name opcodes
              instruction-opcode instruction-operands top-level-name
              procedure-prototype-constants top-level-value immediate-bias
              procedure-prototype))
(export disassemble)

(define instruction-column 8)
(define comment-column 45)

(define longest-mnemonic
  (let loop ((i 0)
             (result 0))
    (if (= i (vector-length opcodes))
        result
        (let ((length (string-length (symbol->string (vector-ref opcodes i)))))
          (loop (+ i 1)
                (if (> length result) length result))))))

(define (instruction-mnemonic instr)
  (opcode-mnemonic (instruction-opcode instr)))

(define (opcode-mnemonic opcode)
  (vector-ref opcodes opcode))

(define (format-mnemonic mnemonic)
  (let* ((m (symbol->string mnemonic))
         (len (string-length m)))
    (string-append m
                   (make-string (- longest-mnemonic len) #\space))))

(define (immediate-value imm)
  (- imm immediate-bias))

(define (format-immediate imm)
  (string-append "$" (number->string (immediate-value imm))))

(define (format-load-immediate instr)
  (let ((operands (instruction-operands instr)))
    (let ((imm (car operands)) (dest (cadr operands)))
      (string-append (format-mnemonic (instruction-mnemonic instr))
                     " "
                     (format-immediate imm)
                     ", "
                     (number->string dest)))))

(define (format-jump instr)
  (let ((offset (car (instruction-operands instr))))
    (string-append (format-mnemonic 'jump)
                   " "
                   (format-immediate offset))))

(define (format-jump-unless instr)
  (let* ((operands (instruction-operands instr))
         (register (car operands))
         (offset (cadr operands)))
    (string-append (format-mnemonic 'jump-unless)
                   " "
                   (number->string register)
                   ", "
                   (format-immediate offset))))

(define (format-general-instruction instr)
  (string-append (format-mnemonic (instruction-mnemonic instr))
                 " "
                 (string-join (map number->string (instruction-operands instr))
                              ", ")))

(define (format-instruction instr)
  (case (instruction-mnemonic instr)
    ((load-fixnum load-character) (format-load-immediate instr))
    ((jump)                       (format-jump instr))
    ((jump-unless)                (format-jump-unless instr))
    (else                         (format-general-instruction instr))))

(define (related-constant instr)
  (case (instruction-mnemonic instr)
    ((load-constant call-constant tail-call-constant)
     (car (instruction-operands instr)))
    (else #f)))

(define (related-top-level instr)
  (let ((m (instruction-mnemonic instr)))
    (case m
      ((load-top-level call-top-level tail-call-top-level)
       (let ((top-level-num (car (instruction-operands instr))))
         (cons top-level-num (top-level-value top-level-num))))
      ((store-top-level)
       (let ((top-level-num (cadr (instruction-operands instr))))
         (cons top-level-num (top-level-value top-level-num))))
      (else #f))))

(define (related-top-level-name instr)
  (let ((related (related-top-level instr)))
    (if related
        (cons (car related) (top-level-name (car related)))
        #f)))

(define (jump-offset instr)
  (immediate-value (case (instruction-mnemonic instr)
                     ((jump)        (car (instruction-operands instr)))
                     ((jump-unless) (cadr (instruction-operands instr))))))

(define (related-jump-address address size instr)
  (+ address size (jump-offset instr)))

(define (related-address instruction-record)
  (let ((address (car instruction-record))
        (size (cadr instruction-record))
        (instr (caddr instruction-record)))
    (case (instruction-mnemonic instr)
      ((jump jump-unless)
       (related-jump-address address size instr))
      (else #f))))

(define (format-related-constant f index)
  (string-append "constant " (number->string index) " = "
                 (datum->string (vector-ref (procedure-prototype-constants f)
                                            index))))

(define (format-related-top-level value)
  (string-append "top-level " (number->string (car value)) " = " (cdr value)))

(define (format-related-address address)
  (string-append "=> " (number->string address)))

(define (instruction-comment f instruction-record)
  (let* ((instr (caddr instruction-record))
         (related-constant (related-constant instr))
         (related-name (related-top-level-name instr))
         (related-address (related-address instruction-record)))
    (cond
     (related-constant (format-related-constant f related-constant))
     (related-name (format-related-top-level related-name))
     (related-address (format-related-address related-address))
     (else #f))))

(define (display-procedure-constants f)
  (display "Constants: ")
  (newline)
  (let* ((consts (procedure-prototype-constants f))
         (len (vector-length consts)))
    (do ((i 0 (+ i 1)))
        ((= i len))
      (let ((value (vector-ref consts i)))
        (display "  ")
        (display i)
        (display ": ")
        (display (type value))
        (display ": ")
        (write value)
        (newline)))))

(define (display-procedure-header f number-kind number)
  (display (or (procedure-prototype-name f) "<lambda>"))

  (when number
    (display " (")
    (display number-kind)
    (display " ")
    (display number)
    (display ")"))

  (display #\:)
  (newline)

  (unless (zero? (vector-length (procedure-prototype-constants f)))
    (display-procedure-constants f)
    (newline)))

(define (make-indent target-column current-column)
  (if (>= current-column target-column)
      ""
      (make-string (- target-column current-column) #\space)))

(define (display-location-and-indent location)
  (let ((location-string (number->string location)))
    (display location-string)
    (display (make-indent instruction-column (string-length location-string)))))

(define (disassemble-procedure f number-kind number)
  (display-procedure-header f number-kind number)
  (display "Code:")
  (newline)

  (let loop ((instruction-records (procedure-prototype-bytecode f)))
    (unless (null? instruction-records)
      (let* ((record (car instruction-records))
             (location (car record))
             (instr (caddr record)))
        (display-location-and-indent location)

        (let ((formatted-instr (format-instruction instr)))
          (display formatted-instr)

          (let ((comment (instruction-comment f record)))
            (when comment
              (display (make-indent comment-column
                                    (string-length formatted-instr)))
              (display " ; ")
              (display comment))))

        (newline)
        (loop (cdr instruction-records))))))

(define (procedure-prototype* procedure-or-prototype)
  (if (scheme-procedure? procedure-or-prototype)
      (procedure-prototype procedure-or-prototype)
      procedure-or-prototype))

(define (find-related-procedures f)
  (let* ((consts (procedure-prototype-constants f))
         (len (vector-length consts)))
    (let loop ((i 0) (result '()))
      (if (= i len)
          result
          (loop (+ i 1)
                (let ((k (vector-ref consts i)))
                  (if (or (scheme-procedure? k) (procedure-prototype? k))
                      (cons (list 'constant i (procedure-prototype* k)) result)
                      result)))))))

(define (add-to-do old-to-do done new-to-do)
  (let loop ((new new-to-do)
             (result old-to-do))
    (if (null? new)
        result
        (loop (cdr new)
              (if (or (member (car new) result
                              (lambda (x y) (eq? (caddr x) (caddr y))))
                      (memq (caddr (car new)) done))
                  result
                  (cons (car new) result))))))

(define (disassemble f)
  (when (scheme-procedure? f)
    (set! f (procedure-prototype f)))
  (let loop ((done '())
             (to-do (list (list #f #f f))))
    (unless (null? to-do)
      (let* ((current (car to-do))
             (current-proc (caddr current)))
        (unless (null? done)
          (newline))

        (disassemble-procedure current-proc (car current) (cadr current))
        (let ((done* (cons current-proc done)))
          (loop done*
                (add-to-do (cdr to-do)
                           done*
                           (find-related-procedures current-proc))))))))

(library (insider disassemble))
(import (insider syntax) (insider basic-procedures) (insider io)
        (insider string) (insider list) (insider vector) (insider numeric)
        (only (insider internal)
              procedure-bytecode procedure-name opcodes instruction-opcode
              instruction-operands top-level-name static-value top-level-value
              closure-procedure))
(export disassemble)

(define instruction-column 8)
(define comment-column 30)

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

(define (related-static instr)
  (let ((m (instruction-mnemonic instr)))
    (if (or (eq? m 'load-static)
            (eq? m 'call-static)
            (eq? m 'tail-call-static))
        (let ((static-num (car (instruction-operands instr))))
          (cons static-num (static-value static-num)))
        #f)))

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

(define (opcode-mnemonic opcode)
  (vector-ref opcodes opcode))

(define (format-mnemonic mnemonic)
  (let* ((m (symbol->string mnemonic))
         (len (string-length m)))
    (string-append m
                   (make-string (- longest-mnemonic len) #\space))))

(define (format-instruction instr)
  (string-append (format-mnemonic (instruction-mnemonic instr))
                 " "
                 (string-join (map number->string (instruction-operands instr))
                              ", ")))

(define (format-related kind value)
  (string-append (symbol->string kind)
                 " "
                 (number->string (car value))
                 " = "
                 (if (eq? kind 'static)
                     (string-append (symbol->string (type (cdr value)))
                                    " "
                                    (datum->string (cdr value)))
                     (cdr value))))

(define (make-indent target-column current-column)
  (if (>= current-column target-column)
      ""
      (make-string (- target-column current-column) #\space)))

(define (instruction-comment instruction-record)
  (let* ((instr (caddr instruction-record))
         (related-static (related-static instr))
         (related-name (related-top-level-name instr)))
    (cond
     (related-static (format-related 'static related-static))
     (related-name (format-related 'top-level related-name))
     (else #f))))

(define (display-procedure-header f number-kind number)
  (display (or (procedure-name f) "<lambda>"))

  (when number
    (display " (")
    (display number-kind)
    (display " ")
    (display number)
    (display ")"))

  (display #\:)
  (newline))

(define (display-location-and-indent location)
  (let ((location-string (number->string location)))
    (display location-string)
    (display (make-indent instruction-column (string-length location-string)))))

(define (disassemble-procedure f number-kind number)
  (display-procedure-header f number-kind number)

  (let loop ((instruction-records (procedure-bytecode f)))
    (unless (null? instruction-records)
      (let* ((record (car instruction-records))
             (location (car record))
             (instr (caddr record)))
        (display-location-and-indent location)

        (let ((formatted-instr (format-instruction instr)))
          (display formatted-instr)

          (let ((comment (instruction-comment record)))
            (when comment
              (display (make-indent comment-column
                                    (string-length formatted-instr)))
              (display " ; ")
              (display comment))))

        (newline)
        (loop (cdr instruction-records))))))

(define (related-procedure record recurse-to-top-levels?)
  (define (find getter name)
    (let ((related (getter (caddr record))))
      (if related
          (let ((related* (cdr related)))
            (if (scheme-procedure? related*)
                (if (plain-procedure? related*)
                    (list name (car related) related*)
                    (list name (car related) (closure-procedure related*)))
                #f))
          #f)))

  (or (find related-static 'static)
      (and recurse-to-top-levels? (find related-top-level 'top-level))))

(define (find-related-procedures f done to-do recurse-to-top-levels?)
  (let loop ((instruction-records (procedure-bytecode f))
             (result '()))
    (if (null? instruction-records)
        result
        (let ((related (related-procedure (car instruction-records)
                                          recurse-to-top-levels?)))
          (loop (cdr instruction-records)
                (if related
                    (cons related result)
                    result))))))

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

(define (disassemble f . rest)
  (when (closure? f)
    (set! f (closure-procedure f)))

  (let ((recurse-to-top-levels? (and (not (null? rest)) (car rest))))
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
                             (find-related-procedures current-proc done* to-do
                                                      recurse-to-top-levels?)))))))))

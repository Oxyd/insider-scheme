(library (insider disassemble))
(import (insider base-scheme)
        (only (insider internal)
              procedure-bytecode procedure-name opcodes instruction-opcode instruction-operands
              top-level-name static-value))
(export disassemble)

(define indent "   ")
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

(define (related-top-level-name instr)
  (let ((m (instruction-mnemonic instr)))
    (cond
     ((or (eq? m 'load-global)
          (eq? m 'call-global)
          (eq? m 'tail-call-global))
      (let ((top-level-num (car (instruction-operands instr))))
        (cons top-level-num (top-level-name top-level-num))))
     ((eq? m 'store-global)
      (let ((top-level-num (cadr (instruction-operands instr))))
        (cons top-level-num (top-level-name top-level-num))))
     (#t #f))))

(define (opcode-mnemonic opcode)
  (vector-ref opcodes opcode))

(define (format-mnemonic mnemonic)
  (let* ((m (symbol->string mnemonic))
         (len (string-length m)))
    (string-append m
                   (make-string (- longest-mnemonic len) #\space))))

(define (string-join strings)
  (let loop ((result (make-string 0))
             (strings strings)
             (first? #t))
    (if (null? strings)
        result
        (loop (string-append result (if first? "" ", ") (car strings))
              (cdr strings)
              #f))))

(define (format-instruction instr)
  (string-append (format-mnemonic (instruction-mnemonic instr))
                 " "
                 (string-join (map number->string (instruction-operands instr)))))

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

(define (disassemble f)
  (display (let ((name (procedure-name f)))
             (if name name "<lambda>")))
  (display #\:)
  (newline)
  (let loop ((instr (procedure-bytecode f)))
    (unless (null? instr)
      (display indent)
      (let ((formatted-instr (format-instruction (car instr))))
        (display formatted-instr)
        (let ((related-static (related-static (car instr)))
              (related-name (related-top-level-name (car instr))))
          (when related-static
            (display (make-indent comment-column (string-length formatted-instr)))
            (display " ; ")
            (display (format-related 'static related-static)))
          (when related-name
            (display (make-indent comment-column (string-length formatted-instr)))
            (display " ; ")
            (display (format-related 'global related-name)))))

      (newline)
      (loop (cdr instr)))))

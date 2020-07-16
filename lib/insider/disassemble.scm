(library (insider disassemble))
(import (insider base-scheme)
        (only (insider internal)
              procedure-bytecode procedure-name instruction-opcode instruction-operands operand-scope
              operand-value operand-immediate-value operand-offset opcodes top-level-name static-value))
(export disassemble)

(define indent "   ")

(define longest-mnemonic
  (let loop ((i 0)
             (result 0))
    (if (< i (vector-length opcodes))
        (let ((info (vector-ref opcodes i)))
          (let ((length (string-length (car info))))
            (loop (+ i 1)
                  (if (> length result)
                      length
                      result))))
        result)))

(define (opcode-info opcode)
  (vector-ref opcodes opcode))

(define (display-operand op category)
  (case category
    ((none)
     (display "<>"))

    ((register)
     (case (operand-scope op)
       ((local) (display "L"))
       ((global) (display "G"))
       ((static) (display "S"))
       ((closure) (display "C")))
     (display (operand-value op)))

    ((absolute)
     (display "=")
     (display (operand-immediate-value op)))

    ((offset)
     (let ((offset (operand-offset op)))
       (when (> offset 0)
         (display "+"))
       (display (operand-offset op))))))

(define (register-comment op)
  (case (operand-scope op)
    ((global) (string-append "G" (number->string (operand-value op)) "=" (top-level-name op)))
    ((static) (string-append "S" (number->string (operand-value op)) "=" (datum->string (static-value op))))
    ((closure local) #f)))

(define (register-comments instr info)
  (let loop ((operands (instruction-operands instr))
             (categories (cdr info))
             (accum '()))
    (if (null? operands)
        (reverse accum)
        (loop
         (cdr operands)
         (cdr categories)
         (if (eq? (car categories) 'register)
             (let ((comment (register-comment (car operands))))
               (if comment (cons comment accum) accum))
             accum)))))

(define (display-mnemonic mnemonic)
  (let ((len (string-length mnemonic)))
    (display mnemonic)
    (let loop ((i (- longest-mnemonic len)))
      (when (> i 0)
        (display " ")
        (loop (- i 1))))))

(define (display-instruction instr)
  (let ((info (opcode-info (instruction-opcode instr))))
    (display indent)
    (display-mnemonic (car info))
    (display #\space)

    (let loop ((op (instruction-operands instr))
               (category (cdr info)))
      (display-operand (car op) (car category))
      (unless (null? (cdr op))
        (display ", ")
        (loop (cdr op) (cdr category))))

    (let ((comments (register-comments instr info)))
      (unless (null? comments)
        (display "  ; ")
        (let loop ((comments comments))
          (display (car comments))
          (unless (null? (cdr comments))
            (display ", ")
            (loop (cdr comments))))))))

(define (disassemble f)
  (display (let ((name (procedure-name f)))
             (if name name "<lambda>")))
  (display #\:)
  (newline)
  (let loop ((instr (procedure-bytecode f)))
    (unless (null? instr)
      (display-instruction (car instr))
      (newline)
      (loop (cdr instr)))))

(disassemble display-operand)

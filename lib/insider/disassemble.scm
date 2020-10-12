(library (insider disassemble))
(import (insider base-scheme)
        (only (insider internal)
              procedure-bytecode procedure-name opcodes instruction-opcode instruction-operands
              top-level-name static-value))
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

(define (first-operand instr)
  (car (instruction-operands instr)))

(define (second-operand instr)
  (cadr (instruction-operands instr)))

(define (jump-target instruction-record)
  (let* ((location (car instruction-record))
         (size (cadr instruction-record))
         (next-location (+ location size))
         (instr (caddr instruction-record))
         (mnemonic (instruction-mnemonic instr)))
    (cond
     ((eq? mnemonic 'jump)
      (+ next-location (first-operand instr)))
     ((eq? mnemonic 'jump-back)
      (- next-location (first-operand instr)))
     ((eq? mnemonic 'jump-unless)
      (+ next-location (second-operand instr)))
     ((eq? mnemonic 'jump-back-unless)
      (- next-location (second-operand instr)))
     (#t #f))))

(define (build-jump-alist f)
  (define instrs (procedure-bytecode f))

  (define (prepend-unique x lst)
    (if (memv x lst)
        lst
        (cons x lst)))

  (define jump-targets
    (let loop ((result '())
               (instrs instrs))
      (if (null? instrs)
          result
          (loop (let ((target (jump-target (car instrs))))
                  (if target
                      (prepend-unique target result)
                      result))
                (cdr instrs)))))

  (let loop ((targets jump-targets)
             (current (length jump-targets))
             (result '()))
    (if (null? targets)
        (reverse result)
        (loop (cdr targets)
              (- current 1)
              (cons (cons (car targets)
                          (string-append "L" (number->string current)))
                    result)))))

(define (instruction-comment instruction-record jump-alist)
  (let* ((instr (caddr instruction-record))
         (related-static (related-static instr))
         (related-name (related-top-level-name instr))
         (target (jump-target instruction-record)))
    (cond
     (related-static (format-related 'static related-static))
     (related-name (format-related 'global related-name))
     (target (string-append "=> " (cdr (assv target jump-alist))))
     (#t #f))))

(define (disassemble f)
  (define jump-alist (build-jump-alist f))
  (display (let ((name (procedure-name f)))
             (if name name "<lambda>")))
  (display #\:)
  (newline)
  (let loop ((instruction-records (procedure-bytecode f)))
    (unless (null? instruction-records)
      (let* ((record (car instruction-records))
             (location (car record))
             (instr (caddr record)))
        (let ((location-label (assv location jump-alist)))
          (if location-label
              (begin
                (display (cdr location-label))
                (display (make-indent instruction-column (string-length (cdr location-label)))))
              (display (make-indent instruction-column 0))))

        (let ((formatted-instr (format-instruction instr)))
          (display formatted-instr)

          (let ((comment (instruction-comment record jump-alist)))
            (when comment
              (display (make-indent comment-column (string-length formatted-instr)))
              (display " ; ")
              (display comment))))

        (newline)
        (loop (cdr instruction-records))))))

(library (insider vm-profiler))
(import (insider base-scheme)
        (only (insider internal)
              opcodes
              instruction-counts reset-instruction-counts!
              instruction-times reset-instruction-times!))
(export profile-instructions)

(define longest-mnemonic
  (let loop ((i 0)
             (result 0))
    (if (= i (vector-length opcodes))
        result
        (let ((length (string-length (symbol->string (vector-ref opcodes i)))))
          (loop (+ i 1)
                (if (> length result) length result))))))

(define (display-padded s)
  (display s)
  (do ((i (string-length s) (+ i 1)))
      ((= i longest-mnemonic))
    (display #\space)))

(define (display-instructions instructions)
  (do ((i 0 (+ i 1)))
      ((= i (vector-length instructions)))
    (display-padded (symbol->string (vector-ref opcodes i)))
    (display ": ")
    (display (vector-ref instructions i))
    (newline))
  (display-padded "total") (display ": ")
  (display (let loop ((i 0) (sum 0))
             (if (= i (vector-length instructions))
                 sum
                 (loop (+ i 1) (+ sum (vector-ref instructions i))))))
  (newline))

(define (display-stats counts times)
  (display "Instruction counts:")
  (newline)
  (display-instructions counts)
  (newline)
  (display "Instruction times:")
  (newline)
  (display-instructions times))

(define (profile-instructions thunk)
  (reset-instruction-counts!)
  (reset-instruction-times!)
  (let ((result (thunk)))
    (let ((counts (instruction-counts))
          (times (instruction-times)))
      (display-stats counts times)
      result)))

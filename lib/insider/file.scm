(library (insider file))
(import (insider base-scheme) (insider control)
        (only (insider internal)
              read read-syntax read-syntax-multiple read-syntax-multiple-ci
              write write-simple write-shared display newline
              open-input-file open-output-file close close-input-port close-output-port
              current-input-port-tag current-output-port-tag))
(export read read-syntax read-syntax-multiple read-syntax-multiple-ci
        write write-simple write-shared display newline
        open-input-file open-output-file close close-input-port close-output-port
        current-input-file current-output-file call-with-port
        call-with-input-file with-input-from-file
        call-with-output-file with-output-to-file)

(define current-input-file (make-parameter-from-tag current-input-port-tag))
(define current-output-file (make-parameter-from-tag current-output-port-tag))

(define (call-with-port port proc)
  (let ((result (proc port)))
    (close port)
    result))

(define (call-with-input-file path proc)
  (call-with-port (open-input-file path) proc))

(define (call-with-output-file path proc)
  (call-with-port (open-output-file path) proc))

(define (with-input-from-file path thunk)
  (let ((port (open-input-file path)))
    (parameterize ((current-input-file port))
      (let ((result (thunk)))
        (close port)
        result))))

(define (with-output-to-file path thunk)
  (let ((port (open-output-file path)))
    (parameterize ((current-output-file port))
      (let ((result (thunk)))
        (close port)
        result))))

(library (insider file))
(import (insider base-scheme) (insider control)
        (only (insider internal)
              current-output-port-tag))
(export current-output-file call-with-port call-with-output-file with-output-to-file)

(define current-output-file (make-parameter-from-tag current-output-port-tag))

(define (call-with-port port proc)
  (let ((result (proc port)))
    (close port)
    result))

(define (call-with-output-file path proc)
  (call-with-port (open-output-file path) proc))

(define (with-output-to-file path thunk)
  (let ((port (open-output-file path)))
    (parameterize ((current-output-file port))
      (let ((result (thunk)))
        (close port)
        result))))

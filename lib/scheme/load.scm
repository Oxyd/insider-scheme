(library (scheme load))
(import (scheme base) (scheme read) (scheme eval) (scheme repl) (scheme file)
        (insider opt-lambda))
(export load)

(define load
  (opt-lambda (filename (env (interaction-environment)))
    (call-with-input-file filename
      (lambda (port)
        (let loop ((datum (read port)))
          (unless (eof-object? datum)
            (eval datum env)
            (loop (read port))))))))

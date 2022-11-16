(library (scheme load))
(import (scheme base) (scheme read) (scheme eval) (scheme repl) (scheme file))
(export load)

(define (load filename (env (interaction-environment)))
  (call-with-input-file filename
    (lambda (port)
      (let loop ((datum (read port)))
        (unless (eof-object? datum)
          (eval datum env)
          (loop (read port)))))))

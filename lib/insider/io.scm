(library (insider io))
(import (insider syntax) (insider control) (insider syntax) (insider error) (insider list) (insider opt-lambda)
        (insider string) (insider char)
        (rename (except (insider internal) define let)
                (read-char %read-char)
                (peek-char %peek-char)
                (read-u8 %read-u8)
                (peek-u8 %peek-u8)
                (write-u8 %write-u8)
                (flush-output-port %flush-output-port)))
(export
 call-with-input-file call-with-output-file call-with-port close
 close-input-port close-output-port current-error-port current-input-port
 current-output-port current-source-file-origin display eof-object eof-object?
 flush-output-port flush-output-port get-output-bytevector get-output-string
 newline open-input-bytevector open-input-file open-input-string
 open-output-bytevector open-output-file open-output-string
 open-source-file-relative peek-char peek-u8 read read-bytevector
 read-bytevector! read-char read-line read-string read-syntax
 read-syntax-multiple read-syntax-multiple-ci read-u8 with-input-from-file
 with-output-to-file write write-char write-shared write-simple write-string
 write-u8)

(define current-input-port (make-parameter-from-tag current-input-port-tag))
(define current-output-port (make-parameter-from-tag current-output-port-tag))
(define current-error-port (make-parameter-from-tag current-error-port-tag))
(define current-source-file-origin (make-parameter-from-tag current-source-file-origin-tag))

(define <eof-object> (list 'eof))

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
    (parameterize ((current-input-port port))
      (let ((result (thunk)))
        (close port)
        result))))

(define (with-output-to-file path thunk)
  (let ((port (open-output-file path)))
    (parameterize ((current-output-port port))
      (let ((result (thunk)))
        (close port)
        result))))

(define (eof-object? x)
  (eq? x <eof-object>))

(define (eof-object)
  <eof-object>)

(define (coerce-value x)
  (or x <eof-object>))

(define read-char
  (opt-lambda ((port (current-input-port)))
    (coerce-value (%read-char port))))

(define peek-char
  (opt-lambda ((port (current-input-port)))
    (coerce-value (%peek-char port))))

(define read-u8
  (opt-lambda ((port (current-input-port)))
    (coerce-value (%read-u8 port))))

(define peek-u8
  (opt-lambda ((port (current-input-port)))
    (coerce-value (%peek-u8 port))))

(define write-u8
  (opt-lambda (byte (port (current-output-port)))
    (%write-u8 port byte)))

(define read-line
  (opt-lambda ((port (current-input-port)))
    (let ((result (string)))
      (let loop ()
        (let ((c (read-char port)))
          (cond
           ((eof-object? c)
            (if (string-null? result)
                (eof-object)
                result))
           ((char=? c #\newline)
            result)
           ((char=? c #\return)
            ;; Consume either \r alone or the sequence \r\n.
            (when (eq? (peek-char port) #\newline)
              (read-char port))
            result)
           (else
            (string-append-char! result c)
            (loop))))))))

(define read-string
  (opt-lambda (k (port (current-input-port)))
    (if (eof-object? (peek-char port))
        (eof-object)
        (let ((result (string)))
          (let loop ((k k))
            (if (zero? k)
                result
                (let ((c (read-char port)))
                  (cond ((eof-object? c)
                         result)
                        (else
                         (string-append-char! result c)
                         (loop (- k 1)))))))))))

(define write-string
  (opt-lambda (s (port (current-output-port)) (start (string-cursor-start s)) (end (string-cursor-end s)))
    (string-for-each-cursor
     (lambda (cursor)
       (write-char (string-ref/cursor s cursor) port))
     s start end)))

(define flush-output-port
  (opt-lambda ((port (current-output-port)))
    (%flush-output-port port)))

(define (reverse-list->bytevector/length elements length)
  (let ((result (make-bytevector length)))
    (do ((i length (- i 1))
         (current elements (cdr current)))
        ((zero? i) result)
      (bytevector-u8-set! result (- i 1) (car current)))))

(define read-bytevector
  (opt-lambda (k (port (current-input-port)))
    (if (eof-object? (peek-u8 port))
        (eof-object)
        (let loop ((k k) (length 0) (accum '()))
          (if (zero? k)
              (reverse-list->bytevector/length accum length)
              (let ((c (read-u8 port)))
                (if (eof-object? c)
                    (reverse-list->bytevector/length accum length)
                    (loop (- k 1) (+ length 1) (cons c accum)))))))))

(define read-bytevector!
  (opt-lambda (bv (port (current-input-port)) (start 0) (end (bytevector-length bv)))
    (if (eof-object? (peek-u8 port))
        (eof-object)
        (let ((max-length (- end start)))
          (let loop ((current start) (bytes-read 0))
            (let ((byte (read-u8 port)))
              (if (or (eof-object? byte) (= current end))
                  bytes-read
                  (begin
                    (bytevector-u8-set! bv current byte)
                    (loop (+ current 1) (+ bytes-read 1))))))))))

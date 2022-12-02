(library (insider io))
(import (insider syntax) (insider control) (insider syntax) (insider error) (insider list)
        (insider string) (insider char) (insider basic-procedures) (insider numeric) (insider bytevector)
        (rename (only (insider internal)
                      read-char peek-char read-u8 peek-u8 write-u8 write-char
                      flush-output-port char-ready? u8-ready?
                      current-input-port-tag current-output-port-tag current-error-port-tag
                      current-source-file-origin-tag port-open? close-port
                      open-input-file open-binary-input-file open-input-string open-input-bytevector
                      open-output-file open-binary-output-file open-output-string open-output-bytevector
                      close-input-port close-output-port display
                      get-output-bytevector get-output-string newline open-source-file-relative
                      read read-syntax read-syntax-multiple read-syntax-multiple-ci
                      write write-shared write-simple
                      file-exists? delete-file
                      <eof-object>)
                (flush-output-port %flush-output-port)
                (read-char %read-char)
                (peek-char %peek-char)
                (write-char %write-char)
                (read-u8 %read-u8)
                (peek-u8 %peek-u8)
                (write-u8 %write-u8)
                (char-ready? %char-ready?)
                (u8-ready? %u8-ready?)))
(export binary-port? call-with-input-file call-with-output-file call-with-port char-ready?
        close-input-port close-output-port close-port current-error-port current-input-port
        current-output-port current-source-file-origin delete-file display eof-object eof-object?
        file-exists?  flush-output-port flush-output-port get-output-bytevector get-output-string
        input-port-open?  input-port?  newline open-input-bytevector open-input-file open-input-string
        open-binary-input-file open-binary-output-file open-output-bytevector open-output-file
        open-output-string open-source-file-relative output-port-open?  output-port?  peek-char peek-u8
        port?  read read-bytevector read-bytevector!  read-char read-line read-string read-syntax
        read-syntax-multiple read-syntax-multiple-ci read-u8 textual-port?  u8-ready?  with-input-from-file
        with-output-to-file write write-bytevector write-char write-shared write-simple write-string
        write-u8)

(define current-input-port (make-parameter-from-tag current-input-port-tag))
(define current-output-port (make-parameter-from-tag current-output-port-tag))
(define current-error-port (make-parameter-from-tag current-error-port-tag))
(define current-source-file-origin (make-parameter-from-tag current-source-file-origin-tag))

(define-type-predicate textual-input-port? insider::textual_input_port)
(define-type-predicate binary-input-port? insider::binary_input_port)
(define-type-predicate textual-output-port? insider::textual_output_port)
(define-type-predicate binary-output-port? insider::binary_output_port)

(define (input-port? p)
  (or (textual-input-port? p) (binary-input-port? p)))

(define (output-port? p)
  (or (textual-output-port? p) (binary-output-port? p)))

(define (textual-port? p)
  (or (textual-input-port? p) (textual-output-port? p)))

(define (binary-port? p)
  (or (binary-input-port? p) (binary-output-port? p)))

(define (port? p)
  (or (textual-input-port? p) (binary-input-port? p) (textual-output-port? p) (binary-output-port? p)))

(define input-port-open? port-open?)
(define output-port-open? port-open?)

(define (call-with-port port proc)
  (let ((result (proc port)))
    (close-port port)
    result))

(define (call-with-input-file path proc)
  (call-with-port (open-input-file path) proc))

(define (call-with-output-file path proc)
  (call-with-port (open-output-file path) proc))

(define (with-input-from-file path thunk)
  (let ((port (open-input-file path)))
    (parameterize ((current-input-port port))
      (let ((result (thunk)))
        (close-port port)
        result))))

(define (with-output-to-file path thunk)
  (let ((port (open-output-file path)))
    (parameterize ((current-output-port port))
      (let ((result (thunk)))
        (close-port port)
        result))))

(define (eof-object? x)
  (eq? x <eof-object>))

(define (eof-object)
  <eof-object>)

(define (coerce-value x)
  (or x <eof-object>))

(define (read-line (port (current-input-port)))
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
          (loop)))))))

(define (read-string k (port (current-input-port)))
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
                       (loop (- k 1))))))))))

(define (write-string s (port (current-output-port))
                      (start (string-cursor-start s))
                      (end (string-cursor-end s)))
  (display (substring s start end) port))

(define (flush-output-port (port (current-output-port)))
  (%flush-output-port port))

(define (reverse-list->bytevector/length elements length)
  (let ((result (make-bytevector length)))
    (do ((i length (- i 1))
         (current elements (cdr current)))
        ((zero? i) result)
      (bytevector-u8-set! result (- i 1) (car current)))))

(define (read-bytevector k (port (current-input-port)))
  (if (eof-object? (peek-u8 port))
      (eof-object)
      (let loop ((k k) (length 0) (accum '()))
        (if (zero? k)
            (reverse-list->bytevector/length accum length)
            (let ((c (read-u8 port)))
              (if (eof-object? c)
                  (reverse-list->bytevector/length accum length)
                  (loop (- k 1) (+ length 1) (cons c accum))))))))

(define (read-bytevector! bv (port (current-input-port)) (start 0) (end (bytevector-length bv)))
  (if (eof-object? (peek-u8 port))
      (eof-object)
      (let ((max-length (- end start)))
        (let loop ((current start) (bytes-read 0))
          (let ((byte (read-u8 port)))
            (if (or (eof-object? byte) (= current end))
                bytes-read
                (begin
                  (bytevector-u8-set! bv current byte)
                  (loop (+ current 1) (+ bytes-read 1)))))))))

(define (write-bytevector bv (port (current-output-port))
                          (start 0) (end (bytevector-length bv)))
  (do ((i start (+ i 1)))
      ((= i end))
    (write-u8 (bytevector-u8-ref bv i) port)))

(define (read-char (port (current-input-port)))
  (%read-char port))

(define (peek-char (port (current-input-port)))
  (%peek-char port))

(define (write-char c (port (current-output-port)))
  (%write-char c port))

(define (read-u8 (port (current-input-port)))
  (%read-u8 port))

(define (peek-u8 (port (current-input-port)))
  (%peek-u8 port))

(define (write-u8 b (port (current-output-port)))
  (%write-u8 b port))

(define (char-ready? (port (current-input-port)))
  (%char-ready? port))

(define (u8-ready? (port (current-input-port)))
  (%u8-ready? port))

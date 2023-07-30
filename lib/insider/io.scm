;;> @parameter
(define current-input-port (make-parameter-from-tag current-input-port-tag))

;;> @parameter
(define current-output-port (make-parameter-from-tag current-output-port-tag))

;;> @parameter
(define current-error-port (make-parameter-from-tag current-error-port-tag))

;;> @parameter
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

;;> Is @scm{p} a binary port?
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

(define (call-with-input-string str proc)
  (call-with-port (open-input-string str) proc))

(define (call-with-output-string proc)
  (call-with-port (open-output-string)
    (lambda (p)
      (proc p)
      (get-output-string p))))

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

(define (read-line (port (current-input-port)))
  (%read-line port))

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

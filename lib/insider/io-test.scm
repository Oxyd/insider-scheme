(library (insider io-test))
(import (insider syntax) (insider io) (insider test) (insider control) (insider list) (insider bytevector))
(export test-io)

(define (test-ports)
  (test-group "ports"
    (let ((p (open-input-string "")))
      (test (input-port? p))
      (test-false (output-port? p))
      (test (textual-port? p))
      (test-false (binary-port? p))
      (test (port? p)))

    (let ((p (open-input-bytevector #u8())))
      (test (input-port? p))
      (test-false (output-port? p))
      (test-false (textual-port? p))
      (test (binary-port? p))
      (test (port? p)))

    (let ((p (open-output-string)))
      (test-false (input-port? p))
      (test (output-port? p))
      (test (textual-port? p))
      (test-false (binary-port? p))
      (test (port? p)))

    (let ((p (open-output-bytevector)))
      (test-false (input-port? p))
      (test (output-port? p))
      (test-false (textual-port? p))
      (test (binary-port? p))
      (test (port? p)))

    (let ((p (open-input-string "")))
      (test (input-port-open? p))
      (close p)
      (test-false (input-port-open? p)))

    (let ((p (open-output-string)))
      (test (output-port-open? p))
      (close p)
      (test-false (output-port-open? p)))))

(define (test-read)
  (test-group "textual read"
    (define-syntax test-port-result
      (syntax-rules ()
        ((test-port-result expected port-contents port body0 body ...)
         (test-equal expected
                     (call-with-port
                      (open-input-string port-contents)
                      (lambda (port)
                        body0 body ...))))))

    (test-port-result (list #\f #\o #\o (eof-object))
                      "foo" port
                      (let* ((a (read-char port))
                             (b (read-char port))
                             (c (read-char port))
                             (d (read-char port)))
                        (list a b c d)))

    (test-port-result (list #\f #\o #\o)
                      "foo" port
                      (parameterize ((current-input-port port))
                        (let* ((a (read-char))
                               (b (read-char))
                               (c (read-char)))
                          (list a b c))))

    (test-port-result (list #\f #\f)
                      "foo" port
                      (let* ((a (peek-char port))
                             (b (read-char port)))
                        (list a b)))

    (test-port-result (list #\f #\f)
                      "foo" port
                      (parameterize ((current-input-port port))
                        (let* ((a (peek-char))
                               (b (read-char)))
                          (list a b))))

    (test-port-result "foo" "foo\nbar" port (read-line port))
    (test-port-result "foo" "foo" port (read-line port))
    (test-port-result "foo" "foo\n" port (read-line port))
    (test-port-result (eof-object) "" port (read-line port))
    (test-port-result (list "foo" "" "bar") "foo\n\nbar"
                      port
                      (let* ((a (read-line port))
                             (b (read-line port))
                             (c (read-line port)))
                        (list a b c)))
    (test-port-result (list "foo" "bar")
                      "foo\rbar" port
                      (let* ((a (read-line port))
                             (b (read-line port)))
                        (list a b)))
    (test-port-result (list "foo" "bar")
                      "foo\r\nbar" port
                      (let* ((a (read-line port))
                             (b (read-line port)))
                        (list a b)))
    (test-port-result "foo" "foo\r" port (read-line port))

    (test-port-result "foo" "foobar" port (read-string 3 port))
    (test-port-result "foobar" "foobar" port (read-string 12 port))
    (test-port-result (eof-object) "" port (read-string 1 port))

    (test-port-result (list 1 2 3) "(1 2 3)" port (read port))))

(define (test-write)
  (test-group "textual write"
    (define-syntax test-port-result
      (syntax-rules ()
        ((test-port-result expected port body0 body ...)
         (test-equal expected
                     (call-with-port
                      (open-output-string)
                      (lambda (port)
                        body0 body ...
                        (get-output-string port)))))))

    (test-port-result "(1 2 3)" port (write '(1 2 3) port))
    (test-port-result "(#0=(1 2) #0#)" port (write-shared '(#0=(1 2) #0#) port))
    (test-port-result "#0=(1 . #0#)" port (write '#0=(1 . #0#) port))

    (test-port-result "\"foo\"" port (write "foo" port))
    (test-port-result "foo" port (display "foo" port))
    (test-port-result "foo" port (write 'foo port))
    (test-port-result "foo" port (display 'foo port))

    (test-port-result "#\\a" port (write #\a port))
    (test-port-result "a" port (display #\a port))

    (test-port-result "\n" port (newline port))

    (test-port-result "a" port (write-char #\a port))
    (test-port-result "a" port (parameterize ((current-output-port port))
                                 (write-char #\a)))

    (test-port-result "foo" port (write-string "foo" port))
    (test-port-result "bar" port (write-string "foobarbaz" port 3 6))))

(define (test-binary-read)
  (test-group "binary read"
    (define-syntax test-port-result
      (syntax-rules ()
        ((test-port-result expected port-contents port body0 body ...)
         (test-equal expected
                     (call-with-port
                      (open-input-bytevector port-contents)
                      (lambda (port)
                        body0 body ...))))))

    (test-port-result (list 1 2 3) #u8(1 2 3) port
                      (let* ((a (read-u8 port))
                             (b (read-u8 port))
                             (c (read-u8 port)))
                        (list a b c)))

    (test-port-result (list 1 1) #u8(1 2 3) port
                      (let* ((a (peek-u8 port))
                             (b (read-u8 port)))
                        (list a b)))

    (test-port-result (list 1 2 3) #u8(1 2 3) port
                      (parameterize ((current-input-port port))
                        (let* ((a (read-u8))
                               (b (read-u8))
                               (c (read-u8)))
                          (list a b c))))

    (test-port-result (eof-object) #u8() port (read-u8 port))
    (test-port-result (eof-object) #u8() port (peek-u8 port))

    (test-port-result #u8(1 2 3) #u8(1 2 3) port (read-bytevector 3 port))
    (test-port-result (eof-object) #u8() port (read-bytevector 3 port))
    (test-port-result #u8(1 2) #u8(1 2) port (read-bytevector 3 port))
    (test-port-result #u8(1 2) #u8(1 2 3 4) port (read-bytevector 2 port))

    (test-port-result (list #u8(1 2 3) 3) #u8(1 2 3) port
                      (let* ((result (make-bytevector 3))
                             (bytes-read (read-bytevector! result port)))
                        (list result bytes-read)))

    (test-port-result (list #u8(1 2 3) 3) #u8(1 2 3 4 5) port
                      (let* ((result (make-bytevector 3))
                             (bytes-read (read-bytevector! result port)))
                        (list result bytes-read)))
    (test-port-result (list #u8(1 2 0 0 0) 2) #u8(1 2) port
                      (let* ((result (make-bytevector 5))
                             (bytes-read (read-bytevector! result port)))
                        (list result bytes-read)))

    (test-port-result (list #u8(0 0 1 2 3 0 0) 3) #u8(1 2 3 4 5) port
                      (let* ((result (make-bytevector 7))
                             (bytes-read (read-bytevector! result port 2 5)))
                        (list result bytes-read)))))

(define (test-binary-write)
  (test-group "binary write"
    (define-syntax test-port-result
      (syntax-rules ()
        ((test-port-result expected port body0 body ...)
         (test-equal expected
                     (call-with-port
                      (open-output-bytevector)
                      (lambda (port)
                        body0 body ...
                        (get-output-bytevector port)))))))

    (test-port-result #u8(1 2 3) port (write-u8 1 port) (write-u8 2 port) (write-u8 3 port))
    (test-port-result #u8(1 2 3) port
                      (parameterize ((current-output-port port))
                        (write-u8 1)
                        (write-u8 2)
                        (write-u8 3)))))

(define (test-io)
  (test-group "I/O"
    (test-ports)
    (test-read)
    (test-write)
    (test-binary-read)
    (test-binary-write)))

(when-main-module
 (test-io))

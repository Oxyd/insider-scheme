(define-record-type <formatter-state>
  (make-state* port format-string position end args current-arg)
  formatter-state?
  (port state-port)
  (format-string state-format-string)
  (position state-position set-state-position!)
  (end state-end)
  (args state-args)
  (current-arg state-current-arg set-state-current-arg!))

(define (make-state port format-string args)
  (make-state* port
               format-string
               (string-cursor-start format-string)
               (string-cursor-end format-string)
               (list->vector args)
               0))

(define (advance-current-arg! state)
  (set-state-current-arg! state (+ (state-current-arg state) 1)))

(define (advance-state-position! state)
  (set-state-position! state (string-cursor-next (state-format-string state)
                                                 (state-position state))))

(define (state-current-char state)
  (string-ref/cursor (state-format-string state) (state-position state)))

(define (state-at-end? state)
  (string-cursor=? (state-position state) (state-end state)))

(define (char-ascii? c)
  (and (char? c) (<= (char->integer c) #x7F)))

(define (ascii-digit? c)
  (and (char-ascii? c) (char-numeric? c)))

(define (parse-number state)
  (let ((index-start (state-position state)))
    (do () ((or (state-at-end? state)
                (not (ascii-digit? (state-current-char state)))))
      (advance-state-position! state))
    (string->number (substring (state-format-string state)
                               index-start
                               (state-position state)))))

(define (find-argument state index)
  (let ((index* (or index (state-current-arg state))))
    (let ((result (if (< index* (vector-length (state-args state)))
                      (vector-ref (state-args state) index*)
                      (error "Not enough arguments"))))
      (unless index
        (advance-current-arg! state))
      result)))

(define (peek state)
  (if (state-at-end? state)
      #f
      (state-current-char state)))

(define (read-char state)
  (if (state-at-end? state)
      #f
      (let ((result (state-current-char state)))
        (advance-state-position! state)
        result)))

(define (require-char state)
  (when (state-at-end? state)
    (error "Unexpected end of format string"))
  (let ((result (state-current-char state)))
    (advance-state-position! state)
    result))

(define (raise-unexpected c)
  (error (string-append "Unexpected " (string c) " in format string")))

(define (consume! state c)
  (let ((looking-at (require-char state)))
    (unless (char=? looking-at c)
      (raise-unexpected looking-at))))

(define-record-type <field-format>
  (field-format fill align sign alternative-form? zero-pad? width precision type)
  field-format?
  (fill field-format-fill)
  (align field-format-align)
  (sign field-format-sign)
  (alternative-form? field-format-alternative-form?)
  (zero-pad? field-format-zero-pad?)
  (width field-format-width)
  (precision field-format-precision)
  (type field-format-type))

(define (parse-fill&align state)
  (define (align-character? c)
    (memq c '(#\< #\> #\^)))

  (let* ((original-position (state-position state))
         (first (read-char state))
         (position-after-first (state-position state))
         (second (read-char state)))
    (cond ((and first second (align-character? second))
           (values first second))
          ((and first (align-character? first))
           (set-state-position! state position-after-first)
           (values #f first))
          (else
           (set-state-position! state original-position)
           (values #f #f)))))

(define (parse-type-spec state)
  (let ((type (peek state)))
    (case type
      ((#\a #\b #\d #\e #\f #\g #\o #\w #\x)
       (advance-state-position! state)
       type)
      (else
       #f))))

(define (parse-sign state)
  (case (peek state)
    ((#\+ #\- #\space) => (lambda (sign)
                            (advance-state-position! state)
                            sign))
    (else #f)))

(define (parse-alternative-form state)
  (case (peek state)
    ((#\#)
     (advance-state-position! state)
     #t)
    (else
     #f)))

(define (parse-zero-pad state)
  (case (peek state)
    ((#\0)
     (advance-state-position! state)
     #t)
    (else
     #f)))

(define parse-width parse-number)

(define (parse-precision state)
  (cond
   ((char=? (peek state) #\.)
    (advance-state-position! state)
    (or (parse-number state)
        (error "Invalid precision in format string")))
   (else
    #f)))

(define (parse-format-spec state)
  (case (require-char state)
    ((#\:)
     (let*-values (((fill align) (parse-fill&align state))
                   ((sign) (parse-sign state))
                   ((alternative-form?) (parse-alternative-form state))
                   ((zero-pad?) (parse-zero-pad state))
                   ((width) (parse-width state))
                   ((precision) (parse-precision state))
                   ((type) (parse-type-spec state)))
       (consume! state #\})
       (field-format fill align sign alternative-form? zero-pad? width precision
                     type)))
    ((#\})
     (field-format #f #f #f #f #f #f #f #f))
    (else => raise-unexpected)))

(define (print-fill count fill port)
  (when (positive? count)
    (do ((i 0 (+ i 1)))
        ((= i count))
      (write-char fill port))))

(define (print-aligned s min-width fill align port)
  (case align
    ((#\<)
     (write-string s port)
     (print-fill (- min-width (string-length s)) fill port))
    ((#\>)
     (print-fill (- min-width (string-length s)) fill port)
     (write-string s port))
    ((#\^)
     (let ((len (string-length s)))
       (print-fill (floor (/ (- min-width len) 2)) fill port)
       (write-string s port)
       (print-fill (ceiling (/ (- min-width len) 2)) fill port)))))

(define (print-number argument port spec formatter)
  (let ((fill (or (field-format-fill spec) #\space))
        (align (or (field-format-align spec) #\>))
        (min-width (field-format-width spec)))
    (if (not min-width)
        (formatter argument port)
        (let ((formatted (call-with-output-string
                          (lambda (string-port)
                            (formatter argument string-port)))))
          (print-aligned formatted min-width fill align port)))))

(define (abs-real-part z)
  (cond ((real? z) (abs z))
        (else (make-rectangular (abs (real-part z)) (imag-part z)))))

(define (negative-real-part? z)
  (negative? (real-part z)))

(define (write-exact-prefix spec port)
  (cond ((field-format-alternative-form? spec)
         (write-string (case (field-format-type spec)
                         ((#\b) "#b")
                         ((#\o) "#o")
                         ((#\d #f) "#d")
                         ((#\x) "#x"))
                       port)
         2)
        (else
         0)))

(define (write-sign argument spec port)
  (let ((sign (field-format-sign spec)))
    (cond ((negative-real-part? argument)
           (write-char #\- port)
           1)
          ((and (memq sign '(#\+ #\space))
                (not (negative-real-part? argument)))
           (write-char sign port)
           1)
          (else
           0))))

(define (exact-number->string argument spec)
  (number->string argument
                  (case (field-format-type spec)
                    ((#\b) 2)
                    ((#\o) 8)
                    ((#\d #f) 10)
                    ((#\x) 16))))

(define (zero-fill-length total-length spec)
  (if (field-format-zero-pad? spec)
      (- (field-format-width spec) total-length)
      0))

(define (require-unspecified! spec accessor error-message)
  (when (accessor spec)
    (error error-message)))

(define (print-exact-number argument port spec)
  (unless (exact? argument)
    (error (string (field-format-type spec)) " is only valid for exact numbers"))

  (require-unspecified! spec field-format-precision
                        "precision not allowed with exact numeric formats")
  (when (and (field-format-alternative-form? spec)
             (eq? (field-format-sign spec) #\space))
    (error "Leading space option cannot be used with alternative form of exact numeric specifiers"))

  (print-number argument port spec
                (lambda (value port)
                  (let* ((prefix-length (write-exact-prefix spec port))
                         (sign-length (write-sign argument spec port))
                         (digits (exact-number->string (abs-real-part argument)
                                                       spec))
                         (total-length (+ prefix-length
                                          sign-length
                                          (string-length digits))))
                    (print-fill (zero-fill-length total-length spec) #\0 port)
                    (write-string digits port)))))

(define (format-inexact-number argument spec port)
  (let ((sign (or (field-format-sign spec) #\-))
        (alternative-form? (field-format-alternative-form? spec))
        (precision (field-format-precision spec))
        (type (or (field-format-type spec) #\g)))
    (cond
     ((real? argument)
      (format-floating-point (inexact argument)
                             #\- alternative-form? precision
                             type port))
     (else
      (let ((r (inexact (real-part argument)))
            (i (inexact (imag-part argument))))
        (format-floating-point r #\- alternative-form?
                               precision type port)
        (format-floating-point i #\+ alternative-form?
                               precision type port)
        (write-char #\i port))))))

(define (inexact-number->string argument spec)
  (call-with-output-string
   (lambda (port)
     (format-inexact-number argument spec port))))

(define (print-inexact-number argument port spec)
  (print-number
   argument port spec
   (lambda (value port)
     (cond
      ((or (infinite? (real-part argument)) (nan? (real-part argument)))
       (format-inexact-number argument spec port))
      (else
       (let* ((sign-length (write-sign argument spec port))
              (digits (inexact-number->string (abs-real-part argument)
                                              spec))
              (total-length (+ sign-length (string-length digits))))
         (print-fill (zero-fill-length total-length spec) #\0 port)
         (write-string digits port)))))))

(define (print-general printer argument port spec)
  (require-unspecified! spec field-format-sign
                        "sign not allowed with general format")
  (require-unspecified! spec field-format-precision
                        "precision not allowed with general format")
  (require-unspecified! spec field-format-alternative-form?
                        "# not allowed with general format")
  (require-unspecified! spec field-format-zero-pad?
                        "sign-aware zero padding not allowed with general format")

  (let ((fill (or (field-format-fill spec) #\space))
        (align (or (field-format-align spec) #\<))
        (min-width (field-format-width spec)))
    (cond
     ((not min-width)
      (printer argument port))
     (else
      (let ((formatted (call-with-output-string
                        (lambda (string-port)
                          (printer argument string-port)))))
        (print-aligned formatted min-width fill align port))))))

(define (print-field state argument spec)
  (let ((port (state-port state)))
    (case (field-format-type spec)
      ((#\a)
       (print-general display argument port spec))
      ((#\w)
       (print-general write argument port spec))
      ((#\b #\o #\d #\x)
       (print-exact-number argument port spec))
      ((#\e #\f #\g)
       (print-inexact-number argument port spec))
      ((#f)
       (cond
        ((and (number? argument) (exact? argument))
         (print-exact-number argument port spec))
        ((number? argument)
         (print-inexact-number argument port spec))
        (else
         (print-general display argument port spec)))))))

(define (process-replacement-field state)
  (let* ((index (parse-number state))
         (spec (parse-format-spec state)))
    (print-field state (find-argument state index) spec)))

(define (print-formatted port fmt . args)
  (let ((state (make-state port fmt args)))
    (do () ((state-at-end? state))
      (cond ((char=? (state-current-char state) #\{)
             (advance-state-position! state)
             (cond ((eq? (peek state) #\{)
                    (advance-state-position! state)
                    (write-char #\{ port))
                   (else
                    (process-replacement-field state))))
            ((char=? (state-current-char state) #\})
             (advance-state-position! state)
             (consume! state #\})
             (write-char #\} port))
            (else
             (write-char (state-current-char state) port)
             (advance-state-position! state))))))

(define (format fmt . args)
  (call-with-output-string
   (lambda (port)
     (apply print-formatted port fmt args))))

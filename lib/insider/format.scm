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
  (field-format type precision)
  field-format?
  (type field-format-type)
  (precision field-format-precision))

(define (parse-type-spec state)
  (let ((type (peek state)))
    (case type
      ((#\a #\b #\d #\f #\o #\w #\x)
       (advance-state-position! state)
       type)
      (else
       #f))))

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
     (let* ((precision (parse-precision state))
            (type (parse-type-spec state)))
       (consume! state #\})
       (field-format type precision)))
    ((#\})
     (field-format #f #f))
    (else => raise-unexpected)))

(define (print-exact-number argument port spec)
  (unless (exact? argument)
    (error (string (field-format-type spec)) " is only valid for exact numbers"))
  (display (number->string argument
                           (case (field-format-type spec)
                             ((#\b) 2)
                             ((#\o) 8)
                             ((#\d) 10)
                             ((#\x) 16)))
           port))

(define (print-inexact-number argument port spec)
  (let ((sign #\-)
        (precision (field-format-precision spec)))
    (cond
     ((real? argument)
      (format-floating-point argument sign precision port))
     (else
      (let ((r (inexact (real-part argument)))
            (i (inexact (imag-part argument))))
        (format-floating-point r sign precision port)
        (format-floating-point i #\+ precision port)
        (write-char #\i port))))))

(define (print-field state argument spec)
  (let ((port (state-port state)))
    (case (field-format-type spec)
      ((#\a #f)
       (display argument port))
      ((#\w)
       (write argument port))
      ((#\b #\o #\d #\x)
       (print-exact-number argument port spec))
      ((#\f)
       (print-inexact-number argument port spec)))))

(define (process-replacement-field state)
  ;; Looking at a {
  (advance-state-position! state)
  (let* ((index (parse-number state))
         (spec (parse-format-spec state)))
    (print-field state (find-argument state index) spec)))

(define (print-formatted port fmt . args)
  (let ((state (make-state port fmt args)))
    (do () ((state-at-end? state))
      (cond ((char=? (state-current-char state) #\{)
             (process-replacement-field state))
            (else
             (write-char (state-current-char state) port)
             (advance-state-position! state))))))

(define (format fmt . args)
  (call-with-output-string
   (lambda (port)
     (apply print-formatted port fmt args))))

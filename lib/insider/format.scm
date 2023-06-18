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

(define (parse-element-index! state)
  (cond ((char=? (state-current-char state) #\})
         (advance-state-position! state)
         #f)
        (else
         (let ((index-start (state-position state)))
           (do () ((or (state-at-end? state)
                       (not (ascii-digit? (state-current-char state)))))
             (advance-state-position! state))
           (when (state-at-end? state)
             (error "Unexpected end of format string"))
           (unless (char=? (state-current-char state) #\})
             (error (string-append "Unexpected "
                                   (string (state-current-char state))
                                   " in format string")))
           (let ((result (string->number (substring (state-format-string state)
                                                    index-start
                                                    (state-position state)))))
             (advance-state-position! state)
             result)))))

(define (find-argument! state index)
  (let ((index* (or index (state-current-arg state))))
    (let ((result (if (< index* (vector-length (state-args state)))
                      (vector-ref (state-args state) index*)
                      (error "Not enough arguments"))))
      (unless index
        (advance-current-arg! state))
      result)))

(define (process-replacement-field! state)
  ;; Looking at a {
  (advance-state-position! state)
  (when (state-at-end? state)
    (error "Unbalanced {} in format specification"))
  (let ((index (parse-element-index! state)))
    (display (find-argument! state index) (state-port state))))

(define (print-formatted port fmt . args)
  (let ((state (make-state port fmt args)))
    (do () ((state-at-end? state))
      (cond ((char=? (state-current-char state) #\{)
             (process-replacement-field! state))
            (else
             (write-char (state-current-char state) port)
             (advance-state-position! state))))))

(define (format fmt . args)
  (call-with-output-string
   (lambda (port)
     (apply print-formatted port fmt args))))

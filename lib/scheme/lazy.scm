(define-record-type <promise>
  (make-promise-object done? value)
  promise-object?
  (done? promise-done? set-promise-done!)
  (value promise-value set-promise-value!))

(define (make-promise-wrapper done? value)
  (box (make-promise-object done? value)))

(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expr)
     (make-promise-wrapper #f (lambda () expr)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (delay-force (make-promise-wrapper #t expr)))))

(define (promise-update! new old)
  (set-promise-done! (unbox old) (promise-done? (unbox new)))
  (set-promise-value! (unbox old) (promise-value (unbox new)))
  (box-set! new old))

(define (force promise)
  (let ((p (unbox promise)))
    (cond ((promise-done? p)
           (promise-value p))
          (else
           (let ((promise* ((promise-value p))))
             (unless (promise-done? (unbox promise))
               (promise-update! promise* promise))
             (force promise))))))

(define (promise? x)
  (and (box? x)
       (promise-object? (unbox x))))

(define (make-promise value)
  (if (promise? value)
      value
      (delay value)))

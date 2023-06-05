(define permissions-none 0)
(define permissions-owner-read #o400)
(define permissions-owner-write #o200)
(define permissions-owner-exec #o100)
(define permissions-owner-all (bitwise-ior permissions-owner-read
                                           permissions-owner-write
                                           permissions-owner-exec))

(define permissions-group-read #o40)
(define permissions-group-write #o20)
(define permissions-group-exec #o10)
(define permissions-group-all (bitwise-ior permissions-group-read
                                           permissions-group-write
                                           permissions-group-exec))

(define permissions-others-read #o4)
(define permissions-others-write #o2)
(define permissions-others-exec #o1)
(define permissions-others-all (bitwise-ior permissions-others-read
                                            permissions-others-write
                                            permissions-others-exec))

(define permissions-all #o777)

(define permissions-set-uid #o4000)
(define permissions-set-gid #o2000)
(define permissions-sticky-bit #o1000)

(define permissions-mask #o7777)

(define (copy-regular-file from to #:when-exists (when-exists 'error))
  (copy-regular-file* from to when-exists))

(define (copy-files from to
                    #:recursive? (recursive? #f)
                    #:when-exists (when-exists 'error)
                    #:symlinks (symlinks 'follow)
                    #:mode (mode 'copy-content))
  (copy-files* from to recursive? when-exists symlinks mode))

(define (create-directory p (existing #f))
  (create-directory* p existing))

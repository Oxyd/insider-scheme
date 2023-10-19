;;> @constant @in-group[permissions]
;;>
;;> These constants represent the bits for read, write and execute permissions
;;> the owner of a file, members of the group that owns the file, and everyone
;;> else, respectively.
(define permissions-none 0)

;;> @constant @in-group[permissions]
(define permissions-owner-read #o400)

;;> @constant @in-group[permissions]
(define permissions-owner-write #o200)

;;> @constant @in-group[permissions]
(define permissions-owner-exec #o100)

;;> @constant @in-group[permissions]
(define permissions-owner-all (bitwise-ior permissions-owner-read
                                           permissions-owner-write
                                           permissions-owner-exec))

;;> @constant @in-group[permissions]
(define permissions-group-read #o40)
;;> @constant @in-group[permissions]
(define permissions-group-write #o20)
;;> @constant @in-group[permissions]
(define permissions-group-exec #o10)
;;> @constant @in-group[permissions]
(define permissions-group-all (bitwise-ior permissions-group-read
                                           permissions-group-write
                                           permissions-group-exec))

;;> @constant @in-group[permissions]
(define permissions-others-read #o4)
;;> @constant @in-group[permissions]
(define permissions-others-write #o2)
;;> @constant @in-group[permissions]
(define permissions-others-exec #o1)
;;> @constant @in-group[permissions]
(define permissions-others-all (bitwise-ior permissions-others-read
                                            permissions-others-write
                                            permissions-others-exec))

;;> @constant
(define permissions-all #o777)

;;> @constant
(define permissions-set-uid #o4000)
;;> @constant
(define permissions-set-gid #o2000)
;;> @constant
(define permissions-sticky-bit #o1000)

;;> @constant
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

(define (set-permissions! path perms #:follow-symlinks? (follow-symlinks? #t))
  (set-permissions!* path perms follow-symlinks?))

(define (add-permissions! path perms #:follow-symlinks? (follow-symlinks? #t))
  (add-permissions!* path perms follow-symlinks?))

(define (remove-permissions! path perms #:follow-symlinks? (follow-symlinks? #t))
  (remove-permissions!* path perms follow-symlinks?))

;;> Blah blah blah
;;> More lines
(define (directory-files/recursive path
                                   #:follow-symlinks? (follow-symlinks? #f)
                                   #:skip-permission-denied? (skip-denied? #f))
  (directory-files/recursive* path follow-symlinks? skip-denied?))

(library (insider include-test))
(import (insider syntax) (insider include) (insider test) (insider control))
(export test-include)

(define (test-include)
  (test-group "include"
    (let ()
      (include "include-test-file-1.scm")
      (test-equal foo 13)
      (test-equal bar 26))

    (let ((x (include "include-test-file-2.scm")))
      (test-equal x 52))

    (let ()
      (include-ci "include-test-file-3.scm")
      (test-equal foo 2))))

(when-main-module
 (test-include))

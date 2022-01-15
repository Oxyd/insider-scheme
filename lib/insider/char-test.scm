(library (insider char-test))
(import (insider syntax) (insider char) (insider test) (insider control))
(export test-char)

(define (comparison)
  (test-group "comparison"
    (test (char=? #\a #\a))
    (test-false (char=? #\a #\b))
    (test (char=? #\a #\a #\a))
    (test-false (char=? #\a #\a #\b #\a))

    (test (char<? #\a #\b))
    (test-false (char<? #\b #\a))
    (test (char<? #\a #\b #\c))
    (test-false (char<? #\a #\a))

    (test (char-ci=? #\a #\a))
    (test (char-ci=? #\a #\A))
    (test-false (char-ci=? #\a #\b))
    (test (char-ci=? #\a #\A #\a #\A))
    (test (char-ci=? #\ň #\Ň))

    (test (char-ci<? #\a #\B))))

(define (test-char)
  (test-group "character"
    (test (char? #\a))
    (test-false (char? "a"))
    (test-false (char? 65))

    (comparison)))

(when-main-module
 (test-char))

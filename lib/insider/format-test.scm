(define (test-format)
  (test-group "format"
    (test-equal "foo bar baz" (format "foo {} baz" "bar"))
    (test-equal "foo 2 bar" (format "foo {} bar" 2))
    (test-equal "1 2 3" (format "{} {} {}" 1 2 3))
    (test-equal "abc" (format "{}{}{}" #\a #\b #\c))
    (test-error (format "{}{}" 1))

    (test-equal "c b a" (format "{2} {1} {0}" #\a #\b #\c))
    (test-equal "a c b" (format "{} {2} {}" #\a #\b #\c))
    (test-error (format "{3}" 1))

    (test-equal #R"(a #\b c)" (format "a {:w} c" #\b))
    (test-equal #R"(foo "bar" baz)" (format "foo {:w} baz" "bar"))))

(when-main-module
 (test-format))

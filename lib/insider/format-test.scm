(define (test-format)
  (test-group "format"
    (test-group "general format"
      (test-equal "foo bar baz" (format "foo {} baz" "bar"))
      (test-equal "foo 2 bar" (format "foo {} bar" 2))
      (test-equal "1 2 3" (format "{} {} {}" 1 2 3))
      (test-equal "abc" (format "{}{}{}" #\a #\b #\c))
      (test-error (format "{}{}" 1))

      (test-equal "c b a" (format "{2} {1} {0}" #\a #\b #\c))
      (test-equal "a c b" (format "{} {2} {}" #\a #\b #\c))
      (test-error (format "{3}" 1))

      (test-equal #R"(a #\b c)" (format "a {:w} c" #\b))
      (test-equal #R"(foo "bar" baz)" (format "foo {:w} baz" "bar"))

      (test-equal "2+3i" (format "{}" 2+3i))

      (test-error (format "{:.2}" 2.0)))

    (test-group "exact numeric format"
      (test-equal "12" (format "{:d}" 12))
      (test-equal "c" (format "{:x}" 12))
      (test-equal "14" (format "{:o}" 12))
      (test-equal "1100" (format "{:b}" 12))

      (test-error (format "{:d}" 'symbol))
      (test-error (format "{:d}" "string"))
      (test-error (format "{:d}" '(1 2)))
      (test-error (format "{:d}" 2.3))
      (test-error (format "{:.2d}" 2))
      (test-error (format "{:.2x}" 2))
      (test-error (format "{:.2o}" 2))
      (test-error (format "{:.2b}" 2)))

    (test-group "inexact numeric format"
      (test-equal "12.000000" (format "{:f}" 12.0))
      (test-equal "1.000000+2.000000i" (format "{:f}" 1.0+2.0i))
      (test-equal "12.00" (format "{:.2f}" 12.0))
      (test-equal "12" (format "{:.0f}" 12.0))
      (test-equal "1.23+2.45i" (format "{:.2f}" 1.231111+2.449999i))
      (test-equal "-nan.0" (format "{:f}" -nan.0))
      (test-equal "+inf.0" (format "{:f}" +inf.0))
      (test-error (format "{:f}" 12)))))

(when-main-module
 (test-format))

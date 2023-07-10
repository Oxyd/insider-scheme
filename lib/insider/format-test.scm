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

      (test-equal "foo  " (format "{:5}" "foo"))
      (test-equal "foo" (format "{:1}" "foo"))
      (test-equal "  foo" (format "{:>5}" "foo"))
      (test-equal "foo--" (format "{:-<5}" "foo"))
      (test-equal "--foo" (format "{:->5}" "foo"))
      (test-equal "-foo-" (format "{:-^5}" "foo"))
      (test-equal "-foo--" (format "{:-^6}" "foo"))

      (test-error (format "{:.2}" "foo"))
      (test-error (format "{:+}" "foo"))
      (test-error (format "{:-}" "foo"))
      (test-error (format "{: }" "foo"))
      (test-error (format "{:-6}" "foo"))
      (test-error (format "{:-/6}" "foo"))
      (test-error (format "{:#a}" "foo"))
      (test-error (format "{:09a}" "foo")))

    (test-group "exact numeric format"
      (test-equal "12" (format "{:d}" 12))
      (test-equal "c" (format "{:x}" 12))
      (test-equal "14" (format "{:o}" 12))
      (test-equal "1100" (format "{:b}" 12))
      (test-equal "1+2i" (format "{:d}" 1+2i))
      (test-equal "+12" (format "{:+d}" 12))
      (test-equal "+1+2i" (format "{:+d}" 1+2i))
      (test-equal " 1+2i" (format "{: d}" 1+2i))
      (test-equal " 12" (format "{: d}" 12))
      (test-equal "   12" (format "{:5d}" 12))
      (test-equal "***12" (format "{:*>5d}" 12))
      (test-equal "12   " (format "{:<5d}" 12))
      (test-equal "  -12" (format "{:5d}" -12))
      (test-equal "  +12" (format "{:+5d}" 12))
      (test-equal "#d12" (format "{:#d}" 12))
      (test-equal "#xc" (format "{:#x}" 12))
      (test-equal "#o14" (format "{:#o}" 12))
      (test-equal "#b1100" (format "{:#b}" 12))
      (test-equal "#d-12" (format "{:#d}" -12))
      (test-equal "#d+12" (format "{:+#d}" 12))
      (test-equal " #d12" (format "{:#5d}" 12))
      (test-equal "00012" (format "{:05d}" 12))
      (test-equal "-0012" (format "{:05d}" -12))
      (test-equal "#d00012" (format "{:#07d}" 12))
      (test-equal "#d-0012" (format "{:#07d}" -12))
      (test-equal "0001+2i" (format "{:07d}" 1+2i))
      (test-equal "   12" (format "{:5}" 12))
      (test-equal "00012" (format "{:05}" 12))
      (test-equal "#d12" (format "{:#}" 12))

      (test-error (format "{:d}" 'symbol))
      (test-error (format "{:d}" "string"))
      (test-error (format "{:d}" '(1 2)))
      (test-error (format "{:d}" 2.3))
      (test-error (format "{:.2d}" 2))
      (test-error (format "{:.2x}" 2))
      (test-error (format "{:.2o}" 2))
      (test-error (format "{:.2b}" 2))
      (test-error (format "{: #d}" 12)))

    (test-group "inexact numeric format"
      (test-equal "12.000000" (format "{:f}" 12.0))
      (test-equal "1.000000+2.000000i" (format "{:f}" 1.0+2.0i))
      (test-equal "12.00" (format "{:.2f}" 12.0))
      (test-equal "12" (format "{:.0f}" 12.0))
      (test-equal "-12.0" (format "{:.1f}" -12.0))
      (test-equal "1.2e+01" (format "{:.1e}" 12.0))
      (test-equal "1.2e+01+1.0e-01i" (format "{:.1e}" 12.0+0.1i))
      (test-equal "12" (format "{:.2g}" 12.0))
      (test-equal "1.2" (format "{:.2g}" 1.2))
      (test-equal "1.2e+02" (format "{:.2g}" 120.0))
      (test-equal "+12.0" (format "{:+.1f}" 12.0))
      (test-equal " 12.0" (format "{: .1f}" 12.0))
      (test-equal "+1.0+2.0i" (format "{:+.1f}" 1.0+2.0i))
      (test-equal "1.23+2.45i" (format "{:.2f}" 1.231111+2.449999i))
      (test-equal "-nan.0" (format "{:f}" -nan.0))
      (test-equal "+inf.0" (format "{:f}" +inf.0))
      (test-equal "   12.0" (format "{:7.1f}" 12.0))
      (test-equal "***12.0" (format "{:*>7.1f}" 12.0))
      (test-equal "12.0   " (format "{:<7.1f}" 12.0))
      (test-equal "12.0" (format "{:.1f}" 12))
      (test-equal "1.0+2.0i" (format "{:.1f}" 1+2i))
      (test-equal "12" (format "{:.0f}" 12.0))
      (test-equal "12." (format "{:#.0f}" 12.0))
      (test-equal "12." (format "{:#.2g}" 12.0))
      (test-equal "12.000" (format "{:#.5g}" 12.0))
      (test-equal "12" (format "{:.5g}" 12.0))
      (test-equal "00012.0" (format "{:07.1f}" 12.0))
      (test-equal "-0012.0" (format "{:07.1f}" -12.0))
      (test-equal "001.2e+01" (format "{:09.1e}" 12.0))
      (test-equal "-01.2e+01" (format "{:09.1e}" -12.0))
      (test-equal "0000000012" (format "{:010.2g}" 12.0))
      (test-equal "0001.2e+02" (format "{:010.2g}" 120.0))
      (test-equal "-001.2e+02" (format "{:010.2g}" -120.0))
      (test-equal "    +inf.0" (format "{:010f}" +inf.0))
      (test-equal "    -inf.0" (format "{:010f}" -inf.0))
      (test-equal "    +nan.0" (format "{:010f}" +nan.0))
      (test-equal "    -nan.0" (format "{:010f}" -nan.0))
      (test-equal "1e+01" (format "{:.1}" 12.0)))))

(when-main-module
 (test-format))
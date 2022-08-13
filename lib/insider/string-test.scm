(library (insider string-test))
(import (insider syntax) (insider string) (insider test) (insider control) (insider char) (insider numeric)
        (insider list) (insider basic-procedures))
(export test-string)

(define (test-string-cursor)
  (test-group "string cursor"
    (test (string-cursor? (string-cursor-start "")))
    (test (string-cursor? (string-cursor-end "")))
    (let* ((s "kůň")
           (first (string-cursor-start s))
           (second (string-cursor-next s first))
           (third (string-cursor-next s second))
           (end (string-cursor-end s)))
      (test-equal third (string-cursor-forward s first 2))
      (test-equal end (string-cursor-forward s first 3))
      (test-equal #\k (string-ref/cursor s first))
      (test-equal #\ů (string-ref/cursor s second))
      (test-equal #\ň (string-ref/cursor s third))

      (test (string-cursor<? first second))
      (test (string-cursor<? first third))
      (test (string-cursor<=? first first))
      (test (string-cursor<=? first second))
      (test (string-cursor<=? first third))

      (test-eq 0 (string-cursor-diff s first first))
      (test-eq 1 (string-cursor-diff s first second))
      (test-eq 2 (string-cursor-diff s first third))
      (test-eq 3 (string-cursor-diff s first end))

      (test-eq 0 (string-cursor->index s first))
      (test-eq 1 (string-cursor->index s second))
      (test-eq 2 (string-cursor->index s third))
      (test-eq 3 (string-cursor->index s end))

      (test-eq first (string-index->cursor s 0))
      (test-eq second (string-index->cursor s 1))
      (test-eq third (string-index->cursor s 2))
      (test-eq end (string-index->cursor s 3))

      (test-eq 2 (string-cursor->index s 2))
      (test-eq second (string-index->cursor s second)))))

(define (test-string-predicates)
  (test-group "string predicates"
    (test (string-null? ""))
    (test-false (string-null? "x"))
    (test-false (string-null? "ž"))

    (test (string-every char-lower-case? "křeček"))
    (test-false (string-every char-lower-case? "křeČek"))
    (test (string-every char-lower-case? "ABcdEF" 2 4))
    (test-eq 4 (string-every digit-value "1234"))
    (test-eq #t (string-every char-lower-case? ""))

    (test (char-upper-case? #\Č))
    (test (string-any char-upper-case? "křeČek"))
    (test-false (string-any char-upper-case? "křeček"))
    (test-eq 4 (string-any digit-value "the answer is 42."))
    (test-false (string-any char-lower-case? ""))))

(define (test-string-constructors)
  (test-group "string constructors"
    (define (integer->digit+1 n)
      (integer->char (+ (char->integer #\0) n 1)))

    (define (add1 i)
      (+ i 1))

    (test-equal "abc" (string #\a #\b #\c))
    (test-equal 3 (string-length (make-string 3)))
    (test-equal "xxx" (make-string 3 #\x))

    (test-equal "123" (string-tabulate integer->digit+1 3))

    (test-equal "abc" (string-unfold null? car cdr '(#\a #\b #\c)))
    (test-equal "123" (string-unfold (lambda (i) (= i 3)) integer->digit+1 add1 0))
    (test-equal "01237"
                (string-unfold (lambda (i) (= i 3))
                               integer->digit+1
                               add1
                               0
                               "0"
                               (lambda (i) (string (integer->digit+1 (* i 2))))))

    (test-equal "cba" (string-unfold-right null? car cdr '(#\a #\b #\c)))))

(define (test-string-conversion)
  (test-group "string conversion"
    (test-equal '(#\a #\b #\c) (string->list "abc"))
    (test-equal '(#\d #\e #\f) (string->list "abcdef" 3))
    (test-equal '(#\d #\e) (string->list "abcdef" 3 5))
    (test-equal '(#\b #\c) (string->list/cursors "abcdef" 1 3))
    (test-equal "abc" (list->string '(#\a #\b #\c)))
    (test-equal "cba" (reverse-list->string '(#\a #\b #\c)))

    (test-equal #(#\a #\b #\c) (string->vector "abc"))
    (test-equal #(#\b #\c) (string->vector/cursors "abcdef" 1 3))

    (test-equal "foobarbaz" (string-join '("foo" "bar" "baz")))
    (test-equal "foo:bar:baz" (string-join '("foo" "bar" "baz") ":"))
    (test-equal "foo" (string-join '("foo") ":"))
    (test-equal "" (string-join '() ":"))
    (test-equal "" (string-join '("") ":"))

    (test-equal "foobarbaz" (string-join '("foo" "bar" "baz") "" 'strict-infix))
    (test-error (string-join '() ":" 'strict-infix))

    (test-equal ":foo:bar:baz" (string-join '("foo" "bar" "baz") ":" 'prefix))
    (test-equal ":foo" (string-join '("foo") ":" 'prefix))
    (test-equal "" (string-join '() ":" 'prefix))
    (test-equal ":" (string-join '("") ":" 'prefix))

    (test-equal "foo:bar:baz:" (string-join '("foo" "bar" "baz") ":" 'suffix))
    (test-equal "" (string-join '() ":" 'suffix))
    (test-equal ":" (string-join '("") ":" 'suffix))

    (test-error (string-join '() "" 'invalid-grammar))

    (test-equal 12 (string->number "12"))
    (test-equal 18 (string->number "12" 16))
    (test-equal 26 (string->number "1a" 16))
    (test-equal 8 (string->number "10" 8))
    (test-equal 18 (string->number "#x12"))
    (test-equal 18 (string->number "#x12" 2))
    (test-equal 100.0 (string->number "1e2"))
    (test-eq #f (string->number "1a"))

    (test-equal "12" (number->string 12))
    (test-equal "c" (number->string 12 16))
    (test-error (number->string 'number))

    (test-equal "A" (utf8->string #u8(#x41)))
    (test-equal "příšerně žluťoučký kůň úpěl ďábelské ódy"
                (utf8->string #u8(#x70 #xc5 #x99 #xc3 #xad #xc5 #xa1 #x65 #x72 #x6e #xc4 #x9b #x20 #xc5 #xbe #x6c #x75 #xc5 #xa5 #x6f #x75 #xc4 #x8d #x6b #xc3 #xbd #x20 #x6b #xc5 #xaf #xc5 #x88 #x20 #xc3 #xba #x70 #xc4 #x9b #x6c #x20 #xc4 #x8f #xc3 #xa1 #x62 #x65 #x6c #x73 #x6b #xc3 #xa9 #x20 #xc3 #xb3 #x64 #x79)))

    (test-equal #u8(#xCE #xBB) (string->utf8 "λ"))
    (test-equal #u8(#x6b #xc5 #xaf #xc5 #x88)
                (string->utf8 "příšerně žluťoučký kůň úpěl ďábelské ódy"
                              19 22))

    (test-equal #(#\A #\B #\C) (string->vector "ABC"))
    (test-equal "123" (vector->string #(#\1 #\2 #\3)))))

(define (test-string-selection)
  (test-group "string selection"
    (test-equal "foo" (string-copy "foo"))
    (test-equal "bar" (substring "foobarbaz" 3 6))
    (test-equal "foo" (string-take "foobarbaz" 3))
    (test-equal "barbaz" (string-drop "foobarbaz" 3))
    (test-equal "baz" (string-take-right "foobarbaz" 3))
    (test-equal "foobar" (string-drop-right "foobarbaz" 3))

    (test-equal "unchanged"
                (let* ((s (string-copy "unchanged"))
                       (copy (substring s 0 (string-length s))))
                  (string-set! copy 0 #\X)
                  s))

    (test-equal "  325" (string-pad "325" 5))
    (test-equal "71325" (string-pad "71325" 5))
    (test-equal "71325" (string-pad "8871325" 5))

    (test-equal "325  " (string-pad-right "325" 5))
    (test-equal "71325" (string-pad-right "71325" 5))
    (test-equal "88713" (string-pad-right "8871325" 5))

    (let ((s "  foo,  \n\r"))
      (test-equal "foo,  \n\r" (string-trim s))
      (test-equal "  foo," (string-trim-right s))
      (test-equal "foo," (string-trim-both s)))))

(define (test-string-affixes)
  (test-group "string affixes"
    (test-equal 3 (string-prefix-length "foobar" "fooquux"))
    (test-equal 0 (string-prefix-length "foo" "quux"))
    (test-equal 3 (string-prefix-length "integer" "point" 0 7 2 5))

    (test-equal 3 (string-suffix-length "monday" "tuesday"))
    (test-equal 3 (string-suffix-length "headmaster" "lead" 0 4))

    (test (string-prefix? "head" "headmaster"))
    (test-false (string-prefix? "squirrel" "headmaster"))
    (test-false (string-prefix? "foobar" "x"))
    (test (string-prefix? "" "anything"))
    (test (string-prefix? "excellent" "cellar" 2 6 0 6))

    (test (string-suffix? "master" "headmaster"))
    (test-false (string-suffix? "squirrel" "headmaster"))
    (test (string-suffix? "" "squirrel"))
    (test (string-suffix? "abcdef" "abcdefghij" 2 4 1 4))))

(define (test-string-searching)
  (test-group "string searching"
    (let ((s "foo bar baz"))
      (test-equal 3 (string-cursor->index s (string-index s char-whitespace?)))
      (test-equal 8 (string-cursor->index s (string-index-right s char-whitespace?)))
      (test-equal 3 (string-cursor->index s (string-skip s char-alphabetic?)))
      (test-equal 8 (string-cursor->index s (string-skip-right s char-alphabetic?))))

    (let ((s "eek -- what a geek."))
      (test-equal 0 (string-cursor->index s (string-contains s "ee")))
      (test-equal 15 (string-cursor->index s (string-contains s "ee" 12 18)))
      (test-false (string-contains s "where"))
      (test-equal 15 (string-cursor->index s (string-contains-right s "ee")))
      (test-equal 0 (string-cursor->index s (string-contains-right s "ee" 0 15))))))

(define (test-string-whole)
  (test-group "whole string"
    (test-equal 3 (string-length "foo"))
    (test-equal 0 (string-length ""))

    (test-equal #\f (string-ref "foo" 0))
    (test-equal #\o (string-ref "foo" 1))
    (test-equal #\o (string-ref "foo" 2))

    (test-equal "Abc"
                (let ((s (string-copy "abc")))
                  (string-set! s 0 #\A)
                  s))

    (test-equal "abCDEfgh"
                (let ((s (string-copy "abcdefgh")))
                  (string-copy! s 2 "CDE")
                  s))

    (test-equal "abfghfgh"
                (let ((s (string-copy "abcdefgh")))
                  (string-copy! s 2 s 5)
                  s))

    (test-equal "abcdecdh"
                (let ((s (string-copy "abcdefgh")))
                  (string-copy! s 5 s 2 4)
                  s))

    (test-equal "aabcefgh"
                (let ((s (string-copy "abcdefgh")))
                  (string-copy! s 1 s 0 3)
                  s))

    (test-equal "abcdefgh"
                (let ((s (string-copy "abcdefgh")))
                  (string-copy! s 1 s 1)
                  s))

    (test-equal ".able was I ere I saw elbA" (string-reverse "Able was I ere I saw elba."))
    (test-equal "snoops" (string-reverse "Who stole the spoons?" 14 20))

    (test-equal "foobarbaz" (string-concatenate '("foo" "bar" "baz")))
    (test-equal "bazbarfoo" (string-concatenate-reverse '("foo" "bar" "baz")))
    (test-equal "bazbarfootail" (string-concatenate-reverse '("foo" "bar" "baz") "tail"))
    (test-equal "bazbarfoota" (string-concatenate-reverse '("foo" "bar" "baz") "tail" 2))
    (test-equal "Hello, I must be going." (string-concatenate-reverse '(" must be" "Hello, I") " going.XXXX" 7))

    (test-equal 2 (string-fold (lambda (c count) (+ count (if (char-upper-case? c) 1 0))) 0 "Foo Bar"))
    (test-equal '(#\k #\ů #\ň) (string-fold-right cons '() "kůň"))

    (test-equal '(101 100 99 98 97)
                (let ((s "abcde") (v '()))
                  (string-for-each-cursor
                   (lambda (cur)
                     (set! v (cons (char->integer (string-ref/cursor s cur)) v)))
                   s)
                  v))

    (test-equal 3
                (let ((length 0))
                  (string-for-each (lambda (c) (set! length (+ length 1))) "foo")
                  length))
    (test-equal "ÈÐá"
                (let ((result (string)))
                  (string-for-each
                   (lambda (c1 c2)
                     (string-append-char! result (integer->char (+ (char->integer c1) (char->integer c2)))))
                   "foo"
                   "bar")
                  result))

    (test-equal "abdegh" (string-map char-foldcase "AbdEgH"))
    (test-equal "IBM"
                (string-map
                 (lambda (c)
                   (integer->char (+ 1 (char->integer c))))
                 "HAL"))
    (test-equal "StUdLyCaPs"
                (string-map
                 (lambda (c k)
                   ((if (eqv? k #\u) char-upcase char-downcase) c))
                 "studlycaps xxx"
                 "ululululul"))

    (test-equal "cdefab" (string-replicate "abcdef" 2 8))
    (test-equal "efabcd" (string-replicate "abcdef" -2 4))
    (test-equal "abcabca" (string-replicate "abc" 0 7))

    (test-equal 2 (string-count "The answer is 42." char-numeric?))

    (test-equal "The miserable perl programmer endured daily ridicule."
                (string-replace "The TCL programmer endured daily ridicule."
                                "another miserable perl drone" 4 7 8 22))
    (test-equal "It's lots of fun to code it up in Scheme."
                (string-replace "It's easy to code it up in Scheme." "lots of fun" 5 9))
    (test-equal "It's really easy to code it up in Scheme."
                (string-replace "It's easy to code it up in Scheme." "really " 5 5))

    (test-equal '("foo" "bar" "baz") (string-split "foo:bar:baz" ":"))
    (test-equal '("foo" "bar" "baz") (string-split "foo, bar, baz" ", "))
    (test-equal '("foo" "bar" "baz" "") (string-split "foo:bar:baz:" ":"))
    (test-equal '("foo" "bar" "baz") (string-split "foo:bar:baz:" ":" 'suffix))
    (test-equal '("foo" "bar" "baz") (string-split ":foo:bar:baz" ":" 'prefix))
    (test-equal '("foo" "bar:baz") (string-split "foo:bar:baz" ":" 'infix 1))
    (test-equal '() (string-split "" ":"))
    (test-error (string-split "" ":" 'strict-infix))
    (test-equal '("f" "o" "o") (string-split "foo" ""))

    (test-equal "bdf" (string-filter char-lower-case? "AbCdEf"))
    (test-equal "ACE" (string-remove char-lower-case? "AbCdEf"))

    (test-equal "aaabbb" (string-append "aaa" "bbb"))
    (test-equal "aaabbbccc" (string-append "aaa" "bbb" "ccc"))

    (test-equal "xxx"
                (let ((s (string-copy "žžž")))
                  (string-fill! s #\x)
                  s))

    (test-equal "žžž"
                (let ((s (string-copy "xxx")))
                  (string-fill! s #\ž)
                  s))

    (test-equal "xxžžžxx"
                (let ((s (string-copy "xxxxxxx")))
                  (string-fill! s #\ž 2 5)
                  s))))

(define (test-string-comparison)
  (test-group "comparison"
    (test (string=? "foo" "foo" "foo"))
    (test-false (string=? "foo" "foo" "bar" "foo"))

    (test (string<? "aaa" "bbb" "ccc"))
    (test-false (string<? "aaa" "ccc" "bbb"))
    (test-false (string<? "aaa" "aaa"))
    (test-false (string<? "aaa" "BBB"))

    (test (string<=? "aaa" "aaa" "aab"))
    (test-false (string<=? "aaa" "aab" "aaa"))

    (test (string>? "ccc" "bbb" "aaa"))
    (test-false (string>? "aaa" "aaa"))
    (test-false (string>? "ccc" "aaa" "bbb"))

    (test (string>=? "ccc" "bbb" "aaa"))
    (test (string>=? "ccc" "ccc" "bbb" "aaa"))
    (test-false (string>=? "aaa" "bbb"))

    (test (string-ci=? "foo" "FOO" "foO"))
    (test-false (string-ci=? "foo" "BAR"))

    (test (string-ci<? "aaA" "bBb" "Ccc"))
    (test-false (string-ci<? "AAA" "ccc" "bbb"))
    (test-false (string-ci<? "AAA" "aaa"))
    (test (string-ci<? "aaa" "BBB"))

    (test (string-ci<=? "aaa" "AAa" "aAb"))
    (test-false (string-ci<=? "AAA" "aab" "aAa"))

    (test (string-ci>? "cCc" "bBB" "AAA"))
    (test-false (string-ci>? "aaa" "aAA"))
    (test-false (string-ci>? "ccc" "AaA" "bbB"))

    (test (string-ci>=? "ccc" "bBB" "Aaa"))
    (test (string-ci>=? "ccc" "cCC" "bbB" "Aaa"))
    (test-false (string-ci>=? "aaA" "BBb"))))

(define (test-string)
  (test-group "string"
    (test (string? "foo"))
    (test-false (string? #\f))

    (test-string-cursor)
    (test-string-predicates)
    (test-string-constructors)
    (test-string-conversion)
    (test-string-selection)
    (test-string-affixes)
    (test-string-searching)
    (test-string-whole)
    (test-string-comparison)))

(when-main-module
 (test-string))

;; Local variables:
;; eval: (put 'test-group 'scheme-indent-function 1)
;; End:

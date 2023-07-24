(define-library (insider string)
  (import (insider syntax) (insider numeric) (insider basic-procedures)
          (insider vector) (insider control) (insider list) (insider error)
          (insider bytevector) (insider char)
          (only (insider internal)
                string string-ref string-set! string-append string-null?
                string-length make-string
                string-cursor-start string-cursor-end
                string-cursor-next* string-cursor-prev*
                string-cursor-diff* string-cursor->index
                string-cursor=? string-cursor<?
                string-copy* string-copy!* string-contains*
                string-contains-right* string-append-char!
                string-append! string-reverse* string=?/pair string<?/pair
                string<=?/pair string>?/pair string>=?/pair string-foldcase
                string->utf8* utf8->string* symbol->string keyword->string
                string->keyword number->string datum->string string->number
                string-upcase string-downcase))
  (export
   string?

   string-append string-append! string-append-char! string-length
   symbol->string keyword->string string->keyword string make-string
   number->string datum->string string-ref string-set! string->number

   string=? string<? string<=? string>? string>=?
   string-ci=? string-ci<? string-ci<=? string-ci>? string-ci>=?

   string-upcase string-downcase string-foldcase
   string->utf8 utf8->string

   string-cursor? string-cursor-start string-cursor-end string-cursor-next
   string-cursor-prev string-cursor-forward string-cursor-back string-cursor=?
   string-cursor<? string-cursor<=? string-cursor>? string-cursor>=?
   string-cursor-diff string-cursor->index string-index->cursor string-ref/cursor

   string-null? string-every string-any

   string-tabulate string-unfold string-unfold-right

   string->list string->list/cursors list->string reverse-list->string
   string->vector string->vector/cursors string-join

   string-copy string-copy/cursors substring substring/cursors string-copy!
   string-fill! string-take string-drop string-take-right string-drop-right
   string-pad string-pad-right string-trim string-trim-right string-trim-both

   string-prefix-length string-suffix-length string-prefix? string-suffix?

   string-index string-index-right string-skip string-skip-right string-contains
   string-contains-right

   string-reverse string-concatenate string-concatenate-reverse string-fold
   string-fold-right
   string-for-each-cursor string-for-each string-map
   string-replicate string-count string-replace string-split string-filter
   string-remove

   vector->string string->vector)
  (include "string.scm"))

(library (insider interactive))
(import (scheme base) (scheme char) (scheme complex) (scheme cxr) (scheme eval)
        (scheme file) (scheme inexact) (scheme lazy) (scheme read) (scheme time)
        (scheme write) (scheme load)
        (insider control))
(export (all-imported-from (scheme base)) (all-imported-from (scheme char))
        (all-imported-from (scheme complex)) (all-imported-from (scheme cxr))
        (all-imported-from (scheme eval)) (all-imported-from (scheme file))
        (all-imported-from (scheme inexact)) (all-imported-from (scheme lazy))
        (all-imported-from (scheme read)) (all-imported-from (scheme time))
        (all-imported-from (scheme write)) (all-imported-from (scheme load))
        import)

(define-syntax import
  (syntax-rules ()
    ((import import-set import-sets ...)
     (dynamic-import (meta (current-expand-module))
                     'import-set 'import-sets ...))))


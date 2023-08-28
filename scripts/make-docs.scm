(import (scheme base) (scheme read) (scheme write) (scheme file)
        (scheme process-context) (insider filesystem) (insider format)
        (insider list) (insider struct) (insider string) (insider filesystem)
        (insider scribble))

(define (find-library-definitions path)
  (cond ((directory? path)
         (filter (lambda (p) (string=? (path-extension p) ".sld"))
                 (directory-files/recursive path #:follow-symlinks? #t)))
        ((string=? (path-extension path) ".sld")
         (list path))
        (else
         (warn "Invalid library path name: {}" path)
         '())))

(define <no-form> (list 'no-form))

(struct element (name
                 module
                 (meta '() #:mutable)
                 (body '() #:mutable)
                 (defining-form-found? #f #:mutable)))

(define (element-copy! target source)
  (element-meta-set! target (element-meta source))
  (element-body-set! target (element-body source))
  (element-defining-form-found?-set! target
                                     (element-defining-form-found? source)))

(struct module (name
                primary-file
                (associated-files #:mutable)
                (imports '() #:mutable)
                (imports-resolved? #f #:mutable)
                (elements '() #:mutable)
                (comment '() #:mutable)))

(define (append-elements! module new-elements)
  (module-elements-set! module (append (module-elements module) new-elements)))

(define (append-imports! module new-imports)
  (module-imports-set! module (append (module-imports module) new-imports)))

(define (append-associated-files! module new-associated-files)
  (module-associated-files-set! module
                                (append (module-associated-files module)
                                        new-associated-files)))

(define (warn message . args)
  (apply printf (string-append "Warning: " message) args)
  (newline))

(define (parse-library-directive! directive module path)
  (unless (null? directive)
    (case (car directive)
      ((export)
       (append-elements! module (map (lambda (name) (element name module))
                                     (cdr directive))))
      ((import)
       (append-imports! module (cdr directive)))
      ((include include-ci)
       (append-associated-files! module
                                 (map (lambda (included-path)
                                        (path-replace-filename path
                                                               included-path))
                                      (cdr directive))))
      ((include-library-declarations)
       (warn "{}: include-library-declarations unimplemented" path))
      ((cond-expand)
       (warn "{}: cond-expand unimplemented" path)))))

(define (parse-module-definition path def)
  (cond
   ((or (null? def)
        (null? (cdr def))
        (not (eq? (car def) 'define-library)))
    (warn "{}: Invalid library file" path)
    #f)
   (else
    (do ((result (module (cadr def) path (list path)))
         (current (cddr def) (cdr current)))
        ((null? current) result)
      (parse-library-directive! (car current) result path)))))

(define (parse-sld path)
  (call-with-input-file path
    (lambda (port)
      (parse-module-definition path (read port)))))

(define (skip-whitespace! port #:skip-newlines? (skip-newlines? #t))
  (let loop ((c (peek-char port)))
    (unless (eof-object? c)
      (when (or (char=? c #\space)
                (and skip-newlines? (char=? c #\newline))
                (and skip-newlines? (char=? c #\tab)))
        (read-char port)
        (loop (peek-char port))))))

(define scheme-doc-comment-prefix ";;>")
(define scheme-doc-comment-prefix-length
  (string-length scheme-doc-comment-prefix))

(define (scheme-doc-comment-line? line)
  (string-prefix? scheme-doc-comment-prefix line))

(define (strip-doc-comment-prefix line)
  (string-drop line scheme-doc-comment-prefix-length))

(define (read-scheme-comment-line port #:skip-newlines? (skip-newlines? #t))
  (skip-whitespace! port skip-newlines?)
  (let ((first (peek-char port)))
    (cond ((eof-object? first)
           first)
          ((char=? first #\;)
           ;; Likely begins a comment, safe to read
           (read-line port))
          (else
           #f))))

(define (read-rest-of-scheme-doc-comment port)
  (let loop ((result '()))
    (let ((line (read-scheme-comment-line port #:skip-newlines? #f)))
      (if (and line (scheme-doc-comment-line? line))
          (loop (cons line result))
          (reverse result)))))

(define (read-scheme-fragment port)
  (let loop ()
    (let ((line (read-scheme-comment-line port)))
      (cond ((eof-object? line)
             line)
            (line
             (if (scheme-doc-comment-line? line)
                 (cons 'doc-comment
                       (cons line (read-rest-of-scheme-doc-comment port)))
                 (loop)))
            (else
             (cons 'form (read port)))))))

(define (read-subsequent-form port)
  ;; To distinguish module-level comments from comments attached to forms, there
  ;; may not be a newline preceding the datum.
  (skip-whitespace! port #:skip-newlines? #f)
  (let ((next (peek-char port)))
    (cond ((eof-object? next)      <no-form>)
          ((char=? next #\newline) <no-form>)
          (else                    (read port)))))

(define (append-module-comment! module comment)
  (module-comment-set! module (append (module-comment module) comment)))

(define (find-define-name form)
  ;; Can be either (define name ...) or (define (name ...) ...) or
  ;; (define ((name ...) ...) ...) or ...
  (let loop ((form (cadr form)))
    (cond ((symbol? form) form)
          ((pair? form)   (loop (car form)))
          (else           #f))))

(define (find-procedure-params form)
  (let ((name-form (cadr form)))
    (cond ((symbol? name-form)
           ;; (define name expr)
           (let ((expr (caddr form)))
             (if (and (pair? expr) (eq? (car expr) 'lambda))
                 (cadr expr)
                 #f)))
          ((pair? name-form)
           ;; (define (subexpr) expr)
           (let loop ((name-form name-form))
             (cond ((symbol? (car name-form))
                    (cdr name-form))
                   ((pair? (car name-form))
                    (loop (car name-form)))
                   (else
                    #f)))))))

(define (find-element module name)
  (let loop ((elements (module-elements module)))
    (cond ((null? elements)
           #f)
          ((eq? (element-name (car elements)) name)
           (car elements))
          (else
           (loop (cdr elements))))))

(define (find-element-for-definition module form)
  (and (pair? form)
       (memq (car form) '(define define-syntax))
       (pair? (cdr form))
       (find-element module (find-define-name form))))

(define meta-commands
  `((procedure . ,(lambda (meta form)
                    (append `((kind . procedure)
                              (procedure-params . ,(cdr form)))
                            meta)))
    (parameter . ,(lambda (meta form)
                    (cons '(kind . parameter) meta)))))

(define (parse-scheme-doc-comment lines)
  (let loop ((scribble (scribble-parse
                        (open-input-string
                         (string-join (map strip-doc-comment-prefix lines)))))
             (meta '())
             (body '()))
    (cond ((null? scribble)
           (values meta (reverse body)))
          ((or (assq (car scribble) meta-commands)
               (and (pair? (car scribble)) (assq (caar scribble) meta-commands)))
           (loop (cdr scribble)
                 (cons (car scribble) meta)
                 body))
          (else
           (loop (cdr scribble)
                 meta
                 (cons (car scribble) body))))))

(define (parse-element-meta meta-scribble)
  (let loop ((meta '()) (scrbl meta-scribble))
    (if (null? scrbl)
        meta
        (let ((f (assq (if (pair? (car scrbl)) (caar scrbl) (car scrbl))
                       meta-commands)))
          (loop ((cdr f) meta (car scrbl)) (cdr scrbl))))))

(define (parse-element-form meta form name)
  (cond
   ((assq 'kind meta)
    meta)
   (else
    (case (car form)
      ((define)
       (let ((params (find-procedure-params form)))
         (cond (params
                (append `((kind . procedure)
                          (procedure-params . ,params))
                        meta))
               (else
                (warn "{}: Unknown define syntax" name)
                meta))))
      ((define-syntax)
       (cons '(kind . syntax) meta))
      (else
       (warn "{}: Unknown form: {:w}" name form)
       meta)))))

(define (set-element-doc-and-form! module form comment path)
  (let ((element (find-element-for-definition module form)))
    (cond (element
           (let-values (((meta body) (parse-scheme-doc-comment comment)))
             (element-meta-set! element
                                (parse-element-form (parse-element-meta meta)
                                                    form
                                                    (element-name element)))
             (element-body-set! element body)
             (element-defining-form-found?-set! element #t)))
          (else
           (warn "{}: Documentation comment ignored for {:w}"
                 path form)))))

(define (extract-module-docs-from-port! module port path)
  (let loop ()
    (let ((fragment (read-scheme-fragment port)))
      (unless (eof-object? fragment)
        (case (car fragment)
          ((doc-comment)
           (let ((comment (cdr fragment))
                 (form (read-subsequent-form port)))
             (if (eq? form <no-form>)
                 (append-module-comment! module comment)
                 (set-element-doc-and-form! module form comment path))
             (loop)))
          ((form)
           (let* ((form (cdr fragment))
                  (element (find-element-for-definition module form)))
             (when element
               (element-meta-set! element
                                  (parse-element-form '() form
                                                      (element-name element)))
               (element-defining-form-found?-set! element #t))
             (loop))))))))

(define (extract-module-docs! module)
  (for-each (lambda (path)
              (call-with-input-file path
                (lambda (port)
                  (extract-module-docs-from-port! module port path))))
            (module-associated-files module)))

(define (find-module name modules)
  (let loop ((m modules))
    (cond ((null? m)
           #f)
          ((equal? name (module-name (car m)))
           (car m))
          (else
           (loop (cdr m))))))

(define (perform-import! target source)
  (do ((exports (module-elements source) (cdr exports)))
      ((null? exports))
    (let* ((source-elem (car exports))
           (target-elem (find-element target (element-name source-elem))))
      ;; When destination doesn't contain an element of this name, it simply
      ;; means it doesn't re-export it.
      (when target-elem
        (element-copy! target-elem source-elem)))))

(define (resolve-import! module import-form modules)
  (case (car import-form)
    ((only except prefix rename)
     (warn "{:w}: {} import specifier unimplemented"
           (module-name module) (car import-form)))
    (else
     (let ((imported-module (find-module import-form modules)))
       (if imported-module
           (perform-import! module imported-module)
           (warn "{:w}: Unknown module {:w}"
                 (module-name module) import-form))))))

(define (resolve-imports! module all-modules)
  (unless (module-imports-resolved? module)
    (do ((import-forms (module-imports module) (cdr import-forms)))
        ((null? import-forms))
      (resolve-import! module (car import-forms) all-modules))
    (module-imports-resolved?-set! module #t)))

(define (module-name->file-name name)
  (string-append (string-join (map datum->string name) ".")
                 ".html"))

(define (html tag (attrs '()) . content)
  (let ((attrs* (string-join (map (lambda (pair)
                                    (format #R"({}="{}")" (car pair) (cdr pair)))
                                  attrs)
                             " "))
        (content* (string-join content)))
    (cond ((and (null? attrs) (string-null? content*))
           (format "<{}/>" tag))
          ((string-null? content*)
           (format "<{} {}>" tag attrs*))
          ((null? attrs)
           (format "<{}>{}</{0}>" tag content*))
          (else
           (format "<{} {}>{}</{0}>" tag attrs* content*)))))

(define (render-element-list module)
  (apply html "ul" '()
         (map (lambda (elem)
                (html "li" '()
                      (html "code" '() (datum->string (element-name elem)))))
              (module-elements module))))

(define (get-meta element name)
  (let ((meta (assq name (element-meta element))))
    (if meta (cdr meta) #f)))

(define (render-element-signature element)
  #;(unless (element-defining-form-found? element)
    (warn "{}: {}: No defining form found"
          (module-primary-file (element-module element))
          (element-name element)))

  (case (get-meta element 'kind)
    ((procedure)
     (html "div" '()
           "Procedure "
           (html "code" '()
                 (datum->string `(,(element-name element)
                                  . ,(get-meta element 'procedure-params))))))
    ((syntax)
     (html "div" '()
           "Syntax "
           (html "code" '() (datum->string (element-name element)))))
    ((parameter)
     (html "div" '()
           "Parameter "
           (html "code" '() (datum->string (element-name element)))))
    (else
     (html "div" '() "Unknown"))))

(define (render-element elem)
  (html "article" '()
        (html "h2" '() (datum->string (element-name elem)))
        (html "p" '() (render-element-signature elem))
        (html "p" '() (datum->string (element-meta elem)))
        (html "p" '() (datum->string (element-body elem)))))

(define (render-element-details module)
  (apply html "section" '()
         (map render-element (module-elements module))))

(define (render-module out-dir module)
  (call-with-output-file
      (path-append out-dir (module-name->file-name (module-name module)))
    (lambda (out)
      (write-string "<!DOCTYPE html>" out)
      (write-string
       (html "html" '((lang . "en"))
             (html "head" '()
                   (html "title" '() (datum->string (module-name module)))
                   (html "meta" '((charset . "utf-8"))))
             (html "body" '()
                   (html "h1" '() (datum->string (module-name module)))
                   (apply html "section" '()
                          (map datum->string (module-imports module)))
                   (render-element-list module)
                   (render-element-details module)))
       out))))

(define (show-usage-and-exit!)
  (printf "Usage: {} <output dir> <input paths ...>\n" (car (command-line)))
  (exit #f))

(when (< (length (command-line)) 3)
  (show-usage-and-exit!))

(let ((output-directory (cadr (command-line)))
      (input-paths (cddr (command-line))))
  (create-directories output-directory)
  (let* ((slds (apply append (map find-library-definitions input-paths)))
         (modules (map parse-sld slds)))
    (for-each extract-module-docs! modules)
    (for-each (lambda (m) (resolve-imports! m modules)) modules)
    (for-each (lambda (m) (render-module output-directory m)) modules)))

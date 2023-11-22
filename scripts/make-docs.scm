(import (scheme base) (scheme read) (scheme write) (scheme file)
        (scheme process-context) (insider filesystem) (insider format)
        (insider list) (insider struct) (insider string) (insider filesystem)
        (insider scribble) (insider basic-procedures) (insider io)
        (insider char))

(define <no-form> (list 'no-form))

(struct element (name
                 module
                 (meta '() #:mutable)
                 (body '() #:mutable)
                 #:defining-form-found? (defining-form-found? #f #:mutable)))

(define (element-copy! target source)
  (element-meta-set! target (element-meta source))
  (element-body-set! target (element-body source))
  (element-defining-form-found?-set! target
                                     (element-defining-form-found? source)))

(struct module (name
                (primary-file #f)
                (associated-files '() #:mutable)
                (imports '() #:mutable)
                #:imports-resolved? (imports-resolved? #f #:mutable)
                (elements '() #:mutable)
                (comment '() #:mutable)))

(define (append-elements! module new-elements)
  (module-elements-set! module (append (module-elements module) new-elements)))

(define (append-element! module new-element)
  (append-elements! module (list new-element)))

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

(define (strip-scheme-doc-comment-prefix line)
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
                 (cons
                  'doc-comment
                  (string-join
                   (map strip-scheme-doc-comment-prefix
                        (cons line (read-rest-of-scheme-doc-comment port)))
                   "\n"))
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

(define (find-element module name)
  (let loop ((elements (module-elements module)))
    (cond ((null? elements)
           #f)
          ((eq? (element-name (car elements)) name)
           (car elements))
          (else
           (loop (cdr elements))))))

(define (meta-push meta key value)
  (let ((pair (assq key meta)))
    (cond (pair
           (set-cdr! pair (append (cdr pair) (list value)))
           meta)
          (else
           (cons (cons key value) meta)))))

(define (syntax-meta meta form)
  (cons '(kind . syntax)
        (cons `(syntax-body . ,(cdr form))
              meta)))

(define meta-commands
  `((procedure . ,(lambda (meta form)
                    (cons '(kind . procedure) meta)))
    (arg . ,(lambda (meta form)
              (meta-push meta 'procedure-args (cdr form))))
    (variable . ,(lambda (meta form)
                   (cons '(kind . variable) meta)))
    (constant . ,(lambda (meta form)
                   (cons '(kind . constant) meta)))
    (parameter . ,(lambda (meta form)
                    (cons '(kind . parameter) meta)))
    (syntax . ,syntax-meta)
    (name . ,(lambda (meta form)
               (cons `(name . ,(cadr form)) meta)))
    (module . ,(lambda (meta form) meta))
    (in-group . ,(lambda (meta form)
                   (when (assq 'group meta)
                     (error "@in-group specified more than once for an element"))
                   (cons `(group . ,(cadr form)) meta)))))

(define (parse-doc-scribble scribble)
  (let loop ((scribble scribble)
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

(define (find-procedure-args form)
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

(define (update-element-meta-from-form meta form name)
  (define (push-meta! key value)
    (set! meta (cons (cons key value) meta)))

  (unless (assq 'kind meta)
    (case (car form)
      ((define)
       (if (find-procedure-args form)
           (push-meta! 'kind 'procedure)
           (push-meta! 'kind 'variable)))
      ((define-syntax)
       (push-meta! 'kind 'syntax))
      (else
       (warn "{}: Unknown form: {:w}" name form))))

  (let ((kind (assq 'kind meta)))
    (when (and kind
               (eq? (cdr kind) 'procedure)
               (not (assq 'procedure-args meta)))
      (let ((args (find-procedure-args form)))
        (if args
            (push-meta! 'procedure-args args)
            (warn "{}: Cannot deduce procedure arguments" name)))))

  meta)

(define (find-define-name form)
  ;; Can be either (define name ...) or (define (name ...) ...) or
  ;; (define ((name ...) ...) ...) or ...
  (let loop ((form (cadr form)))
    (cond ((symbol? form) form)
          ((pair? form)   (loop (car form)))
          (else           #f))))

(define (find-element-for-definition module form)
  (and (pair? form)
       (memq (car form) '(define define-syntax))
       (pair? (cdr form))
       (find-element module (find-define-name form))))

(define (find-element-name form meta)
  (cond ((assq 'name meta) => cdr)
        (else (find-define-name form))))

(define (set-element-doc-and-form! module form comment path)
  (let-values (((meta-scribble body)
                (parse-doc-scribble
                 (scribble-parse (open-input-string comment)))))
    (let* ((meta (parse-element-meta meta-scribble))
           (name (find-element-name form meta))
           (element (and name (find-element module name))))
      (cond (element
             (element-meta-set! element
                                (update-element-meta-from-form
                                 meta
                                 form
                                 (element-name element)))
             (element-body-set! element body)
             (element-defining-form-found?-set! element #t))
            (else
             (warn "{}: Documentation comment ignored for {:w}"
                   path form))))))

(define (extract-scheme-module-docs-from-port! module port path)
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
                                  (update-element-meta-from-form
                                   '()
                                   form
                                   (element-name element)))
               (element-defining-form-found?-set! element #t))
             (loop))))))))

(define (extract-scheme-module-docs! module)
  (for-each (lambda (path)
              (call-with-input-file path
                (lambda (port)
                  (extract-scheme-module-docs-from-port! module port path))))
            (module-associated-files module)))

(define c++-doc-comment-prefix "//>")
(define c++-doc-comment-prefix-length (string-length c++-doc-comment-prefix))

(define (c++-doc-comment-line? line)
  (string-prefix? c++-doc-comment-prefix line))

(define (strip-c++-doc-comment-prefix line)
  (string-drop line c++-doc-comment-prefix-length))

(define (read-rest-of-c++-doc-comment port)
  (let loop ((result '()))
    (let ((line (read-line port)))
      (if (and line (c++-doc-comment-line? line))
          (loop (cons line result))
          (reverse result)))))

(define (read-c++-comment port)
  (let loop ()
    (skip-whitespace! port)
    (let ((line (read-line port)))
      (cond ((eof-object? line)
             line)
            ((c++-doc-comment-line? line)
             (string-join (map strip-c++-doc-comment-prefix
                               (cons line (read-rest-of-c++-doc-comment port)))
                          "\n"))
            (else
             (loop))))))

(define (read-c++-file port)
  (let loop ((comments '()))
    (let ((comment (read-c++-comment port)))
      (if (eof-object? comment)
          (reverse comments)
          (loop (cons comment comments))))))

(define (find-c++-module-declaration-in-comment scrbl)
  (let loop ((scrbl scrbl))
    (cond ((null? scrbl)
           #f)
          ((pair? (car scrbl))
           (if (eq? (caar scrbl) 'module)
               (cadar scrbl)
               (loop (cdr scrbl))))
          (else
           (loop (cdr scrbl))))))

(define (find-c++-module-declaration scribbles)
  (let loop ((scribbles scribbles))
    (if (null? scribbles)
        #f
        (let ((decl (find-c++-module-declaration-in-comment (car scribbles))))
          (or decl (loop (cdr scribbles)))))))

(define (find-module name modules)
  (let loop ((m modules))
    (cond ((null? m)
           #f)
          ((equal? name (module-name (car m)))
           (car m))
          (else
           (loop (cdr m))))))

(define (find-or-create-c++-module name modules)
  (let ((mod (find-module name modules)))
    (if mod
        (values mod modules)
        (let ((mod (module name #:imports-resolved? #t)))
          (values mod (cons mod modules))))))

(define (parse-c++-module! mod scribbles path)
  (append-associated-files! mod (list path))
  (let loop ((scrbl scribbles))
    (unless (null? scrbl)
      (let-values (((meta-scrbl body) (parse-doc-scribble (car scrbl))))
        (let ((meta (parse-element-meta meta-scrbl)))
          (cond
           ((assq 'name meta)
            => (lambda (name)
                 (append-element! mod (element (cdr name) mod meta body
                                               #:defining-form-found? #t))))
           (else
            (append-module-comment! mod body)))))
      (loop (cdr scrbl)))))

(define (parse-c++ path modules)
  (let ((port (open-input-file path)))
    (let ((comments (read-c++-file port)))
      (cond
       ((null? comments)
        modules)
       (else
        (let* ((scribbles (map (lambda (c)
                                 (scribble-parse (open-input-string c)))
                               comments))
               (name (find-c++-module-declaration scribbles)))
          (let-values (((mod modules*) (find-or-create-c++-module name modules)))
            (parse-c++-module! mod scribbles path)
            modules*)))))))

(define (perform-import! target import-set)
  (do ((imports import-set (cdr imports)))
      ((null? imports))
    (let* ((import (car imports))
           (target-name (car import))
           (source-elem (cdr import))
           (target-elem (find-element target target-name)))
      ;; When destination doesn't contain an element of this name, it simply
      ;; means it doesn't re-export it.
      (when target-elem
        (element-copy! target-elem source-elem)))))

(define (make-import-set import-form modules importing-module-name)
  (case (car import-form)
    ((only)
     (let ((import-form* (cadr import-form))
           (identifiers (cddr import-form)))
       (filter (lambda (elem) (memq (car elem) identifiers))
               (make-import-set import-form* modules
                                importing-module-name))))
    ((except)
     (let ((import-form* (cadr import-form))
           (identifiers (cddr import-form)))
       (filter (lambda (elem) (not (memq (car elem) identifiers)))
               (make-import-set import-form* modules
                                importing-module-name))))
    ((rename)
     (let ((import-form* (cadr import-form))
           (renames (cddr import-form)))
       (map (lambda (elem)
              (let ((exported-name (car elem)) (element (cdr elem)))
                (cond ((assq exported-name renames)
                       => (lambda (rename-pair)
                            (cons (cdr rename-pair) element)))
                      (else
                       elem))))
            (make-import-set import-form* modules
                             importing-module-name))))
    ((prefix)
     (let ((import-form* (cadr import-form))
           (prefix (symbol->string (caddr import-form))))
       (map (lambda (elem)
              (cons (string->symbol (string-append prefix
                                                   (symbol->string (car elem))))
                    (cdr elem)))
            (make-import-set import-form* modules
                             importing-module-name))))
    (else
     (let ((imported-module (find-module import-form modules)))
       (cond (imported-module
              (map (lambda (elem) (cons (element-name elem) elem))
                   (module-elements imported-module)))
             (else
              (warn "{:w}: Unknown module {:w}"
                    importing-module-name import-form)
              '()))))))

(define (resolve-import! module import-form modules)
  (perform-import! module (make-import-set import-form modules
                                           (module-name module))))

(define (resolve-imports! module all-modules)
  (unless (module-imports-resolved? module)
    (do ((import-forms (module-imports module) (cdr import-forms)))
        ((null? import-forms))
      (resolve-import! module (car import-forms) all-modules))
    (module-imports-resolved?-set! module #t)))

(define (module-name->file-name name)
  (string-append (string-join (map datum->string name) ".")
                 ".html"))

(define (escape-html s)
  (let ((out (open-output-string)))
    (string-for-each-cursor
     (lambda (cursor)
       (let ((c (string-ref s cursor)))
         (case c
           ((#\<) (write-string "&lt;" out))
           ((#\>) (write-string "&gt;" out))
           ((#\&) (write-string "&amp;" out))
           (else (write-char c out)))))
     s)
    (get-output-string out)))

(define (sxml->string sxml)
  (define (f expr)
    (cond
     ((string? expr)
      (escape-html expr))
     ((pair? expr)
      (let* ((has-attrs? (and (not (null? (cdr expr)))
                              (pair? (cadr expr))
                              (eq? (caadr expr) '@)))
             (attrs (if has-attrs? (cdadr expr) '()))
             (body (if has-attrs? (cddr expr) (cdr expr)))
             (attrs* (string-join (map (lambda (attr)
                                         (format #R"({}="{}")"
                                                 (car attr) (cadr attr)))
                                       attrs)
                                  " "))
             (body* (string-join (map f body)))
             (tag (car expr)))
        (cond ((and (string-null? attrs*) (null? body))
               (format "<{}/>" tag))
              ((null? body)
               (format "<{} {}>" tag attrs*))
              ((string-null? attrs*)
               (format "<{}>{}</{0}>" tag body*))
              (else
               (format "<{} {}>{}</{0}>" tag attrs* body*)))))
     (else
      (error "Invalid SXML" expr))))

  ;; The argument is expected to be of the form (*TOP* stuff ...).
  (string-join (map f (cdr sxml))))

(define (render-element-list module)
  `(div (@ (class "nav-left"))
        (ul ,@(map (lambda (elem)
                     (let ((name (datum->string (element-name elem))))
                       `(li
                         (a (@ (href ,(string-append "#" name)))
                            (code ,name)))))
                   (module-elements module)))))

(define (get-meta element name)
  (let ((meta (assq name (element-meta element))))
    (if meta (cdr meta) #f)))

(define (datum->string/quote datum)
  (call-with-output-string
   (lambda (out)
     (let f ((datum datum))
       (cond
        ((pair? datum)
         (cond ((and (eq? (car datum) 'quote)
                     (pair? (cdr datum)))
                (write-char #\' out)
                (f (cadr datum)))
               (else
                (write-char #\( out)
                (let loop ((elem datum))
                  (f (car elem))
                  (cond ((null? (cdr elem))
                         #void)
                        ((pair? (cdr elem))
                         (loop (cdr elem)))
                        (else
                         (write-string " . " out)
                         (f (cdr elem)))))
                (write-char #\) out))))
        ((vector? datum)
         (write-string "#(" out)
         (do ((i 0 (+ i 1)))
             ((= i (vector-length datum)))
           (f (vector-ref datum i)))
         (write-char #\) out))
        (else
         (write datum out)))))))

(define (render-procedure-argument arg)
  (cond ((symbol? arg)
         `(span (@ (class "element-signature-argument"))
                ,(symbol->string arg)))
        ((keyword? arg)
         `(span (@ (class "element-signature-argument-keyword"))
                "#:" ,(keyword->string arg)))
        (else
         ;; Argument with default value expression
         (let ((name (car arg)) (expr (cadr arg)))
           `(span "("
                  (span (@ (class "element-signature-argument"))
                        ,(symbol->string name))
                  " "
                  (span (@ (class "element-signature-default-expr"))
                        ,(datum->string/quote expr))
                  ")")))))

(define (render-procedure-arguments args)
  ;; args may be an improper list, or indeed just a symbol.
  (cond ((null? args)
         '())
        ((symbol? args)
         `(" . " ,(render-procedure-argument args)))
        ((pair? args)
         `(" "
           ,(render-procedure-argument (car args))
           ,@(render-procedure-arguments (cdr args))))
        (else
         (error "Bad element in procedure arguments" args))))

(define (render-procedure-arguments/wide args (break-line? #f))
  (let ((br (if break-line? '(br) "")))
    (cond ((null? args)
           '())
          ((symbol? args)
           `(,br
             (div (@ (class "element-signature-argument-container"))
                   " . " ,(render-procedure-argument args))))
          ((and (pair? args) (keyword? (car args)))
           `(,br
             (div (@ (class "element-signature-argument-container"))
                   ,(render-procedure-argument (car args))
                   " "
                   ,(render-procedure-argument (cadr args)))
             ,@(render-procedure-arguments/wide (cddr args) #t)))
          ((pair? args)
           `(,br
             (div (@ (class "element-signature-argument-container"))
                   ,(render-procedure-argument (car args)))
             ,@(render-procedure-arguments/wide (cdr args) #t)))
          (else
           (error "Bad element in procedure arguments" args)))))

(define (procedure-signature-length element)
  (string-length (datum->string (cons (element-name element)
                                      (get-meta element 'procedure-args)))))

(define (render-procedure-signature element)
  (if (< (procedure-signature-length element) 60)
      `(code "("
             (span (@ (class "element-signature-name"))
                   ,(symbol->string (element-name element)))
             ,@(render-procedure-arguments (get-meta element 'procedure-args))
             ")")
      `(code
        (div (@ (class "element-signature-container"))
             (div "("
                  (span (@ (class "element-signature-name"))
                        ,(symbol->string (element-name element))))
             (div ,@(render-procedure-arguments/wide
                     (get-meta element 'procedure-args))
                  ")")))))

(define (render-syntax-nonterminal-name name)
  (define (meta-char? c)
    (char=? c #\_))

  (let loop ((accum '())
             (current (string-cursor-start name))
             (end (string-cursor-end name)))
    (cond
     ((string-cursor=? current end)
      (reverse accum))
     (else
      (let ((current-char (string-ref/cursor name current)))
        (case current-char
          ((#\_)
           (let ((next (string-cursor-next name current)))
             (loop (cons `(sub ,(string (string-ref/cursor name next)))
                         accum)
                   (string-cursor-next name next)
                   end)))
          (else
           (let ((run-end (string-index name meta-char? current end)))
             (loop (cons (substring name current run-end) accum)
                   run-end
                   end)))))))))

(define (render-syntax-nonterminal nonterm)
  `(span (@ (class "nonterminal"))
         ,@(render-syntax-nonterminal-name nonterm)))

(define (render-syntax-body-element elem)
  (cond ((string? elem)
         `(,(render-syntax-nonterminal elem)))
        ((pair? elem)
         (case (car elem)
           ((repeated)
            `(,@(render-syntax-body (cdr elem))
              " …"))
           ((optional)
            `("["
              ,@(render-syntax-body (cdr elem))
              "]"))
           (else
            (error "Unknown tag in syntax nonterminal" (car elem)))))
        (else
         (error "Unknown element in syntax nonterminal" elem))))

(define (flatten/intersperse lists separator)
  (if (null? lists)
      '()
      (append (car lists)
              (flatten/intersperse (cdr lists) separator))))

(define (render-syntax-body body)
  (let ((rendered-elems (map render-syntax-body-element body)))
    (flatten/intersperse rendered-elems " ")))

(define (render-syntax-signature element)
  (let ((name (datum->string (element-name element)))
        (body (get-meta element 'syntax-body)))
    (cond
     (body
      `(code "("
             (span (@ (class "element-signature-name")) ,name)
             " "
             ,@(render-syntax-body body)
             ")"))
     (else
      `(code (span (@ (class "element-signature-name")) ,name))))))

(define (render-element-signature element)
  #;(unless (element-defining-form-found? element)
    (warn "{}: {}: No defining form found"
          (module-primary-file (element-module element))
          (element-name element)))

  (case (get-meta element 'kind)
    ((procedure)
     (render-procedure-signature element))
    ((syntax)
     (render-syntax-signature element))
    (else
     `(code (span (@ (class "element-signature-name"))
                  ,(datum->string (element-name element)))))))

(define (render-element-kind element)
  (case (get-meta element 'kind)
    ((procedure) "procedure")
    ((syntax)    "syntax")
    ((parameter) "parameter")
    ((variable)  "variable")
    ((constant)  "constant")
    (else        "unknown")))

(define (render-scribble-element env scrbl)
  (cond ((string? scrbl)
         scrbl)
        ((pair? scrbl)
         (cond ((assq (car scrbl) env)
                => (lambda (tag) ((cdr tag) scrbl)))
               (else
                (warn "Unknown Scribble tag {:w}" (car scrbl))
                `(code ,(datum->string scrbl)))))
        (else
         (warn "Unknown Scribble element {:w}" scrbl)
         `(code ,(datum->string scrbl)))))

(define (render-scribble env)
  (lambda (scrbl)
    (map (lambda (s) (render-scribble-element env s))
         scrbl)))

(define (indent-width line)
  (string-cursor-diff line (string-cursor-start line)
                      (string-skip line char-whitespace?)))

(define (line-empty? x)
  (string-null? (string-trim x)))

(define (code-lines scrbl)
  (define (drop lines)
    (do ((lines lines (cdr lines)))
        ((or (null? lines) (not (line-empty? (car lines))))
         lines)))
  (reverse (drop (reverse (drop (filter string? scrbl))))))

(define scribble-code-tags
  `((evaluates-to . ,(lambda (scrbl)
                       `(span (@ (class "evaluates-to"))
                              (span (@ (class "symbol")) "⟹")
                              " "
                              ,@(render-code (cdr scrbl)))))))

(define render-code (render-scribble scribble-code-tags))

(define (scribble-code scrbl)
  (let* ((lines (code-lines (cdr scrbl)))
         (non-empty-lines (filter (lambda (s)
                                    (not (string-null? (string-trim s))))
                                  lines)))
    (cond ((null? non-empty-lines)
           '(pre (@ (class "code")) ""))
          (else
           (let* ((indent-width (fold (lambda (line width)
                                        (min width (indent-width line)))
                                      (indent-width (car non-empty-lines))
                                      (cdr non-empty-lines)))
                  (unindent (lambda (s)
                              (cond
                               ((not (string? s))
                                (render-scribble-element scribble-code-tags s))
                               ((>= (string-length s) indent-width)
                                (string-drop s indent-width))
                               (else
                                "\n")))))
             `(pre (@ (class "code"))
                   ,@(map unindent (cdr scrbl))))))))

(define scribble-tags
  `((c . ,(lambda (scrbl) `(code ,@(render-body (cdr scrbl)))))
    (code . ,scribble-code)
    (example . ,(lambda (scrbl)
                  `(div (@ (class "example"))
                        (div (@ (class "example-header"))
                             "Example")
                        ,@(render-body (cdr scrbl)))))))

(define render-body (render-scribble scribble-tags))

(define (render-element-body elem)
  (render-body (element-body elem)))

(define (render-element-header elem)
  `(div (@ (class "element-header"))
        (div (@ (class "element-signature"))
             ,(render-element-signature elem))
        (div (@ (class "element-kind"))
             ,(render-element-kind elem))))

(define (render-element-anchors elem)
  `(a (@ (id ,(datum->string (element-name elem)))) ""))

(define (render-element-group group)
  `(div (@ (class "element-group"))
        ,@(map render-element-anchors group)
        (div (@ (class "element"))
             ,@(map render-element-header group)
             (div (@ (class "element-description"))
                  ""
                  ,@(apply append
                           (map render-element-body group))))))

(define (make-element-groups elements)
  (let loop ((elements elements) (group-alist '()))
    (cond ((null? elements)
           (reverse (map cdr group-alist)))
          ((get-meta (car elements) 'group)
           => (lambda (group-name)
                (cond ((assq group-name group-alist)
                       => (lambda (group-pair)
                            (set-cdr! group-pair
                                      (append (cdr group-pair)
                                              (list (car elements))))
                            (loop (cdr elements) group-alist)))
                      (else
                       (loop (cdr elements)
                             (cons (cons group-name (list (car elements)))
                                   group-alist))))))
          (else
           (loop (cdr elements)
                 (cons (cons #f (list (car elements)))
                       group-alist))))))

(define (render-element-details module)
  `(div ,@(map render-element-group
               (make-element-groups (module-elements module)))))

(define (render-module out-dir module)
  (call-with-output-file
      (path-append out-dir (module-name->file-name (module-name module)))
    (lambda (out)
      (write-string "<!DOCTYPE html>" out)
      (write-string
       (sxml->string
        `(*TOP*
          (html (@ (lang "en"))
                (head (title ,(datum->string (module-name module)))
                      (meta (@ (charset "utf-8")))
                      (link (@ (rel "stylesheet")
                               (href "style.css"))))
                (body ,(render-element-list module)
                      (div (@ (class "main-content"))
                           (h1 ,(datum->string (module-name module)))
                           ,(render-element-details module))))))
       out))))

(define (find-files-recursive path extension)
  (cond ((directory? path)
         (filter (lambda (p) (string=? (path-extension p) extension))
                 (directory-files/recursive path
                                            #:follow-symlinks? #t)))
        ((string=? (path-extension path) extension)
         (list path))
        (else
         '())))

(define (find-library-definitions path)
  (find-files-recursive path ".sld"))

(define (find-c++-files path)
  (append (find-files-recursive path ".cpp")
          (find-files-recursive path ".hpp")))

(define (show-usage-and-exit!)
  (printf "Usage: {} <output dir> <static files dir> <input paths ...>\n"
          (car (command-line)))
  (exit #f))

(when (< (length (command-line)) 4)
  (show-usage-and-exit!))

(let ((output-directory (cadr (command-line)))
      (statics-dir (caddr (command-line)))
      (input-paths (cdddr (command-line))))
  (create-directories output-directory)
  (copy-files statics-dir output-directory
              #:recursive? #t
              #:when-exists 'overwrite)
  (let* ((slds (apply append (map find-library-definitions input-paths)))
         (scheme-modules (map parse-sld slds))
         (c++-modules (fold parse-c++ '()
                            (apply append (map find-c++-files input-paths)))))
    (for-each extract-scheme-module-docs! scheme-modules)
    (let ((modules (append scheme-modules c++-modules)))
      (for-each (lambda (m) (resolve-imports! m modules)) modules)
      (for-each (lambda (m) (render-module output-directory m)) modules))))

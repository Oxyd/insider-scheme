(import (scheme base) (scheme read) (scheme write) (scheme file)
        (scheme process-context) (insider filesystem) (insider format)
        (insider list) (insider struct) (insider string) (insider filesystem))

(define (show-usage-and-exit!)
  (printf "Usage: {} <output dir> <input dirs ...>\n" (car (command-line)))
  (exit #f))

(define (find-library-definitions dir)
  (filter (lambda (p) (string=? (path-extension p) ".sld"))
          (directory-files/recursive dir #:follow-symlinks? #t)))

(struct module (name (exports #:mutable)))

(define (append-exports! module new-exports)
  (module-exports-set! module (append (module-exports module) new-exports)))

(define (warn message . args)
  (apply printf (string-append "Warning: " message) args))

(define (parse-library-directive! directive module)
  (unless (null? directive)
    (case (car directive)
      ((export)
       (append-exports! module (cdr directive))))))

(define (parse-module-definition path def)
  (cond
   ((or (null? def)
        (null? (cdr def))
        (not (eq? (car def) 'define-library)))
    (warn "{}: Invalid library file" path)
    #f)
   (else
    (do ((result (module (cadr def) '()))
         (current (cddr def) (cdr current)))
        ((null? current) result)
      (parse-library-directive! (car current) result)))))

(define (parse-sld path)
  (call-with-input-file path
    (lambda (port)
      (parse-module-definition path (read port)))))

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
                   (apply html "ul" '()
                          (map (lambda (export)
                                 (html "li" '()
                                       (html "pre" '() (datum->string export))))
                               (module-exports module)))))
       out))))

(when (< (length (command-line)) 3)
  (show-usage-and-exit!))

(let ((output-directory (cadr (command-line)))
      (input-directories (cddr (command-line))))
  (create-directories output-directory)
  (let* ((slds (apply append (map find-library-definitions input-directories)))
         (modules (map parse-sld slds)))
    (for-each (lambda (m) (render-module output-directory m)) modules)))

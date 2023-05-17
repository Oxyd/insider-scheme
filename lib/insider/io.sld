(define-library (insider io)
  (import (insider syntax) (insider control) (insider syntax) (insider error)
          (insider list) (insider string) (insider char)
          (insider basic-procedures) (insider numeric) (insider bytevector)
          (rename (only (insider internal)
                        read-char peek-char read-line read-u8 peek-u8 write-u8
                        write-char flush-output-port char-ready? u8-ready?
                        current-input-port-tag current-output-port-tag
                        current-error-port-tag current-source-file-origin-tag
                        port-open? close-port open-input-file
                        open-binary-input-file open-input-string
                        open-input-bytevector open-output-file
                        open-binary-output-file open-output-string
                        open-output-bytevector close-input-port
                        close-output-port display get-output-bytevector
                        get-output-string newline open-source-file-relative read
                        read-syntax read-syntax-multiple read-syntax-multiple-ci
                        write write-shared write-simple file-exists? delete-file
                        current-working-directory set-current-working-directory!
                        <eof-object>)
                  (flush-output-port %flush-output-port)
                  (read-char %read-char)
                  (peek-char %peek-char)
                  (read-line %read-line)
                  (write-char %write-char)
                  (read-u8 %read-u8)
                  (peek-u8 %peek-u8)
                  (write-u8 %write-u8)
                  (char-ready? %char-ready?)
                  (u8-ready? %u8-ready?)))
  (export binary-port? call-with-input-file call-with-input-string
          call-with-output-file call-with-output-string call-with-port
          char-ready? close-input-port close-output-port close-port
          current-error-port current-input-port current-output-port
          current-source-file-origin delete-file display eof-object eof-object?
          file-exists? flush-output-port flush-output-port get-output-bytevector
          get-output-string input-port-open? input-port? newline
          open-input-bytevector open-input-file open-input-string
          open-binary-input-file open-binary-output-file open-output-bytevector
          open-output-file open-output-string open-source-file-relative
          output-port-open? output-port? peek-char peek-u8 port? read
          read-bytevector read-bytevector! read-char read-line read-string
          read-syntax read-syntax-multiple read-syntax-multiple-ci read-u8
          textual-port? u8-ready? with-input-from-file with-output-to-file write
          write-bytevector write-char write-shared write-simple write-string
          write-u8 current-working-directory set-current-working-directory!)
  (include "io.scm"))

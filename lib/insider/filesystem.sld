(define-library (insider filesystem)
  (import (insider internal))
  (export path-elements path-append path-root-name path-root-directory
          path-root-path path-relative-path path-parent path-filename
          path-stem path-extension absolute-path canonical-path
          weakly-canonical-path relative-path proximate-path
          lexically-normal-path lexically-relative-path lexically-proximate-path
          file-exists? delete-file current-path set-current-path!))

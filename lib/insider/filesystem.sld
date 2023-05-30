(define-library (insider filesystem)
  (import (except (insider internal) bitwise-ior)
          (only (insider numeric) bitwise-ior))
  (export path-elements path-append path-root-name path-root-directory
          path-root-path path-relative-path path-parent path-filename
          path-stem path-extension absolute-path canonical-path
          weakly-canonical-path relative-path proximate-path
          lexically-normal-path lexically-relative-path lexically-proximate-path
          file-exists? delete-file current-path set-current-path!
          file-status symlink-status file-status? file-status-permissions
          file-status-type

          permissions-none permissions-owner-read permissions-owner-write
          permissions-owner-exec permissions-owner-all permissions-group-read
          permissions-group-write permissions-group-exec permissions-group-all
          permissions-others-read permissions-others-write permissions-others-exec
          permissions-others-all permissions-all permissions-set-uid
          permissions-set-gid permissions-sticky-bit permissions-mask

          block-file? character-file? directory? fifo? file-other? regular-file?
          socket? symlink?)
  (include "filesystem.scm"))

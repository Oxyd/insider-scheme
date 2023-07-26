(define-library (insider filesystem)
  (import (scheme base) (insider numeric)
          (rename (only (insider internal)
                        path-elements path-append path-root-name
                        path-root-directory path-root-path path-relative-path
                        path-parent path-filename path-remove-filename
                        path-replace-filename path-replace-extension path-stem
                        path-extension absolute-path canonical-path
                        weakly-canonical-path relative-path proximate-path
                        lexically-normal-path lexically-relative-path
                        lexically-proximate-path file-exists? delete-file
                        delete-all-files current-path set-current-path!
                        file-status symlink-status block-file? character-file?
                        directory? fifo? file-other? regular-file? socket?
                        symlink? copy-regular-file copy-files read-symlink
                        create-symlink create-hard-link create-directory-symlink
                        copy-symlink create-directory create-directories
                        files-equivalent? file-size hard-link-count
                        last-write-time set-last-write-time! set-permissions!
                        add-permissions! remove-permissions! rename-file
                        resize-file filesystem-space temporary-directory-path
                        directory-files directory-files/recursive)
                  (copy-regular-file copy-regular-file*)
                  (copy-files copy-files*)
                  (create-directory create-directory*)
                  (set-permissions! set-permissions!*)
                  (add-permissions! add-permissions!*)
                  (remove-permissions! remove-permissions!*)
                  (directory-files/recursive directory-files/recursive*)))
  (export path-elements path-append path-root-name path-root-directory
          path-root-path path-relative-path path-parent path-filename
          path-remove-filename path-replace-filename path-replace-extension
          path-stem path-extension absolute-path canonical-path
          weakly-canonical-path relative-path proximate-path
          lexically-normal-path lexically-relative-path lexically-proximate-path
          file-exists? delete-file delete-all-files current-path
          set-current-path! file-status symlink-status copy-regular-file
          copy-files read-symlink create-symlink create-hard-link
          create-directory-symlink copy-symlink create-directory
          create-directories files-equivalent? file-size hard-link-count
          last-write-time set-last-write-time! set-permissions! add-permissions!
          remove-permissions! rename-file resize-file filesystem-space
          temporary-directory-path directory-files directory-files/recursive

          permissions-none permissions-owner-read permissions-owner-write
          permissions-owner-exec permissions-owner-all permissions-group-read
          permissions-group-write permissions-group-exec permissions-group-all
          permissions-others-read permissions-others-write permissions-others-exec
          permissions-others-all permissions-all permissions-set-uid
          permissions-set-gid permissions-sticky-bit permissions-mask

          block-file? character-file? directory? fifo? file-other? regular-file?
          socket? symlink?)
  (include "filesystem.scm"))

(define-library (insider control)
  (import (insider syntax)
          (insider basic-procedures)
          (insider list)
          (insider error)
          (only (insider internal)
                eq? capture-stack replace-stack! create-parameter-tag
                find-parameter-value set-parameter-value! call-parameterized
                apply values call-with-values with-exception-handler raise
                raise-continuable dynamic-wind main-module?-tag environment
                interactive-environment eval meta current-expand-module-tag
                dynamic-import interaction-environment-specifier-tag
                call-with-continuation-barrier))
  (export call-with-current-continuation call/cc let/cc make-parameter
          make-parameter-from-tag parameterize apply values call-with-values
          with-exception-handler raise raise-continuable dynamic-wind guard
          when-main-module environment interactive-environment eval meta
          current-expand-module dynamic-import interaction-environment-specifier
          call-with-continuation-barrier)
  (include "control.scm"))

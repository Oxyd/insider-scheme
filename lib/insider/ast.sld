(define-library (insider ast)
  (import
   (scheme base)
   (scheme write)
   (insider string)
   (only (insider internal)
         literal-expression? literal-expression-value
         local-reference-expression? local-reference-expression-variable
         top-level-reference-expression? top-level-reference-expression-variable
         unknown-reference-expression? unknown-reference-expression-name
         application-expression? application-expression-target
         application-expression-arguments
         sequence-expression? sequence-expression-expressions
         let-expression? let-expression-definitions let-expression-body
         local-set!-expression? local-set!-expression-target
         local-set!-expression-expression
         top-level-set!-expression? top-level-set!-expression-target
         top-level-set!-expression-expression
         lambda-expression? lambda-expression-parameters
         lambda-expression-has-rest? lambda-expression-body lambda-expression-name
         lambda-expression-free-variables
         if-expression? if-expression-test if-expression-consequent
         if-expression-alternative
         local-variable? local-variable-name local-variable-set?
         local-variable-constant-initialiser
         top-level-variable? top-level-variable-name top-level-variable-set?
         top-level-variable-constant-initialiser
         analyse analyse-module))
  (export literal-expression? literal-expression-value
          local-reference-expression? local-reference-expression-variable
          top-level-reference-expression? top-level-reference-expression-variable
          unknown-reference-expression? unknown-reference-expression-name
          application-expression? application-expression-target
          application-expression-arguments
          sequence-expression? sequence-expression-expressions
          let-expression? let-expression-definitions let-expression-body
          local-set!-expression? local-set!-expression-target
          local-set!-expression-expression
          top-level-set!-expression? top-level-set!-expression-target
          top-level-set!-expression-expression
          lambda-expression? lambda-expression-parameters
          lambda-expression-has-rest? lambda-expression-body lambda-expression-name
          lambda-expression-free-variables
          if-expression? if-expression-test if-expression-consequent
          if-expression-alternative
          local-variable? local-variable-name local-variable-set?
          local-variable-constant-initialiser
          top-level-variable? top-level-variable-name top-level-variable-set?
          top-level-variable-constant-initialiser
          analyse analyse-module

          expression? variable? pretty-print-ast)
  (include "ast.scm"))

;;;; Types
(define-interface type-structure-parser-interface
  (export parse
          structure->grouped-bindings
          alias-everything

          scheme-binding?
          identifier?
          nested?
          aliased?
          anonymous-aliased?

          type-structure-syntax-error
          type-structure-syntax-error?
          parse-error
          (descend :syntax)
          pretty-path))

(define-interface type-destructuring-interface
  (export (destructure :syntax)
          (with-destructured :syntax)))
(define-interface comp/interface-creator-interface
  (export (define-interface-maker :syntax)
          interface?
          
          ref-method
          fold-method
          simple-name
          interface-type

          undeclared-type
          
          interface-error
          interface-error?))

(define-interface comp/interfaces-interface
  (export make-interface
          make-simple-interface
          make-compound-interface
          type->interface
          s48->interface
          interface-ref
          interface-member?
          interface?
          for-each-declaration
          fold-declarations))

(define-interface comp/structure-interface
  (export make-structure
          s48->structure
          structure?
          structure-lookup))

(define-interface comp/structure-reflection-interface
  (export structure-name
          structure-interface
          structure-bindings))

(define-interface comp/module-language-interface
  (export ((define-interface
             export
             compound-interface

             subset
             with-prefix
             modify)
           :syntax)

          prefix
          expose
          hide
          alias
          rename))

(define-interface comp/module-language-creator-interface
  (export fold->new
          add-name
          interface-name
          (let-interface-name :syntax)))





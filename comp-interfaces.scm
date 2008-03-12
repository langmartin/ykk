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

(define-interface identifier-interface
  (export identifier
          identifier?
          new-identifier))

(define-interface store-interface
  (export allocate))

(define-interface low-stob-interface
  (export stob-ref
          stob-set!
          stob-size))

(define-interface stob-type-interface
  (export stob-type?
          make-stob-type
          merge-type-descriptions
          stob-type-slot-index
          stob-type-self-index
          make-monomorphic-stob-predicate
          partial-initializer
          stob-type-name
          stob-type-self
          stob-type-self-size
          stob-type-slots
          stob-type-slots-size))

(define-interface stob-interface
  (export stob-error
          stob-error?
          stob?
          make-stob
          stob-identifier
          stob-type))

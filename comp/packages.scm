(define-structure comp/s48-dependencies
  (export undeclared-type

          s48:interface?
          s48:interface-ref
          s48:for-each-declaration

          s48:structure-name
          s48:structure-interface
          s48:structure?
          s48:structure-lookup
          
          s48:get-structure
          s48:get-interface

          ;; for tests
          s48:make-simple-interface)
    
  (open (subset meta-types (undeclared-type))
        (with-prefix interfaces s48:)
        (with-prefix packages s48:)
        (with-prefix packages-internal s48:)

        ;; for procedures below
        scheme
        environments
        package-commands-internal)

  (begin
    (define s48:get-structure get-structure)
    (define (s48:get-interface name)
      (environment-ref (config-package) name))))

;;;; Modules
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

(define-structures ((comp/interfaces comp/interfaces-interface)
                    (comp/interface-creator comp/interface-creator-interface))  
  (open scheme
        srfi-1 srfi-8 srfi-9+
        types
        ykk/types type-reflection
        assert
        fluids+
        exceptions
        comp/s48-dependencies)
  
  (files (comp interface)))

(define-interface comp/structure-interface
  (export make-structure
          s48->structure
          structure?
          structure-lookup))

(define-interface comp/structure-reflection-interface
  (export structure-name
          structure-interface
          structure-bindings))

(define-structures ((comp/structure comp/structure-interface)
                    (comp/structure-reflection comp/structure-reflection-interface))
  (open scheme
        srfi-8 srfi-9+
        assert
        ykk/names ; FIXME: make this comp/names
        comp/interfaces comp/interface-creator
        comp/s48-dependencies)
  (files (comp package)))

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

(define-structures ((comp/module-language comp/module-language-interface)
                    (comp/module-language-create comp/module-language-creator-interface))
  (open scheme
        srfi-1 srfi-8
        assert
        fluids+
        exceptions
        big-util ; concatenate-symbol
        ykk/names ; for testing
        comp/interfaces comp/interface-creator
        comp/structure comp/structure-reflection
        comp/s48-dependencies)
  (files (comp module-language)))

;;;; Graph / Compiler
(define-interface identifier-interface
  (export identifier
          identifier?
          new-identifier))

(define-structure identifier identifier-interface
  (open extra-scheme
        uuidgen)
  (files (comp identifier)))

(define-interface stob-utility-interface
  (export :stob
          make-stob
          stob-ref
          stob-set!
          stob?
          stob-length
          allocate->stob
          allocate/verify
          (define-stob-accessors :syntax)
          stob->list))

(define-structure stob-utility stob-utility-interface
  (open extra-scheme        
        (with-prefix persistent-immutable z)
        zvector-utils)
  (files (comp stob-util)))

(define-interface description-procedural-interface
  (export :description description?
          make-description
          list->description
          unfold-list->description-source

          specification?
          make-specification
          list->specification
          unfold-list->specification-source

          attribute?
          make-attribute

          description-specifications
          description-length
          description-specification-index
          specification-names
          specification-of
          specification-name
          specification-attributes
          specification-length
          get-specification-attribute
          attribute-name
          attribute-value

          descriptions-equal?
          map-specification-attributes          
          project-description
          project-specifications
          project-specification
          descriptions-consistent?
          combine-descriptions-by-name
          ))

(define-structure description-procedural description-procedural-interface
  (open extra-scheme
        syntax-util
        assert
        alist
        list
        proc-def
        methods
        records
        exceptions)
  (files (comp description)))

(define-interface description-interface
  (compound-interface
   description-procedural-interface
   (export (syntax/normalize-description :syntax)
           (syntax/normalize-specification :syntax)
           (syntax/update-attribute-values :syntax)
           (syntax/quote-non-literal :syntax)
           (syntax/make-description :syntax))))

(define-structure description description-interface
  (for-syntax (open extra-scheme
                    list
                    description-procedural
                    names
                    alist
                    syntax-util
                    uuidgen))
  (open extra-scheme
        assert
        description-procedural
        alist)
  (files (comp description-syntax)))

(define-interface type-inspection-interface
  (export strict-subtype?
          identical-type?
          direct-descendant?
          ancestors
          ancestors-and-self
          more-specific-type?
          type-name
          scheme-form-conforms-to-type?
          self-evaluating?))

(define-interface type-implementation-utility-interface
  (compound-interface
   type-inspection-interface
   (export compute-priority
           &type-name)))

(define-interface type-description-interface
  (export specification-type
          (define-specification-attribute-accessors :syntax)
          (type-description :syntax)
          invalid
          values-consistent/description?
          value-consistent/specification?))

(define-structures ((type-description type-description-interface)
                    (type-inspection type-inspection-interface)
                    (type-implementation-utility type-implementation-utility-interface))
  (open extra-scheme
        assert
        alist
        list
        proc-def
        methods meta-methods
        exceptions
        primitives ; for UNSPECIFIC
        description
        stob-utility
        syntax-util
        record-types)  
  (files (comp type-util)
         (comp type-description)))

(define-interface ykk/record-procedural-interface
  (export make-protocol-driver
          basic-driver
          extending-driver
          length-validator
          verifier

          generative-allocator
          generative-make
          generative-composer
          basic-generative-constructor
          default-generative-protocol

          nongenerative-allocator
          nongenerative-make
          nongenerative-composer
          basic-nongenerative-constructor
          default-nongenerative-protocol

          type-slot-index &type-slot-index

          stob-type
          make-record-predicate
          make-record-accessor

          (description :syntax)

          :maybe-rtd
          :rtd-type
          rtd-type?
          rtd-consistent?
          rtd-type-protocol
          rtd-type-driver
          make-rtd-type
          
          rtd-id
          rtd-name
          rtd-parent
          rtd-nongenerative
          rtd-sealed?
          rtd-opaque?
          rtd-slots         
          rtd-slot-count
          rtd-environment
          rtd-predicate
          rtd-priority

          define-name->name
          rtd->predicate-name
          rtd->protocol-name
          rtd->driver-name
          rtd->constructor-name
          rtd-accessor-name))

(define-structure ykk/record-procedural ykk/record-procedural-interface
  (open extra-scheme
        list
        proc-def
        description
        primitive-types
        type-description type-implementation-utility
        stob-utility
        records
        assert
        syntax-util
        methods meta-methods
        identifier
        pp
        simple-signals
        (subset packages (:package)))  
  (files (comp record-procedural)))

(define-interface ykk/record-syntax-interface
  (export (unrecord :syntax)
          (record-update :syntax)
          (sharing-record-update :syntax)
          (define-record-type/primitive :syntax)))

(define-structure ykk/record-syntax ykk/record-syntax-interface
  (for-syntax (open extra-scheme
                    names
                    ykk/record-procedural
                    list
                    environments
                    alist
                    description
                    syntax-util))  
  (open extra-scheme
        environments
        ykk/record-procedural
        stob-utility
        assert
        syntax-util
        description
        (subset sharing (share)))
  (files (comp record-syntax)))

(define-structure ykk/records
  ykk/record-syntax-interface
  (open ykk/record-syntax))
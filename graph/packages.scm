(define (r5 structure)
  (with-prefix structure r5:))

;;;; zipper
(define-structure shift-reset
  (export
   (shift :syntax)
   (reset :syntax))
  (open scheme signals escapes fluids records threads)
  (files new-shift))

(define-interface persistent-immutable-interface
  (export
   allocate
   persistent-symbol
   persistent-symbol-set!))

(define-interface vector-interface
  (export
   :vector
   make-vector
   vector
   primitive-vector
   vector?
   vector-length
   vector-ref
   vector-set!))

(define-structures
  ((persistent-immutable (compound-interface
                          vector-interface
                          persistent-immutable-interface))
   (persistent-internal (export log-port log-set! replay-log-port)))
  (open (modify
         extra-scheme
         (hide make-vector vector vector? vector-length vector-ref))
        (r5 scheme)
        tables
        locks
        srfi-9+
        ykk-ports
        ykk-parsing
        monad-style-output
        list
        uuidgen)
  (files persistent-immutable))

(define-structure heap-rotate
  (export rotate-log-and-store-heap
          initialize-logging)
  (open extra-scheme
        write-images
        usual-resumer
        posix-files
        posix-time
        locks
        os-strings
        assert
        ykk-ports
        list
        persistent-internal)
  (files heap-rotate))

(define-structure persistent-immutable-equal
  (export equal?)
  (open persistent-immutable
        scheme
        (r5 scheme)
        scheme-extensions
        alist)
  (files equal))

(define-interface tiny-srfi-43-interface
  (export
   vector-fold-index
   vector-fold
   vector-fold-right-index
   vector-fold-right))

(define-structure zvector-utils tiny-srfi-43-interface
  (open persistent-immutable
        scheme
        list)
  (files zlist))

;;;; The comp Collection
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

  (files interface))

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
  (files package))

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
  (files module-language))

;;;; Graph / Compiler
(define-interface identifier-interface
  (export identifier
          identifier?
          new-identifier))

(define-structure identifier identifier-interface
  (open extra-scheme
        uuidgen)
  (files identifier))

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
        assert
        zvector-utils)
  (files stob-util))

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
        methods
        records
        exceptions)
  (files description))

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
  (files description-syntax))

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
        methods meta-methods
        exceptions
        primitives ; for UNSPECIFIC
        description
        stob-utility
        syntax-util
        record-types)
  (files type-util
         type-description))

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
  (files record-procedural))

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
        list)
  (files record-syntax))

(define-structure ykk/records
  ykk/record-syntax-interface
  (open ykk/record-syntax))

;;;; tree diffing & merging
(define-structure tree-merging
  (export lcs-fold)
  (open scheme
        srfi-1
        srfi-9+
        srfi-13
        srfi-26
        simple-signals
        simple-conditions
        shift-reset
        table
        random
        time)
  (files diffing))

;;;; Graph
(define-interface graph-interface
  (export

   ;; constructors
   (root :syntax)
   (edge :syntax)
   (node :syntax)
   (children :syntax)
   child-list
   add-child

   ;; types
   :graph
   :node
   :edge
   :children
   :child

   ;; predicates
   graph?
   root?
   leaf?
   child?
   children?
   null-children?

   ;; accessors
   graph-name
   graph-edge
   graph-node
   graph-children
   graph-type
   child-name
   child->graph
   next-child

   ;; children
   end-of-children?
   share-children
   share-child
   fold-children

   ;; mutators
   add-child
   replace-node
   replace-node-children
   replace-children
   rename

   ;; conditions
   graph-error
   graph-error?))

(define-interface graph-traversal-interface
  (export walk
          map->list
          traverse

          :graph-zipper
          graph-zipper?
          z-dir
          z-item
          z-k
          zip-graph
          move
          zip-all-the-way-up))

(define-interface graph-path-interface
  (export path->list
          make-absolute

          absolute?
          relative?
          path-error
          path-error?

          resolve
          resolve-in
          find-child

          z-graph?
          z-child?
          z-end?
          up down prev next parent first-child))

(define-interface graph-access-interface
  (export has-access?))

(define-module (make-traversal-structure graph)
  (structure graph-traversal-interface
             (open extra-scheme
                   assert
                   srfi-9+
                   shift-reset
                   graph
                   (modify list
                           (rename (shared:share share)
                                   (shared:shared-cons shared-cons))
                           (prefix shared:))
                   ykk/records ; for testing
                   )
             (files graph-traversal)))

(define-module (make-path-structure graph traverse)
  (structure graph-path-interface
    (open extra-scheme
          assert
          list srfi-9+ srfi-13 srfi-14
          checking
          shift-reset
          handle
          graph
          traverse
          ykk/records ; for testing
          exceptions
          )
    (files graph-path)))

(define-syntax define-graph-structures
  (syntax-rules (primitive implement)
    ((_ big (implement interfaces ...) (primitive prim) traverse path)
     (begin
       (def traverse (make-traversal-structure prim))
       (def path (make-path-structure prim traverse))
       (define-structure big (compound-interface interfaces ...
                                                 graph-interface
                                                 graph-traversal-interface
                                                 graph-path-interface)
         (open prim traverse path))))))

;; --------------------
;; Persisted

(define-structure primitive-persisted-graph graph-interface
  (open extra-scheme
        assert
        list
        exceptions
        data-definition
        ykk/records
        methods
        primitive-types
        syntax-util)
  (files persisted-graph))

(define-graph-structures persisted-graph
  (implement)
  (primitive primitive-persisted-graph)
  persisted-traversal
  persisted-path)

;; --------------------
;; Scanned

(define-interface source-scan-interface
  (export shallow-scan))

(define-structure source-scan source-scan-interface
  (open extra-scheme
        assert
        list
        exceptions)
  (files source-scan))

(define-interface scanned-graph-interface
  (compound-interface
   graph-interface
   (export scan
           scanned->source
           graph-forms
           graph-structures
           has-access?)))

(define-structure primitive-scanned-graph scanned-graph-interface
  (open extra-scheme
        assert
        list srfi-9+
        exceptions
        (with-prefix primitive-persisted-graph source:)
        methods
        ykk/records
        source-scan
        environments)
  (files scanned-graph))

(define-graph-structures scanned-graph
  (implement scanned-graph-interface)
  (primitive primitive-scanned-graph)
  scanned-traversal
  scanned-path)

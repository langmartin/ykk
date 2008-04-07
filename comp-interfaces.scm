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


(define-interface primitive-types-interface
  (export :maybe-symbol maybe-symbol?
          :maybe-sexpr maybe-sexpr?
          :sexpr sexpr?
          :code-block code-block?))

(define-interface identifier-interface
  (export identifier
          identifier?
          new-identifier))

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

(define-interface syntax-util-procedural-interface
  (export self-evaluating?
          quotation?
          literal?
          macro-use?
          procedure-call?
          keyword?

          gensym
          quote-non-literal
          remove-keyword-indication
          keywords->alist

          continue
          continue/values
          continue-into
          continue-into/values

          define-now!
          force-up!
          up-one-tower-level
          for-syntax-environment
          definition-value

          expand
          map-expand
          apply-macro-transformer
          transformer-procedure
          ))

(define-interface srfi-89-procedural-interface
  (export srfi-89:require-positionals
          srfi-89:optional-positionals
          srfi-89:named-parameters
          srfi-89:parse-formals
          srfi-89:stack->k
          beta-substitute))

(define-interface srfi-89-syntax-interface
  (export define-syntax*
          srfi-89/required-parameters
          srfi-89/optional-parameters
          srfi-89/named-parameters
          srfi-89/rest
          srfi-89/no-rest))

(define-interface syntax-util-interface
  (compound-interface
   syntax-util-procedural-interface
   (export (syntax-k :syntax)
           (syntax-k/values :syntax)
           (syntax-k-into :syntax)
           (syntax-k-into/values :syntax)
           (define/expansion :syntax)
           (define/force-up :syntax)
           (syntax/eval :syntax)
           (define-syntax/applicative-order :syntax)
           (define-syntax* :syntax)
           (syntax/quote-non-literal :syntax))))

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

(define-interface description-interface
  (compound-interface
   description-procedural-interface
   (export (syntax/normalize-description :syntax)
           (syntax/normalize-specification :syntax)
           (syntax/update-attribute-values :syntax)
           (syntax/quote-non-literal :syntax)
           (syntax/make-description :syntax))))

(define-interface type-inspection-interface
  (export strict-subtype?
          identical-type?
          direct-descendant?
          ancestors
          ancestors-and-self
          more-specific-type?))

(define-interface type-implementation-utility-interface
  (compound-interface
   type-inspection-interface
   (export compute-priority)))

(define-interface type-description-interface
  (export specification-type
          (define-specification-attribute-accessors :syntax)
          (type-description :syntax)
          invalid
          values-consistent/description?
          value-consistent/specification?))

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

(define-interface ykk/record-syntax-interface
  (export (unrecord :syntax)
          (record-update :syntax)
          (sharing-record-update :syntax)
          (define-record-type/primitive :syntax)))

;;;; Graph

;; --------------------
;; General-pupose

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

;; --------------------
;; Implementation-specific

(define-interface scanned-graph-interface
  (compound-interface
   graph-interface
   (export scan
           scanned->source
           graph-forms
           graph-structures
           has-access?)))

;; --------------------
;; Auxilliary

(define-interface source-scan-interface
  (export shallow-scan))

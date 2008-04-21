;;;; zipper
(define-structure shift-reset
  (export
   (shift :syntax)
   (reset :syntax))
  (open scheme signals escapes fluids records threads)
  (files (zipper new-shift)))

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
         scheme+
         (hide make-vector vector vector? vector-length vector-ref))
        (r5 scheme)
        tables
        locks
        uuidgen)
  (files (zipper persistent-immutable)))

(define-structure heap-rotate
  (export rotate-log-and-store-heap
          initialize-logging)
  (open scheme+
        write-images
        usual-resumer
        posix-files
        posix-time
        locks
        os-strings
        persistent-internal)
  (files (zipper heap-rotate)))

(define-structure persistent-immutable-equal
  (export equal?)
  (open persistent-immutable
        scheme
        (r5 scheme)
        language-ext
        alist)
  (files (zipper equal)))

(define-interface tiny-srfi-43-interface
  (export
   vector-fold-index
   vector-fold
   vector-fold-right-index
   vector-fold-right))

(define-structure zvector-utils tiny-srfi-43-interface  
  (open persistent-immutable
        scheme)  
  (files (graph zlist)))

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
  (files (zipper diffing)))

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
                   list srfi-9+
                   shift-reset
                   graph
                   proc-def
                   (modify sharing
                           (rename (shared:share share)
                                   (shared:shared-cons shared-cons))
                           (prefix shared:))
                   ykk/records ; for testing
                   )
             (files graph-traversal)))

(define-module (make-path-structure graph traverse)
  (structure graph-path-interface
    (open extra-scheme
          list srfi-9+ srfi-13 srfi-14
          proc-def
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
        list
        exceptions
        data-definition
        proc-def
        ykk/records
        methods
        primitive-types
        syntax-util
        (subset sharing (share)))
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
        list srfi-9+
        proc-def
        exceptions
        (with-prefix primitive-persisted-graph source:)
        methods
        ykk/records
        source-scan
        sharing
        environments)
  (files scanned-graph))

(define-graph-structures scanned-graph
  (implement scanned-graph-interface)
  (primitive primitive-scanned-graph)
  scanned-traversal
  scanned-path)

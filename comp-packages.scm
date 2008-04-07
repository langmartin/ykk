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


(define-structures ((comp/interfaces comp/interfaces-interface)
                    (comp/interface-creator comp/interface-creator-interface))  
  (open scheme
        srfi-1 srfi-8 srfi-9+
        types
        ykk/types type-reflection
        assert
        fluids+
        conditions+        
        comp/s48-dependencies)
  
  (files (comp interface)))

(define-structures ((comp/structure comp/structure-interface)
                    (comp/structure-reflection comp/structure-reflection-interface))
  (open scheme
        srfi-8 srfi-9+
        assert
        ykk/names ; FIXME: make this comp/names
        comp/interfaces comp/interface-creator
        comp/s48-dependencies)
  (files (comp package)))

(define-structures ((comp/module-language comp/module-language-interface)
                    (comp/module-language-create comp/module-language-creator-interface))
  (open scheme
        srfi-1 srfi-8
        assert
        fluids+
        conditions+
        big-util ; concatenate-symbol
        ykk/names ; for testing
        comp/interfaces comp/interface-creator
        comp/structure comp/structure-reflection
        comp/s48-dependencies)
  (files (comp module-language)))

(define-structure primitive-types primitive-types-interface
  (open extra-scheme
        methods)
  (files (utility primitive-types)))

(define-structure identifier identifier-interface
  (open extra-scheme
        uuidgen)
  (files (comp identifier)))

(define-structure stob-utility stob-utility-interface
  (open extra-scheme        
        (with-prefix persistent-immutable z)
        zvector-utils)
  (files (comp stob-util)))

(define-structures ((syntax-procedural syntax-util-procedural-interface)
                    (srfi-89-procedural srfi-89-procedural-interface))
  (open extra-scheme
        environments
        packages
        (subset compiler-envs (environment-macro-eval))
        (subset nodes (schemify))
        (subset names (desyntaxify))
        types
        bindings
        locations
        syntactic
        proc-def
        srfi-1+
        simple-signals
        assert
        alists)  
  (files (comp syntax-util-procedures)))

(define-structures ((syntax-util syntax-util-interface)
                    (srfi-89-syntax srfi-89-syntax-interface))
  (for-syntax (open extra-scheme
                    syntax-procedural
                    srfi-89-procedural
                    names
                    alists
                    srfi-1+
                    uuidgen))  
  (open extra-scheme
        syntax-procedural
        assert)
  (files (comp syntax-util)))

(define-structure description-procedural description-procedural-interface
  (open extra-scheme
        syntax-util
        assert
        alists
        srfi-1+
        proc-def
        methods
        records
        simple-signals
        conditions+)
  (files (comp description)))

(define-structure description description-interface
  (for-syntax (open extra-scheme
                    srfi-1+
                    description-procedural
                    names
                    alists
                    syntax-util
                    uuidgen))
  (open extra-scheme
        assert
        description-procedural
        alists)
  (files (comp description-syntax)))

(define-structures ((type-description type-description-interface)
                    (type-inspection type-inspection-interface))
  (open extra-scheme
        assert
        alists
        srfi-1+
        proc-def
        methods meta-methods
        simple-signals simple-conditions handle
        primitives ; for UNSPECIFIC
        description
        stob-utility)  
  (files (comp type-util)
         (comp type-description)))

(define-structure ykk/record-procedural ykk/record-procedural-interface
  (open extra-scheme
        srfi-1+
        proc-def
        description
        primitive-types
        type-description type-inspection
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

(define-structure ykk/record-syntax ykk/record-syntax-interface
  (for-syntax (open extra-scheme
                    names
                    ykk/record-procedural
                    srfi-1+
                    environments
                    alists
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

;;;; Graph

;; --------------------
;; Abstract

(define-module (make-traversal-structure graph)
  (structure graph-traversal-interface
             (open extra-scheme
                   srfi-1+ srfi-9+
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
          srfi-1+ srfi-9+ srfi-13 srfi-14
          proc-def
          checking
          shift-reset
          conditions+
          graph
          traverse
          ykk/records ; for testing
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
        srfi-1+
        conditions+
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

(define-structure source-scan source-scan-interface
  (open extra-scheme
        srfi-1+
        conditions+)
  (files source-scan))

(define-structure primitive-scanned-graph scanned-graph-interface
  (open extra-scheme
        srfi-1+ srfi-9+
        proc-def
        conditions+
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

;;;; Plist Demo

(define-structure plist-demo
  (export)
  (open extra-scheme
        srfi-1+
        ykk/records
        (with-prefix persisted-graph source:)
        scanned-graph        
        assert
        methods
        primitive-types
        persistent-immutable
        proc-def))

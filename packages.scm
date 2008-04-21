(define-syntax define-meta-structure
  (syntax-rules ()
    ((_ struct (package ...) body ...)
     (define-structure struct
       (compound-interface
        (interface-of package)
        ...)
       (open package ...)
       body ...))))

(define (s48 structure)
  (with-prefix structure s48:))

(define (r5 structure)
  (with-prefix structure r5:))

(define (ykk structure)
  (with-prefix structure ykk:))

;;;; core
(define-interface list-interface
  (compound-interface
   (interface-of srfi-1)
   (export
    intersperse
    fold-append
    fold-right-append
    map/cons*
    map*
    share
    fmap-car
    fmap-cdr
    fmap-pair
    fmap-cadr
    fmap-list
    fold-numbers
    fold-right-numbers)))

(define-structure list
  list-interface
  (open extra-scheme
        srfi-1
        assert)
  (files utility/list))

(define-interface alist-interface
  (export
   cons-alist
   fold-two
   list->alist
   unfold-list->alist
   update-alist
   update-force-alist
   merge-alists/template
   merge-alists
   (let-foldr* :syntax)
   alist-tree-insert
   alist-key-index
   map-car
   project-alist-onto
   partition-alist
   ((pluck-spec pluck-alist) :syntax)
   ((bind-spec bind-alist) :syntax)
   keyword-projector/defaults
   keyword-partitioner/defaults
   alist-has-keys?
   alist-has-only-keys?
   alist-has-exactly-keys?
   alist-ref
   (unalist :syntax)   
   unalist-proc
   pair->list
   list->pair
   choose-keys
   remove-keys
   predicate-eq))

(define-structure alist
  alist-interface
  (open extra-scheme
        list
        big-util
        exceptions
        assert
        optional-arguments)
  (files utility/alists))

(define-interface string-interface
  (compound-interface
   (interface-of srfi-13)
   (interface-of srfi-14)
   (export
    string->normal-symbol
    string->label
    string->name
    string->identifier
    tech-name
    normalize-string)))

(define-structure string string-interface
  (open extra-scheme
        list
        srfi-13
        srfi-14
        assert)
  (files (utility string)))

;; SRFI-9 + define-record-discloser
(define-structure srfi-9+
  (export (define-record-type :syntax)
          define-record-discloser)
  (open scheme-level-2
	(s48 define-record-types))
  (begin
    (define define-record-discloser s48:define-record-discloser)
    (define-syntax define-record-type
      (syntax-rules ()
        ((define-record-type type-name . stuff)
         (s48:define-record-type type-name type-name . stuff))))))

(define-meta-structure scheme+
  (extra-scheme
   assert
   ykk-ports
   ykk-parsing
   monad-style-output
   optional-arguments
   list
   srfi-2
   srfi-9+
   string
   alist
   exceptions))

;;;; YKK
(define-structure zassert
  (compound-interface assert-interface (export equal?))
  (open persistent-immutable-equal
        scheme
        signals
        ykk-ports)
  (files utility/assert))

;;;; Syntax
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
          error/syntax
          ))

(define-interface srfi-89-procedural-interface
  (export srfi-89:require-positionals
          srfi-89:optional-positionals
          srfi-89:named-parameters
          srfi-89:parse-formals
          srfi-89:stack->k
          beta-substitute))

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
        list
        simple-signals
        assert
        alist)  
  (files (utility syntax-util-procedures)))

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
           (expand/strip :syntax)
           (define-syntax* :syntax)
           (syntax/quote-non-literal :syntax))))

(define-structures ((syntax-util syntax-util-interface)
                    (srfi-89-syntax srfi-89-syntax-interface))
  (for-syntax (open extra-scheme
                    syntax-procedural
                    srfi-89-procedural
                    names
                    alist
                    list
                    uuidgen))  
  (open extra-scheme
        syntax-procedural
        assert)
  (files (utility syntax-util)))

;;;; HTTP / HTML / Web
(define-interface form-server-interface
  (export form-server
          (page :syntax)
          bread-crumb
          header
          footer
          load-file))

(define-structure form-server form-server-interface
  (open scheme+
        posix-processes
        pages
        htmlprag
        http
        exceptions
        display-conditions
        forms)
  (files http/form-server))

(define-interface forms-interface
  (export (form :syntax)
          form->shtml

          ;; input -> shtml
          text
          textarea
          radio
          select
          checkbox
          submit

          ;; sxml -> shtml
          sxml-text
          sxml-textarea
          sxml-radio
          sxml-select
          sxml-checkbox
          sxml-submit))

(define-structure forms forms-interface
  (open scheme+
        sxml
        sxml-tree-trans
        checking
        methods)
  (files http/forms))

(define-interface sxml-interface
  (export sxpath-run
          sxml-attlist
          (let-sxml-attlist :syntax)
          (let-sxml-pluck-attlist :syntax)
          sxml-first-text
          sxpath-error?))

(define-structure sxml sxml-interface
  (open scheme+
        sxml-tree-trans
        sxpath)  
  (files utility/sxml))

(define-interface pages-interface
  (export (page-response :syntax)))

(define-structure pages pages-interface
  (open scheme+
        http
        htmlprag)
  (files http/page))

;;;; Plist
(define-structure plist-demo
  (export)
  (for-syntax (open scheme+
                    (subset names (desyntaxify))
                    http-protocol))
  (open extra-scheme
        list
        (modify srfi-1 (prefix srfi-1:) (expose member))
        ykk/records
        (with-prefix persisted-graph source:)
        scanned-graph        
        assert
        methods
        primitive-types
        persistent-immutable
        proc-def
        http http-protocol
        ykk/record-procedural
        description
        forms form-server
        string+
        alist
        threads threads-internal
        pages
        exceptions
        monad-style-output ykk-ports
        syntax-util
        htmlprag
        type-inspection
        environments)  
  (files plist-demo))
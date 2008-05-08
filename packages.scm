(define (s48 structure)
  (with-prefix structure s48:))

(define (ykk structure)
  (with-prefix structure ykk:))

;;;; YKK
(define-structure zassert
  (compound-interface assert-interface (export equal?))
  (open persistent-immutable-equal
        scheme
        signals
        ykk-ports)
  (files utility/assert))

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
  (export form->shtml

          ;; input -> shtml
          text
          textarea
          radio
          select
          checkbox
          submit

          form-data
          with-form-data
          (let-form-data :syntax)))

(define-structure forms forms-interface
  (open scheme+
        sxml-tree-trans
        sxml-tools
        methods
        (with-prefix regexps rx:)
        exceptions
        environments
        exceptions
        fluids+)
  (files http/forms))

(define-interface sxml-interface
  (export txpath-run
          sxpath-run
          (let-sxml-attrs :syntax)
          (let-sxml-pluck-attrs :syntax)
          (pluck-attributes :syntax)
          (bind-attributes :syntax)
          update-attributes
          add-class
          join-classes
          merge-attribute-sets))

(define-structure sxml-tools
  (compound-interface
   (interface-of sxml-basic-tools)
   sxml-interface)
  (open scheme+
        list
        alist
        string
        sxml-basic-tools)
  (files utility/sxml))

;;;; Plist
(define-structure plist-demo
  (export)
  (open extra-scheme
        list
        ykk/records
        (with-prefix persisted-graph source:)
        scanned-graph        
        assert
        methods
        primitive-types
        persistent-immutable
        http http-protocol
        ykk/record-procedural
        description
        forms form-server
        string
        alist
        threads threads-internal
        pages
        exceptions
        monad-style-output ykk-ports
        syntax-util
        htmlprag sxml-tools
        type-inspection
        environments)  
  (files plist-demo))

(define-structure rss-reader-milestone
  (export)
  (open scheme+
        pp
        ssax-vanilla
        htmlprag
        threads
        locks
        ykk/records
        (with-prefix persisted-graph source:)
        scanned-graph
        assert
        methods
        primitive-types
        persistent-immutable
        heap-rotate
        http
        ducts
        ykk/record-procedural
        description
        forms
        alist))


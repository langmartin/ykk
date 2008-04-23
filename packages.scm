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
          (let-sxml-pluck-attrs :syntax)))

(define-structure sxml-tools
  (compound-interface
   (interface-of sxml-basic-tools)
   sxml-interface)
  (open scheme+
        alist
        sxml-basic-tools)
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
        htmlprag
        type-inspection
        environments)  
  (files plist-demo))

(define-structure rss-reader-milestone
  (export)
  (open scheme+
        threads
        ykk/records
        (with-prefix persisted-graph source:)
        scanned-graph        
        assert
        methods
        primitive-types
        persistent-immutable
        http
        ykk/record-procedural
        description
        forms
        alist)
  (files rss-reader-milestone))

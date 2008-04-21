(define (s48 structure)
  (with-prefix structure s48:))

(define (r5 structure)
  (with-prefix structure r5:))

(define (ykk structure)
  (with-prefix structure ykk:))

;;;; add #; and #,(foo ...) to the reader
(define-structure octothorpe-extensions
  (export define-reader-ctor)
  (open scheme primitives reading extended-ports)
  (files utility/octothorpe-extensions))

;;;; core utilites
(define-structure optional-arguments
  optional-arguments-interface
  (open scheme)
  (files utility/optional-arguments))

(define-structure ykk-ports
  ykk-ports-interface
  (open scheme
        i/o i/o-internal extended-ports
        optional-arguments)
  (files utility/ykk-ports))

(define-structure assert
  assert-interface
  (open scheme
        signals
        ykk-ports)
  (files utility/assert))

(define-structure checking checking-interface
  (for-syntax (open scheme srfi-1 names))
  (open scheme
        assert
        simple-signals
        meta-methods)
  (files (utility check)))

(define-structure extra-scheme extra-scheme-interface
  (open (modify scheme (hide cond let let* letrec define))
        srfi-61                         ; steroidal cond
        srfi-71                         ; steroidal let
        simple-signals
        assert
        big-util
        (modify checking (rename (define-checked define))))
  (files (utility extra-scheme)))

(define-structure more-regexps more-regexps-interface
  (open scheme
        regexps
        posix-regexps)
  (files (utility regexps)))

;;;; fundamental utils
(define-structure language-ext
  language-ext-interface
  (open scheme
        assert)
  (files utility/language-ext))

(define-structure srfi-1+
  srfi-1+-interface
  (open scheme
        srfi-1
        assert)
  (files utility/list))

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

(define-structure string+ string+-interface
  (open scheme+
        srfi-1+
        srfi-13
        srfi-14
        assert)
  (files (utility string)))

(define-structure ykk-parsing
  ykk-parsing-interface
  (open scheme
        signals
        assert
        extended-ports
        optional-arguments
        ykk-ports
        srfi-13)
  (files (utility ykk-parsing)))

(define-structure monad-style-output
  monad-style-output-interface
  (open scheme
        assert
        optional-arguments
        ykk-ports)
  (files utility/monad-style-output))

;;;; Meta-structure for convenience
(define-syntax define-meta-structure
  (syntax-rules ()
    ((_ struct (package ...) body ...)
     (define-structure struct
       (compound-interface
        (interface-of package)
        ...)
       (open package ...)
       body ...))))

(define-meta-structure scheme+
  (extra-scheme
   simple-signals
   assert
   ykk-ports
   ykk-parsing
   monad-style-output
   language-ext
   optional-arguments
   srfi-1+
   srfi-2
   srfi-9+
   srfi-13
   alists))

;;;; further core, not part of scheme+, though
(define-structure fluids+ fluids+-interface
  (for-syntax (open scheme fluids))
  (open scheme fluids)
  (files utility/fluids))

(define-structure alists
  alists-interface
  (open extra-scheme
        srfi-1+
        big-util
        exceptions
        assert
        proc-def
        optional-arguments)
  (files utility/alists))

(define-structure exceptions
  exceptions-interface
  (open scheme
        handle
        simple-signals
        simple-conditions
        fluids+)  
  (files (utility exceptions)))

(define-structure proc-def procedure-definition-interface
  (open scheme
        srfi-26)
  (files (utility procedure-def)))

(define-structure sharing sharing-interface
  (open extra-scheme
        srfi-1+
        proc-def)
  (files (utility share)))

(define-structure data-definition data-definition-interface
  (open extra-scheme
        methods)
  (files (utility data-def)))

(define-structure dates dates-interface
  (open scheme+
        load-dynamic-externals
        external-calls
        posix-time
        regexps
        more-regexps
        exceptions
        srfi-26)
  (files utility/dates))

;;;; Red/Black Trees
(define-structures ((red/black red/black-interface)
                    (red/black-inspection red/black-inspection-interface))
   (open scheme srfi-8 srfi-9 record-types primitives simple-signals pp)
   (files (utility red-black-constructed-from-records)
          (utility red-black)
          (utility vred-black)))

(define-structures ((list-red/black red/black-interface)
                    (list-red/black-inspection red/black-inspection-interface))
   (open scheme srfi-8 srfi-9 simple-signals pp)
   (files (utility red-black-constructed-from-lists)
          (utility red-black)
          (utility vred-black)))

;;;; Sets
(define-structure rb-set set-interface
  (open scheme red/black srfi-1 assert)
  (files (utility set-red-black)
         (utility vset)))

(define-structure list-set set-interface
  (open scheme srfi-1 srfi-9+ assert)
  (files (utility set-list)
         (utility vset)))

(define set rb-set)

;;;; Primitive
(define-structures ((ykk/bindings ykk/bindings-interface)
                    (ykk/bindings-internal ykk/bindings-internal-interface))
  (open scheme
        meta-types
        locations
        methods              ; FIXME: for :value, :symbol, etc.  Change to ykk/methods later
        srfi-9+
        assert
        (s48 bindings)
        (s48 packages))
  (files (prim binding)))

(define-structures ((ykk/names ykk/names-interface)
                   (ykk/names-inspection ykk/names-inspection-interface))
  (open scheme
        set
        assert)
  (files (prim names)))

(define-structures ((ykk/environments ykk/environments-interface)
                    (ykk/environments-internal ykk/environments-internal-interface)
                    (environment-manipulation environment-manipulation-interface))
  (open scheme
        uuidgen
        srfi-1
        srfi-9+
        fluids+
        conditions
        primitives
        big-util                ; for IDENTITY (is this worth the open?)
        methods                 ; FIXME: for :symbol, change to ykk/methods later
        (subset packages-internal (for-each-definition))
        ykk/names-inspection    ; LIST-NAMES for record discloser (FIXME: remove when done with initial development)
        ykk/names
        ykk/bindings
        assert)
  (files (prim environments)))

(define-structure ykk/evaluation ykk/evaluation-interface
  (open scheme
        compiler-envs
        package-commands-internal
        srfi-8
        srfi-9+
        primitives
        methods              ; for :symbol, FIXME: remove later
        (s48 bindings)
        (s48 evaluation)
        (s48 environments)
        (s48 packages)
        (s48 packages-internal)
        (s48 names)
        ykk/bindings
        ykk/bindings-internal
        ykk/environments
        ykk/environments-internal
        assert)
  (files (prim evaluation)))

;;;; generics
(define-structure ykk/methods ykk/methods-interface
  (open scheme
	define-record-types
	records record-types records-internal
	bitwise util primitives
	simple-signals
        assert
        ykk-ports)
  (files (prim ykk-methods))
  (optimize auto-integrate))

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

;;;; forms
(define-structure form-server form-server-interface
  (open scheme+
        posix-processes
        pages
        htmlprag
        http
        exceptions
        display-conditions
        forms)
  (files forms/form-server))

(define-structure forms forms-interface
  (open scheme+
        sxml
        sxml-tree-trans
        checking
        methods)
  (files forms/forms))

;;;; sxml
(define-structure sxml sxml-interface
  (open scheme+
        sxml-tree-trans
        sxpath
        exceptions)
  (files utility/sxml))

;;;; pages
(define-structure pages pages-interface
  (open scheme+
        http
        htmlprag)
  (files http/page))

;;;; ducts
(define-structure duct-internal duct-interface
  (open scheme+
        ascii
        text-codecs
        byte-vectors)
  (files http/duct-internal))

(define-structure ducts ducts-interface
  (open scheme+
        byte-vectors
        bitwise
        ascii
        unicode
        text-codecs
        url
        duct-internal)
  (files http/ducts))

;;;; mime & url, http
(define-structure mime mime-interface
  (open scheme+
        reading
        unicode-char-maps
        text-codecs
        byte-vectors
        posix
        srfi-40
        ducts
        url)
  (files http/mime))

(define-structure url url-interface
  (open scheme+
        ascii
        unicode
        bitwise)
  (files http/url))

(define-structure curl
  (export
   curl*
   curl-ssl->string)
  (open scheme
        srfi-8
        signals
        posix
        i/o
        ykk-ports
        srfi-1
        assert
        url)
  (files (http curl)))

(define-structure uuidgen
  (export uuidgen-v1->hex-string)
  (open scheme srfi-27 bitwise
        assert
        ykk-ports
        srfi-1
        srfi-13)
  (files http/uuid))

(define-structure csv
  (export read-csv-record
          display-as-csv-field
          display-as-csv-row
          display-as-csv-table)
  (open scheme
        signals)
  (files (http csv)))

(define-structure json
  (export alist->json-obj
          list->json-arr
          json-fold-right               ; cons nil duct . ocons opair
          )
  (open scheme+
        ducts)
  (files (http json)))

(define-structure http
  (compound-interface http-interface (interface-of url))
  (open scheme+
        sockets
        byte-vectors
        tables
        threads
        srfi-40
        srfi-8
        exceptions
        posix-files
        mime
        url
        ducts
        json
        ssax-vanilla
        fluids+)
    (files (http http)
           (http http-standard-dispatch)))

(define-structure http-protocol http-protocol-interface
  (open scheme+
        tables
        assert)
  (files (http protocol)))

(define-structure standard-test
  (export)
  (open scheme+
        htmlprag
        http
        mime
        url
        json)
  (files (http standard-test)))

;;;; zipper
(define-structure shift-reset
  (export
   (shift :syntax)
   (reset :syntax))
  (open scheme signals escapes fluids records threads)
  (files (zipper new-shift)))

(define-structure zipper
  zipper-interface
  (open scheme srfi-9)
  (files (zipper zipper)))

(define-interface persistent-immutable-interface
  (export
   allocate
   persistent-symbol
   persistent-symbol-set!))

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
        alists)
  (files (zipper equal)))

(define-structure zassert
  (compound-interface assert-interface (export equal?))
  (open persistent-immutable-equal
        scheme
        signals
        ykk-ports)
  (files utility/assert))

(define-structure persistent-records
  (export
   (define-record-type :syntax)
   define-record-discloser
   vector?)
  (for-syntax (open scheme assert))
  (open persistent-immutable
        scheme
        signals
        tables
        methods)
  (files (zipper functional-records)))

(define-structure zscheme+
  (interface-of scheme+)
  (open (modify
         scheme+
         (hide equal? assert
               make-vector vector vector? vector-ref vector-set! vector-length
               define-record-type define-record-discloser))
        zassert
        persistent-records
        persistent-immutable))

(define-structures ((zlist (compound-interface
                             list-interface
                             tiny-srfi-1-interface))
                    (zvector-utils tiny-srfi-43-interface))  
  (open persistent-records
        zassert
        zscheme+
        (r5 srfi-1)
        language-ext
        optional-arguments)
  (files (zipper zlist)))

;;; Types
(define-structures ((ykk/types ykk/types-interface)
                    (type-reflection ykk/type-reflection-interface))  

  ;; for destructuring
  (for-syntax (open scheme srfi-1))
  (open extra-scheme
        (modify sharing (rename (shared:share share)) (prefix shared:))
        methods meta-methods
        srfi-1+ srfi-8 srfi-9+
        proc-def
        exceptions
        primitives
        (subset record-types (record-type?)))
  (files types))

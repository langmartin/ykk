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

(define-structure zassert
  (compound-interface
   assert-interface
   (export equal?))
  (open persistent-immutable-equal
        scheme
        signals
        ykk-ports)
  (files utility/assert))

(define-structure optional-arguments
  optional-arguments-interface
  (open scheme
        assert)
  (files utility/optional-arguments))

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

(define-structure ykk-parsing
  ykk-parsing-interface
  (open scheme
        signals
        assert
        extended-ports
        optional-arguments
        ykk-ports)
  (files (utility ykk-parsing)))

(define-structure monad-style-output
  monad-style-output-interface
  (open scheme
        assert
        optional-arguments
        ykk-ports)
  (files utility/monad-style-output))

(define-structure alists
  alists-interface
  (open scheme
        srfi-1
        assert)
  (files utility/alists))

(define-structure http-build-utilities
  (compound-interface
   the-interface-formerly-know-as-util
   the-interface-formerly-know-as-io-util
   signals-interface
   define-record-types-interface)
  (open scheme
        signals
        i/o i/o-internal extended-ports
        define-record-types
        big-util
        srfi-1+
        srfi-2
        srfi-13
        srfi-78
        assert
        optional-arguments
        language-ext
        alists
        ykk-ports
        ykk-parsing
        monad-style-output))

(define-structure exceptions
  exceptions-interface
  (open scheme
        handle
        simple-conditions)
  (files (utility exceptions)))

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

;;;; uuidgen


;;;; Fluids
(define-structure fluids+ fluids+-interface
  (for-syntax (open scheme fluids))
  (open scheme fluids)
  (files utility/fluids))

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

;;;; ducts
(define-structure duct-internal duct-interface
  (open scheme
        http-build-utilities
        ascii
        text-codecs
        byte-vectors)
  (files http/duct-internal))

(define-structure ducts ducts-interface
  (open scheme
        http-build-utilities
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
  (open scheme
        http-build-utilities
        reading
        unicode-char-maps
        text-codecs
        byte-vectors
        posix
        srfi-40
        ducts)
  (files http/mime))

(define-structure url url-interface
  (open scheme
        http-build-utilities
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
  (export uuidgen)
  (open scheme srfi-27 bitwise)
  (files http/uuid))

(define-structure http http-interface
  (open scheme
        http-build-utilities
        fluids
        sockets
        byte-vectors
        tables
        threads
        srfi-40
        srfi-8
        exceptions
        mime
        url
        ducts)
    (files http/http))

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

;;; the set of symbols that will work when passed to persistent-symbol-set!
(define-structure persistent-symbols
  (export
   *processes*)
  (open scheme)
  (begin
    (define *processes* #f)))

(define-structure persistent-immutable
  vector-interface
  (open scheme
        signals
        srfi-9+
        (r5 scheme)
        (r5 srfi-1)
        tables
        package-commands-internal
        environments
        fluids+
        uuidgen
        ykk-ports
        monad-style-output
        ykk-parsing
        persistent-symbols
        language-ext)
  (files (zipper persistent-immutable)))

(define-structure persistent-immutable-equal
  (export equal?)
  (open persistent-immutable
        scheme
        (r5 scheme)
        language-ext
        alists)
  (files (zipper equal)))

(define-structure persistent-logging
  persistent-immutable-logging-interface
  (open persistent-immutable))

(define-structure persistent-records
  (export
   (define-record-type :syntax)
   vector?)
  (for-syntax (open scheme assert))
  (open persistent-immutable
        scheme
        signals)
  (files (zipper functional-records)))

(define-structure def-record
  (export
   (def-record :syntax)
   (def-discloser :syntax))
  (open scheme
        persistent-records
        methods)
  (files (zipper def-record)))

(define-structure persistent-lists
  (compound-interface
   list-interface
   tiny-srfi-1-interface
   tiny-srfi-43-interface)
  (open scheme
        (r5 scheme)
        (r5 srfi-1)
        persistent-records
        def-record
        zassert)
  (files (zipper persistent-things)))

(define-structure process
  process-interface
  (open scheme
        srfi-9+
        simple-signals
        assert
        ykk-ports
        monad-style-output
        persistent-records)
  (files (zipper process)))

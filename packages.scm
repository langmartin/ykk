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

(define-structure oleg-style-parsing
  oleg-style-parsing-interface
  (open scheme
        signals
        assert
        extended-ports
        optional-arguments
        ykk-ports)
  (files utility/oleg-style-parsing))

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
        oleg-style-parsing
        monad-style-output))

;;;; Red/Black Trees
(define-structures ((red/black red/black-interface)
                    (red/black-inspection red/black-inspection-interface))
   (open scheme srfi-8 srfi-9 record-types primitives simple-signals pp)   
   (files (utility red-black)))

;;;; UUID gen
(define-structure uuid
  (export uuidgen)
  (open scheme srfi-27 bitwise)
  (files http/uuid))

;;;; logging cons
(define-structure logging-cons logging-cons-interface
  (open scheme define-record-types tables i/o
        srfi-1
        util uuid)
  (files zipper/logging-cons))

;;;; Fluids
(define-structure extended-fluids extended-fluids-interface
  (for-syntax (open scheme fluids))
  (open scheme fluids)
  (files fluids))

;;;; Primitive
(define (s48 structure)
  (modify structure (prefix s48-)))

(define-structures ((ykk/bindings ykk/bindings-interface)
                    (ykk/bindings-internal ykk/bindings-internal-interface))
  (open scheme define-record-types meta-types locations
        methods ; FIXME: for :value, :symbol, etc.  Change to ykk/methods later
        (s48 bindings) (s48 packages))
  (files (prim binding)))

(define-structures ((ykk/names ykk/names-interface)
                   (ykk/names-inspection ykk/names-inspection-interface))
  (open scheme red/black red/black-inspection)
  (files (prim names)))

(define-structures ((ykk/environments ykk/environments-interface)
                    (ykk/environments-internal ykk/environments-internal-interface))
  
  (open scheme uuid define-record-types srfi-1 extended-fluids conditions primitives
        big-util ; for IDENTITY (is this worth the open?)
        methods ; FIXME: for :symbol, change to ykk/methods later
        ykk/names-inspection ; LIST-NAMES for record discloser (FIXME: remove when done with initial development)
        ykk/names
        ykk/bindings)
  (files (prim environments)))

(define-structures ((ykk/evaluation ykk/evaluation-interface)
                    (ykk/evaluation-internal ykk/evaluation-internal-interface))
  
  (open scheme compiler-envs package-commands-internal define-record-types primitives
        methods ; for :symbol, FIXME: remove later
        (s48 bindings) (s48 evaluation) (s48 environments) (s48 packages) (s48 packages-internal) (s48 names)
        ykk/bindings ykk/bindings-internal ykk/environments ykk/environments-internal)
  (files (prim evaluation)))

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

;;;; mime & url
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

;;;; http
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
        mime
        url
        ducts)
    (files http/http))

;;;; add #; and #,(foo ...) to the reader
(define-structure octothorpe-extensions
  (export define-reader-ctor)
  (open scheme primitives reading extended-ports)
  (files utility/octothorpe-extensions))

;;;; core utilites
(define-structure util util-interface
  (open scheme signals
        srfi-1 srfi-2 srfi-78
        extended-ports
        i/o-internal)
  (files utility/util))

(define-structure io-util io-util-interface
  (open scheme signals
        i/o i/o-internal extended-ports
        util)
  (files utility/io-util))

(define-structure assert assert-interface
  (open scheme)
  (files utility/assert))

(define-structure optional-arguments optional-arguments-interface
  (open scheme
        assert)
  (files utility/optional-arguments))

(define-structure language-ext language-ext-interface
  (open scheme
        assert)
  (files utility/language-ext))

(define-structure srfi-1+ srfi-1+-interface
  (open scheme)
  (files utility/list))

(define-structure utility-grab-bag
  (compound-interface
   assert-interface
   srfi-1+-interface
   srfi-2-inteface                      ; and-let*
   srfi-13-interface
   big-util-interface
   language-ext-interface
   alist-interface
   optional-arguments-interface)
  (open scheme signals
        srfi-1 srfi-2 srfi-78
        i/o i/o-internal extended-ports
        assert optional-arguments language-ext
        
        ))

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

;;;; ducts
(define-structure duct-internal duct-interface
  (open scheme signals define-record-types
        ascii
        text-codecs
        byte-vectors
        ports
        util
        io-util)
  (files http/duct))

(define-structure ducts ducts-interface
  (open scheme signals
        byte-vectors bitwise ascii unicode
        text-codecs
        srfi-13
        util io-util
        url
        duct-internal)
  (files http/ducts))

;;;; mime & url
(define-structure mime mime-interface
  (open scheme signals define-record-types
        reading
        unicode-char-maps text-codecs byte-vectors
        srfi-1 srfi-13 srfi-40
        posix
        util io-util
        ducts)
  (files http/mime))

(define-structure url url-interface
  (open scheme
        define-record-types
        ascii unicode
        bitwise
        srfi-1 srfi-13
        util io-util)
  (files http/url))

;;;; http
(define-structure http http-interface
  (open scheme signals
        fluids
        sockets
        define-record-types
        byte-vectors
        tables
        threads
        srfi-40 srfi-8
        util io-util
        mime ducts
        url)
  (files http/http))

(define-structure http-proxy
  (export proxy-server)
  (open scheme
        sockets
        tables
        srfi-40 srfi-8
        ducts
        io-util)
  (files http/http-proxy))

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


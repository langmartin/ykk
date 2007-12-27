;;;; add #; and #,(foo ...) to the reader
(define-structure octothorpe-extensions
  (export define-reader-ctor)
  (open scheme primitives reading extended-ports)
  (files utility/octothorpe-extensions))

;;;; Core Utilites
(define-interface util-interface
  (compound-interface
   srfi-78-interface
   (export
    ;; srfi-2
    (and-let* :syntax)
    ;; optional arguments
    (if-car :syntax)
    optional
    (let-optionals* :syntax)
    (let-optionals :syntax)
    (call/datum-rest :syntax)
    ;; general syntax
    wind-fluid
    (unless :syntax)
    (when :syntax)
    (begin1 :syntax)
    make-not
    call-while
    (while :syntax)
    (until :syntax)
    (case-equal :syntax)
    ;; lists
    map*
    depth-first
    intersperse
    list->alist
    find-first
    update-alist
    update-force-alist
    cons-alist
    (let-foldr* :syntax)
    ;; assertion
    concat-for-each
    concat
    concat-write
    (assert :syntax))))

(define-structure util util-interface
  (open scheme signals
        srfi-1 srfi-2 srfi-78
        extended-ports
        i/o-internal)
  (files utility/util))

;;;; Red/Black Trees
(define-structures ((red/black red/black-interface)
                    (red/black-inspection red/black-inspection-interface))
   (open scheme srfi-8 srfi-9 record-types primitives simple-signals pp)   
   (files (utility red-black)))

;;;; Core I/O Utilities
(define-interface io-util-interface
  (compound-interface
   extended-ports-interface
   i/o-interface
   i/o-internal-interface
   (export
    ;; ports
    (let-port-rest :syntax)
    string/port->port
    port?
    close-port
    call-with-current-output-port
    with-current-output-port
    (let-current-output-port :syntax)
    with-current-input-port
    (let-current-input-port :syntax)
    maybe-current-input-port
    (let-maybe-current-input-port :syntax)
    call-with-string-output-port
    with-string-output-port
    (let-string-output-port :syntax)
    with-string-input-port
    (let-string-input-port :syntax)
    call-with-u8-output-port
    with-u8-output-port
    (let-u8-output-port :syntax)
    with-string-ports
    (let-string-ports :syntax)

    ;; parsing
    next-chunk-primitive
    next-chunk-for-each
    next-chunk
    not-eof-object?
    port-slurp
    string-or-chars->predicate
    crlf?
    read-crlf-line
    string-split
    whitespace?
    consume-chars

    ;; output
    disp-for-each
    disp
    writ
    output-for-each
    output

    ;; gambit like
    read-line
    read-all
    with-output-to-string
    call-with-output-string
    with-input-from-string
    call-with-input-string
    )))

(define-structure io-util io-util-interface
  (open scheme signals
        i/o i/o-internal extended-ports
        util)
  (files utility/io-util))

;;;; UUID gen
(define-structure uuid
  (export uuidgen)
  (open scheme srfi-27 bitwise)
  (files uuid))

;;;; logging cons
(define-interface logging-cons-interface
  (export
   initialize-log
   lnil
   lcons
   lcar
   lcdr
   lnull?
   lpair?
   llist?
   map*
   depth-first
   ))

(define-structure logging-cons logging-cons-interface
  (open scheme define-record-types tables i/o
        srfi-1
        util uuid)
  (files zipper/logging-cons))

;;;; I/O to support http
(define-interface duct-interface
  (export
   duct?
   duct-parent
   port->duct
   ;; duct->input-port
   (duct-extend* :syntax)
   duct-get-property
   duct-get-local-property
   duct-set-property!
   duct-read
   duct-peek
   duct-write
   duct-close
   duct-foldr
   duct-for-each
   duct->string
   duct-next-chunk-for-each
   duct-next-chunk
   ))

(define-structure duct duct-interface
  (open scheme signals define-record-types
        ascii
        text-codecs
        byte-vectors
        ports
        util
        io-util)
  (files duct))

(define-interface ducts-interface
  (compound-interface
   duct-interface
   (export
    d/byte-len
    d/http-chunked
    d/peek
    d/base64
    d/ascii
    )))

(define-structure ducts ducts-interface
  (open scheme signals
        byte-vectors bitwise ascii unicode
        text-codecs
        srfi-13
        util io-util
        url
        duct)
  (files ducts))

(define-interface mime-interface
  (export
   ;; record-type
   mime-headers
   mime-content-type
   mime-port
   mime-duct
   mime-body
   ;; procs
   mime-stream
   mime-read-all
   make-bytelen-duct
   header-cons
   header-assoc
   header-filter
   header-split
   content-type->header
   ))

(define-structure mime mime-interface
  (open scheme signals define-record-types
        reading
        unicode-char-maps text-codecs byte-vectors
        srfi-1 srfi-13
        posix
        util io-util
        ducts)
  (files mime))

(define-interface url-interface
  (export
   make-url
   url?
   url-protocol
   url-host
   url-port
   url-path
   url-parameters
   parse-url
   url=?
   url-parameter-string
   urldecode
   urldecode-string
   urlencode
   urlencode-string
   ))

(define-structure url url-interface
  (open scheme
        define-record-types
        ascii unicode
        bitwise
        srfi-1 srfi-13
        util io-util
        )
  (files url))

;;;; http
(define-interface http-interface
  (export
   http-get
   http-form-post
   http-server
   http-server-exec
   http-server-exec?
   http-server-close
   (let-http-response :syntax)
   (let-http-request :syntax)
   (let-headers :syntax)
   (let-content-length :syntax)
   (let-header-data :syntax)
   reduce-headers
   ))

(define-structure http http-interface
  (open scheme signals
        fluids
        extended-ports
        i/o-internal
        sockets
        define-record-types
        byte-vectors
        threads
        util io-util
        mime ducts url
        )
  (files http))

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


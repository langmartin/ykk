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

;;;; Core I/O Utilities
(define-interface io-util-interface
  (compound-interface
   extended-ports-interface
   i/o-interface
   i/o-internal-interface
   (export
    (call/port-rest :syntax)
    string/port->port
    port?
    call-with-string-ports
    (with-string-ports :syntax)
    close-port

    ;; parsing
    next-chunk-display
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

;;;; I/O to support http
(define-interface duct-interface
  (export
   duct?
   duct-parent
   port->duct
   duct->input-port
   (duct-extend :syntax)
   duct-get-property
   duct-get-this-property
   duct-set-property!
   duct-read-all
   duct-read
   duct-write
   duct-close
   duct-display
   duct->string
   ))

(define-structure duct duct-interface
  (open scheme signals define-record-types
        ascii
        text-codecs
        byte-vectors
        proposals
        ports
        util
        io-util)
  (files duct))

(define-interface ducts-interface
  (compound-interface
   duct-interface
   (export
    d/byte-len
    d/leave-open
    d/base64
    d/ascii
    d/unicode
    )))

(define-structure ducts ducts-interface
  (open scheme signals
        byte-vectors bitwise ascii unicode
        text-codecs
        util
        io-util
        duct)
  (files ducts))

(define-interface mime-interface
  (export
   ;; record-type
   mime-content-type
   mime-headers
   mime-body
   ;; procs
   mime-read-all
   mime-stream
   cons-header
   filter-headers
   content-type->header
   split-headers
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
   urldecode
   urldecode-string
   ))

(define-structure url url-interface
  (open scheme
        define-record-types
        srfi-13
        util io-util
        )
  (files url))

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

;;;; http
(define-interface http-interface
  (export
   http-start-server
   http-client
   call-with-http-reply
   proxy-handler
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
        url
        )
  (files http-server))

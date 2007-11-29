;;;; add #; and #,(foo ...) to the reader
(define-structure octothorpe-extensions
  (export define-reader-ctor)
  (open scheme primitives reading extended-ports)
  (files utility/octothorpe-extensions))

;;;; smallish set of core utils that I keep wanting
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
    (call/port-rest :syntax)
    string/port->port
    ;; general syntax
    wind-fluid
    (unless :syntax)
    (when :syntax)
    ;; utils
    port?
    next-chunk-display
    next-chunk
    not-eof-object?
    port-slurp
    string-or-chars->predicate
    crlf?
    disp-for-each
    disp
    writ
    output-for-each
    output
    concat-for-each
    concat
    concat-write
    ;; zipper
    map*
    depth-first
    ;; duct-work
    call-with-string-ports
    (with-string-ports :syntax)
    (begin1 :syntax)
    list->alist
    find-first
    update-alist
    update-force-alist
    close-port
    call-while
    (while :syntax)
    (until :syntax)
    (case-equal :syntax)
    ;; from mime
    make-not
    intersperse
    ;; better covered by srfi-78 (check and check-ec)
    (assert :syntax))))

(define-structure util util-interface
  (open scheme signals extended-ports i/o-internal
        srfi-1 srfi-2 srfi-78)
  (files utility/util))

;;;; UUID gen
(define-structure uuid
  (export uuidgen)
  (open scheme srfi-27 bitwise)
  (files uuid))

;;;; gambit built-in work alikes
(define-interface gambit-compat-interface
  (export
   read-line
   read-all
   with-output-to-string
   call-with-output-string
   with-input-from-string
   call-with-input-string
   ))

(define-structure gambit-compat gambit-compat-interface
  (open scheme i/o-internal extended-ports
        util)
  (files utility/gambit-compat))

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
  (open scheme signals
        define-record-types
        ports extended-ports
        ascii
        text-codecs
        i/o i/o-internal
        byte-vectors
        proposals
        util)
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
        i/o i/o-internal extended-ports
        byte-vectors bitwise ascii unicode
        text-codecs
        util
        duct)
  (files ducts))

(define-interface mime-interface
  (export
   MIME:parse-content-type
   MIME:read-headers
   mime-read-all
   mime-stream
   cons-header
   filter-headers
   content-type->header
   split-headers
   ))

(define-structure mime mime-interface
  (open scheme signals
        reading i/o-internal extended-ports define-record-types
        unicode-char-maps text-codecs i/o byte-vectors
        srfi-1 srfi-13
        posix
        util gambit-compat
        ducts)
  (files mime))

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
   http:respond
   http:start-server
   http:get
   http:post
   http:put
   http:delete
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
        util gambit-compat
        mime
        )
  (files http-server))

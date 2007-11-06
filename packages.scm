;;;; add #; and #,(foo ...) to the reader
(define-structure octothorpe-extensions
  (export define-reader-ctor)
  (open scheme primitives reading extended-ports)
  (files octothorpe-extensions))

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
    (or-eof :syntax)
    ;; better covered by srfi-78 (check and check-ec)
    (assert :syntax))))

(define-structure util util-interface
  (open scheme signals extended-ports i/o-internal
        srfi-1 srfi-2 srfi-78)
  (files util))

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
   call-with-input-string))

(define-structure gambit-compat gambit-compat-interface
  (open scheme i/o-internal extended-ports
        util)
  (files gambit-compat))

;;;; I/O to support http
(define-structure urlencoding
  (export urlencode-display
          urlencode
          urldecode)
  (open scheme srfi-13 ascii bitwise extended-ports
        util)
  (files urlencoding))

(define-interface duct-interface
  (export duct?
          duct-parent
          port->duct
          duct->input-port
          (duct-extend :syntax)
          duct-read-all
          duct-read
          duct-write
          duct-close
          d/byte-len
          d/leave-open
          d/base64
          d/ascii
          d/unicode))

(define-structure duct duct-interface
  (open scheme signals
        srfi-1
        define-record-types byte-vectors
        ports extended-ports
        i/o i/o-internal proposals
        bitwise ascii unicode text-codecs
        util)
  (files duct))

(define-structure mime
  (export next-token
          skip-while
          http-token-char?
          MIME:parse-content-type
          MIME:read-headers
          set-content-parser!)
  (open scheme signals
        reading i/o-internal extended-ports define-record-types
        unicode-char-maps text-codecs i/o byte-vectors tables
        srfi-13 srfi-40
        posix
        util gambit-compat
        duct)
  (files mime))

;;;; logging cons
(define-interface logging-cons-interface
  (export initialize-log
          lnil
          lcons
          lcar
          lcdr
          lnull?
          lpair?
          llist?
          map*
          depth-first))

(define-structure logging-cons logging-cons-interface
  (open scheme define-record-types tables i/o
        srfi-1
        util uuid)
  (files zipper/logging-cons))

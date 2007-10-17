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
    ;; all related to optional arguments
    (if-car :syntax)
    (let-optionals* :syntax)
    (let-optionals :syntax)
    optional
    (call/port-rest :syntax)
    string/port->port
    ;; general syntax
    wind-fluid
    (unless :syntax)
    (when :syntax)
    ;; utils
    next-chunk
    string-or-chars->predicate
    crlf?
    concat
    list->alist
    ;; better covered by srfi-78 (check and check-ec)
    (assert :syntax))))

(define-structure util util-interface
  (open scheme signals extended-ports i/o-internal
        srfi-1 srfi-2 srfi-78
        gambit-compat)
  (files util))

;;;; UUID gen
(define-structure uuid
  (export uuidgen)
  (open scheme srfi-27 bitwise)
  (files uuid))

;;;; gambit built-in work alikes
(define-interface gambit-compat-interface
  (export port?
          with-output-to-string
          call-with-output-string
          with-input-from-string
          call-with-input-string))

(define-structure gambit-compat gambit-compat-interface
  (open scheme i/o-internal extended-ports)
  (files gambit-compat))

;;;; I/O to support http
(define-structure urlencoding
  (export urlencode-display
          urlencode
          urldecode)
  (open scheme srfi-13 ascii bitwise extended-ports
        util)
  (files urlencoding))

(define-structure mime
  (export next-token
          skip-while
          http-token-char?
          MIME:parse-content-type
          MIME:read-headers)
  (open scheme signals reading i/o-internal unicode-char-maps extended-ports
        srfi-1 srfi-13
        util gambit-compat)
  (files mime))

(define-structure base64
  (export base64-encode-vector
          base64-encode-port
          base64-encode-string
          base64-decode-string
          base64-decode-port)
  (open scheme extended-ports bitwise ascii byte-vectors srfi-78
        util)
  (files base64))

;; (define-structure io-http
;;   (export http-server)
;;   (open scheme define-record-types i/o-internal srfi-1 srfi-13 signals
;;         util urlencoding mime))

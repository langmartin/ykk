;;;; add #; and #,(foo ...) to the reader
(define-structure octothorpe-extensions
  (export define-reader-ctor)
  (open scheme primitives reading extended-ports)
  (files octothorpe-extensions))

;;;; smallish set of core utils that I keep wanting
(define-interface util-interface
  (export
   (if-car :syntax)
   optional
   port-dot-rest
   string/port->port
   wind-fluid
   (unless :syntax)
   (when :syntax)
   (and-let* :syntax)
   next-chunk
   string-or-chars->predicate
   crlf?))

(define-structure util util-interface
  (open scheme signals extended-ports i/o-internal srfi-2
        gambit-compat unit-testing)
  (files util))

;;;; a simple unit testing interface
(define-interface unit-testing-interface
  (export
   (define-functional-test :syntax)
   (define-functional-tests :syntax)
   (define-effecting-test :syntax)
   (define-effecting-tests :syntax)
   run-functional-tests
   run-effecting-tests))

(define-structure unit-testing unit-testing-interface
  (open scheme signals)
  (files unit-testing))

;;;; UUID gen
(define-structure uuid
  (export uuidgen)
  (open scheme srfi-27 bitwise)
  (files uuid))

;;;; gambit built-in work alikes
(define-interface gambit-compat-interface
  (export
   port?
   with-output-to-string
   call-with-output-string
   with-input-from-string))

(define-structure gambit-compat gambit-compat-interface
  (open scheme i/o-internal extended-ports
        util)
  (files gambit-compat))

;;;; I/O to support http
(define-structure urlencoding
  (export
   urlencode-display
   urlencode
   urldecode)
  (open scheme srfi-13 ascii bitwise extended-ports
        util unit-testing)
  (files urlencoding))

;; (define-structure io-http io-http-interface
;;   (open scheme define-record-types i/o-internal srfi-1 srfi-13
;;         util unit-testing urlencoding))

;;;; add #; and #,(foo ...) to the reader
(define-structure octothorpe-extensions
  (export define-reader-ctor)
  (open scheme primitives reading extended-ports)
  (files octothorpe-extensions))

;;;; smallish set of core utils that I keep wanting
(define-interface util-interface
  (export
   if-car
   optional
   port-dot-rest
   string/port->port
   wind-fluid
   unless
   when
   and-let*
   next-chunk
   string-or-chars->predicate
   crlf?))

(define-structure util util-interface
  (open scheme signals extended-ports i/o-internal
        gambit-compat unit-testing)
  (files util))

;;;; a simple unit testing interface
(define-interface unit-testing-interface
  (export
   define-functional-test
   define-functional-tests
   define-effecting-test
   run-functional-tests
   run-effecting-tests))

(define-structure unit-testing unit-testing-interface
  (open scheme signals)
  (files testing))

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
  (open scheme i/o-internal)
  (files gambit-compat))

;;;; I/O to support http
;; (define-structure io-http io-http-interface
;;   (open scheme define-record-types i/o-internal
;;         util unit-testing))

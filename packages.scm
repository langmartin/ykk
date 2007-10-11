;;;; add #; and #,(foo ...) to the reader
(define-structure octothorpe-extensions
  (export define-reader-ctor)
  (open scheme primitives reading extended-ports)
  (files octothorpe-extensions))

;;;; smallish set of core utils that I keep wanting
(define-interface util-interface
  (export
   optional
   port-dot-rest
   wind-fluid
   next-chunk
   string-or-chars->predicate
   crlf?))

(define-structure util util-interface
  (open scheme signals extended-ports)
  (files util))

;;;; a simple unit testing interface
(define-interface unit-testing-interface
  (export
   define-functional-test
   define-effecting-test
   run-functional-tests
   run-effecting-tests))

(define-structure unit-testing unit-testing-interface
  (open scheme signals util)
  (files testing))

;;;; UUID gen
(define-structure uuid
  (export uuidgen)
  (open scheme srfi-27 bitwise)
  (files uuid))

;;;; gambit built-in functions
(define-interface gambit-compat-interface
  (export
   with-output-to-string
   call-with-output-string
   with-input-from-string))

(define-structure gambit-compat gambit-compat-interface
  (open scheme i/o-internal)
  (files gambit-compat))

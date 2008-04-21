;;;; Language
(define-structure octothorpe-extensions
  (export define-reader-ctor)
  (open scheme primitives reading extended-ports)
  (files octothorpe-extensions))

(define-interface extra-scheme-interface
  (compound-interface
   (interface-of scheme)
   (interface-of srfi-71)
   (interface-of srfi-26)
   scheme-extensions))

(define checking-interface
  (export (define-checked :syntax)
          (check :syntax)
          (wildcard :syntax)))

(define-structure checking checking-interface
  (for-syntax (open scheme srfi-1 names))
  (open scheme
        assert
        simple-signals
        meta-methods)
  (files check))

(define-structure extra-scheme extra-scheme-interface
  (open (modify scheme (hide cond let let* letrec define))
        srfi-61                         ; steroidal cond
        srfi-71                         ; steroidal let
        language-ext
        (modify checking (rename (define-checked define)))))

;;;; core utilites
(define-interface optional-arguments-interface
  (export
   ;; optional arguments
   (if-car :syntax)
   (if-cdr :syntax)
   (let-optionals* :syntax)
   (let-optionals :syntax)
   (call/datum-rest :syntax)))

(define-structure optional-arguments
  optional-arguments-interface
  (open scheme)
  (files optional-arguments))

(define-interface ykk-ports-interface
  (compound-interface
   (interface-of extended-ports)
   (interface-of i/o)
   (interface-of i/o-internal)
   (export
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
    maybe-current-output-port
    (let-maybe-current-output-port :syntax)
    call-with-string-output-port
    with-string-output-port
    (let-string-output-port :syntax)
    call-with-string-input-port
    with-string-input-port
    (let-string-input-port :syntax)
    call-with-u8-output-port
    with-u8-output-port
    (let-u8-output-port :syntax)
    with-string-ports
    (let-string-ports :syntax))))

(define-structure ykk-ports
  ykk-ports-interface
  (open scheme
        i/o i/o-internal extended-ports
        optional-arguments)
  (files ykk-ports))

(define-interface assert-interface
  (export
   concat-for-each
   concat
   concat-write
   concat->symbol
   (assert :syntax)))

(define-structure assert
  assert-interface
  (open scheme
        signals
        ykk-ports)
  (files assert))

(define-interface more-regexps-interface
  (export (case-regex :syntax)
          (case-posix-regex :syntax)))

(define-structure more-regexps more-regexps-interface
  (open scheme
        regexps
        posix-regexps)
  (files regexps))

(define-interface scheme-extensions-interface
  (export
   
   (unless :syntax)
   (when :syntax)
   (begin1 :syntax)
   make-not
   (case-equal :syntax)

   promise?
   maybe-force
   always?
   never?
   proj-0 proj-1 proj-2
   
   ;; part of big-util
   error
   breakpoint
   atom?
   null-list?
   neq?
   n=
   identity
   no-op
   concatenate-symbol))

(define-structure scheme-extensions
  scheme-extensions-interface
  (open scheme
        simple-signals
        big-util
        assert)
  (files language-ext))

(define-interface ykk-parsing-interface
  (export
   next-chunk-primitive
   next-chunk-for-each
   next-chunk
   not-eof-object?
   port-slurp
   string-or-chars->predicate
   crlf?
   read-crlf-line
   read-line
   port-fold
   port-fold-right
   read-all
   string-split
   whitespace?
   consume-chars
   escape-by-doubling
   escape-and-quote))

(define-structure ykk-parsing
  ykk-parsing-interface
  (open scheme
        signals
        assert
        extended-ports
        optional-arguments
        ykk-ports
        srfi-13)
  (files ykk-parsing))

(define-interface monad-style-output-interface
  (export
    disp-for-each
    disp
    writ
    output-for-each
    output))

(define-structure monad-style-output
  monad-style-output-interface
  (open scheme
        assert
        optional-arguments
        ykk-ports)
  (files monad-style-output))

(define-interface fluids+-interface
  (compound-interface
   (interface-of fluids)
   (export (define-fluid :syntax))))

(define-structure fluids+ fluids+-interface
  (for-syntax (open scheme fluids))
  (open scheme fluids)
  (files fluids))

(define-interface exceptions-interface
  (compound-interface
   (interface-of handle)
   (interface-of simple-signals)
   (interface-of simple-conditions)
   (export
    (define-condition :syntax)
    with-condition
    (let-condition :syntax)
    raise-condition
    with-exception-catcher
    condition-stuff)))

(define-structure exceptions
  exceptions-interface
  (open scheme
        handle
        simple-signals
        simple-conditions
        fluids+)  
  (files exceptions))

(define-interface dates-interface
  (export date->time
          smart/date->time
          time->date))

(define-structure dates dates-interface
  (open scheme
        load-dynamic-externals
        external-calls
        posix-time
        regexps
        more-regexps
        exceptions
        srfi-26)
  (files dates))

;;;; Data
;; FIXME: remove when graph is refactored
(define-interface data-definition-interface
  (export (define-tagged-list-data :syntax)))

(define-structure data-definition data-definition-interface
  (open extra-scheme
        methods)
  (files data-def))

(define-interface red/black-interface
  (export r/b-make-tree
          r/b-number-tree
          r/b-symbol-tree
          r/b-string-tree

          r/b-tree?
          r/b-empty?
          r/b-ref

          r/b-insert
          r/b-maybe-replace
          r/b-insert-set

          r/b-delete
          r/b-delete-set

          r/b-lfold))

(define-interface red/black-inspection-interface
  (export r/b-tree->node-list
          r/b-tree/in-order->list))

(define-structures ((red/black red/black-interface)
                    (red/black-inspection red/black-inspection-interface))
   (open scheme srfi-8 srfi-9 record-types primitives simple-signals pp)
   (files (utility red-black-constructed-from-records)
          (utility red-black)
          (utility vred-black)))

(define-structures ((list-red/black red/black-interface)
                    (list-red/black-inspection red/black-inspection-interface))
   (open scheme srfi-8 srfi-9 simple-signals pp)
   (files red-black-constructed-from-lists
          red-black
          vred-black))

(define-interface set-interface
  (export make-set
          empty-set

          set?
          empty?
          in-set?
          set-ref
          set=?
          set<=?
          set->list

          lfold-set

          adjoin adjoin-list
          remove remove-list
          union
          intersection
          difference))

(define-structure rb-set set-interface
  (open scheme red/black srfi-1 assert)
  (files set-red-black vset))

(define-structure list-set set-interface
  (open scheme srfi-1 srfi-9+ assert)
  (files (utility set-list)
         vset))

(define set rb-set)

(define-interface primitive-types-interface
  (export :maybe-symbol maybe-symbol?
          :maybe-sexpr maybe-sexpr?
          :sexpr sexpr?
          :code-block code-block?))

(define-structure primitive-types primitive-types-interface
  (open scheme
        methods)
  (files primitive-types))
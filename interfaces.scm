;;;; Primitive
(define-interface fluids+-interface
  (export make-fluid
          (define-fluid :syntax)
	  let-fluid
	  let-fluids
	  fluid

	  fluid-cell-ref
	  fluid-cell-set!

	  set-fluid!))

(define-interface ykk/bindings-interface
  (export new-binding
          new-value-binding
          binding?
          binding-type
          binding-value
          cast-binding))

(define-interface ykk/bindings-internal-interface
  (export binding->s48-binding))

(define-interface ykk/names-interface
  (export make-name-table
          define-names
          delete-names
          lookup-name
          fold-names))

(define-interface ykk/names-inspection-interface
  (export list-names))

(define-interface ykk/environments-interface
  (export empty-environment
          extend-environment
          forget-bindings
          cast-bindings
          current-environment
          with-current-environment
          environment-ref
          lookup))

(define-interface environment-manipulation-interface
  (export (subset :syntax)
          (with-prefix :syntax)
          (modify :syntax)
          prefix
          expose
          hide
          alias
          rename))

(define-interface ykk/environments-internal-interface
  (export carefully
          %environment-ref
          extend-with-native-package
          native-package->ykk-definitions))

(define-interface ykk/evaluation-interface
  (export safe-evaluation-environment
          safe-eval
          safe-eval->env
          safe-eval/extend))

;;;; Red/Black Trees
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

;;;; Sets
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

;;;; ykk methods
(define-interface ykk/methods-interface
  (export (define-generic :syntax)
	  (define-method :syntax)
	  (define-simple-type :syntax)
          (with-generic :syntax)
	  :values
	  :value
	  :number
	  :complex
	  :real
	  :rational
	  :integer
	  :exact-integer
	  :boolean
	  :symbol
	  :char
	  :null
	  :pair
	  :vector
	  :string
	  :procedure
	  :input-port
	  :output-port
	  :eof-object
	  :record
	  :record-type
	  :zero
	  singleton
	  disclose &disclose))

;;;; core utilites
(define-interface optional-arguments-interface
  (export
   ;; optional arguments
   (if-car :syntax)
   (if-cdr :syntax)
   (let-optionals* :syntax)
   (let-optionals :syntax)
   (call/datum-rest :syntax)))

(define-interface alists-interface
  (export
   list->alist
   update-alist
   update-force-alist
   cons-alist
   (let-foldr* :syntax)))

(define-interface assert-interface
  (export
   concat-for-each
   concat
   concat-write
   (assert :syntax)))

(define-interface language-ext-interface
  (export
   (unless :syntax)
   (when :syntax)
   (begin1 :syntax)
   make-not
   (case-equal :syntax)))

(define-interface srfi-1+-interface
  (compound-interface
   srfi-1-interface
   (export
    intersperse
    fold-append)))

(define-interface the-interface-formerly-know-as-util
  (compound-interface
   assert-interface
   srfi-1+-interface
   srfi-2-interface                     ; and-let*
   srfi-13-interface
   srfi-78-interface                    ; check
   big-util-interface
   language-ext-interface
   alists-interface
   optional-arguments-interface))

(define-interface ykk-ports-interface
  (compound-interface
   extended-ports-interface
   i/o-interface
   i/o-internal-interface
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

(define-interface oleg-style-parsing-interface
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
   read-all
   string-split
   whitespace?
   consume-chars))

(define-interface monad-style-output-interface
  (export
    disp-for-each
    disp
    writ
    output-for-each
    output))

(define-interface the-interface-formerly-know-as-io-util
  (compound-interface
   ykk-ports-interface
   oleg-style-parsing-interface
   monad-style-output-interface))

(define-interface exceptions-interface
  (export
   with-exception-catcher
   condition-stuff))

;;;; ducts
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
   duct-next-chunk))

(define-interface ducts-interface
  (compound-interface
   duct-interface
   (export
    d/byte-len
    d/http-chunked
    d/peek
    d/base64
    d/ascii
    d/characters
    d/null)))

;;;; mime & url
(define-interface mime-interface
  (export
   ;; record-type
   mime-headers
   mime-content-type
   mime-port
   mime->byte-duct
   mime->duct
   ;; interface
   mime-stream
   mime-read-all
   header-cons
   header-assoc
   header-filter
   header-split
   xfer-chunked?
   null-header))

(define-interface url-interface
  (export
   make-url
   url?
   url-protocol
   url-host
   url-port
   url-path
   url-parameters
   url-parameters?
   parse-url
   url=?
   url-parameter-string
   urldecode
   urldecode-string
   urlencode
   urlencode-string))

;;;; http
(define-interface http-interface
  (export
   http-get
   http-form-post
   http-server
   http-server-exec
   http-server-exec?
   http-server-close
   call/http-version
   proxy-client
   (let-http-response :syntax)
   (let-http-request :syntax)
   (let-headers :syntax)
   (let-content-length :syntax)
   (let-header-data :syntax)
   header-reduce
   http-keepalive?
   proxy-server))

;;;; zippers, kernel
(define-interface zipper-interface
  (export
   zipper
   zipper?
   z-k
   z-curr-node
   zip-all-the-way-up))

(define-interface zlist-logging-interface
  (export
   reopen-log-file
   replay-log-port))

(define-interface zcons-record-interface
  (export
   cons-cons
   pair?
   cons-id
   cons-next
   cons-car
   cons-cdr
   cons-set-cdr!))

(define-interface zlist-interface
  (export
   cons
   null
   list
   list?
   pair?
   null?
   car
   cdr))

(define-interface zlist-srfi-1-interface
  (export
   map*
   for-each
   list-tail
   fold
   fold-pair
   fold-right
   fold-pair-right
   depth-first))

(define-interface top-level-data-interface
  (export
   top-ref
   top-set
   top-del))

(define-interface process-interface
  (export
   process-cons
   process?
   process-auth
   process-block
   fold-blocked
   fold-running))

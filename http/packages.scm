(define-syntax define-meta-structure
  (syntax-rules ()
    ((_ struct (package ...) body ...)
     (define-structure struct
       (compound-interface
        (interface-of package)
        ...)
       (open package ...)
       body ...))))

(define-meta-structure scheme+
  (extra-scheme
   assert
   ykk-ports
   ykk-parsing
   monad-style-output
   optional-arguments
   list
   srfi-2
   srfi-9+
   string
   alist
   exceptions))

;;;; ducts
(define-interface duct-internal-interface
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
   whitespace?
   duct-next-chunk-for-each
   duct-next-chunk))

(define-structure duct-internal duct-internal-interface
  (open scheme+
        ascii
        text-codecs
        byte-vectors)
  (files duct-internal))

(define-interface ducts-interface
  (compound-interface
   duct-internal-interface
   (export
    d/byte-len
    d/http-chunked
    d/peek
    d/base64
    d/ascii
    d/characters
    d/null)))

(define-structure ducts ducts-interface
  (open scheme+
        byte-vectors
        bitwise
        ascii
        unicode
        text-codecs
        url
        duct-internal)
  (files ducts))

;;;; mime & url, http
(define-interface mime-interface
  (export
   ;; record-type
   mime-headers
   mime-content-type
   mime-content-type-type
   mime-port
   set-mime-port!
   mime->byte-duct
   mime->duct
   ;; interface
   port->mime-stream
   port->mime-list
   header-cons
   header-assoc
   header-filter
   header-split
   header-null
   header-null?
   xfer-chunked?))

(define-structure mime mime-interface
  (open scheme+
        reading
        unicode-char-maps
        text-codecs
        byte-vectors
        posix
        srfi-40
        ducts
        url)
  (files mime))

(define-interface url-interface
  (export
   for-each-display
   cons-parameter
   make-url
   url?
   url-protocol
   url-host
   url-port
   url-path
   url-parameters
   url-parameters?
   parse-url
   parse-url-path
   url=?
   url-foldr-parameters
   url-parameter-string
   urldecode
   urldecode-string
   urlencode
   urlencode-string
   set-parameters
   add-parameters-to-path))

(define-structure url url-interface
  (open scheme+
        ascii
        unicode
        bitwise)
  (files url))

(define-structure curl
  (export
   curl*
   curl-ssl->string)
  (open scheme
        srfi-8
        signals
        posix
        i/o
        ykk-ports
        srfi-1
        assert
        url)
  (files curl))

(define-structure csv
  (export read-csv-record
          display-as-csv-field
          display-as-csv-row
          display-as-csv-table)
  (open scheme
        signals)
  (files csv))

(define-structure json
  (export alist->json-obj
          list->json-arr
          json-fold-right               ; cons nil duct . ocons opair
          )
  (open scheme+
        ducts)
  (files json))

(define-interface http-interface
  (export
   http-get
   http-form-post
   http-server
   http-server-exec
   http-server-exec?
   http-server-close
   ;; client forms v1
   (let-multithreaded :syntax)
   (let-http-response :syntax)
   (let-http-request :syntax)
   (let-headers :syntax)
   (let-header-data :syntax)
   header-reduce
   ;; std handler
   http-keepalive?
   http-register-page!
   http-register-code-handler!
   standard-http-server
   make-request
   with-request
   request-version
   request-method
   request-url
   request-parameters
   request-path
   request-headers
   get-parameters
   standard-parameters
   set-standard-host!
   ;; client forms v2
   http-response
   begin-http-response
   begin-content-length
   status
   header
   header-delete
   header-clear-all))

(define-structure http
  (compound-interface http-interface (interface-of url))
  (open scheme+
        sockets
        byte-vectors
        tables
        threads
        srfi-40
        srfi-8
        exceptions
        posix-files
        mime
        url
        ducts
        json
        ssax-vanilla
        fluids+
        htmlprag ; for tests
        )  
  (files http
         protocol
         http-standard-dispatch))

(define http-protocol-interface
  (export status-code->phrase))

(define-structure http-protocol http-protocol-interface
  (open scheme+
        tables
        assert)
  (files protocol))

(define-structure standard-test
  (export)
  (open scheme+
        htmlprag
        http
        mime
        url
        json)
  (files standard-test))

(define-interface pages-interface
  (export
   (define-resource :syntax)
   (response :syntax)
   (page :syntax)
   (reset-page :syntax)
   (page-response :syntax)
   (simulate-request :syntax)
   (method-case :syntax)

   simple-error

   moved-permanently
   redirect-found   
   see-other
   temporary-redirect
   bad-request
   forbidden
   not-found
   method-not-allowed
   server-error
   not-implemented
   ))

(define-structure pages pages-interface
  (for-syntax
   (open extra-scheme
         syntax-util
         http-protocol
         list alist))  
  (open extra-scheme
        syntax-util
        list string
        http http-protocol
        htmlprag
        exceptions
        ykk-ports monad-style-output
        assert)
  (files page page-responses))
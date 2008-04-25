;;;; richer standard dispatch
(define (output-response output-port version response)
  (let-current-output-port
      output-port
    (if (or #t (string-ci=? version "HTTP/1.0")) ; no difference for now
        (output version
                " "
                response))))

(define (call/http-version port proc)
  (let* ((lst (string-split (read-crlf-line port) whitespace? 3))
         (lst (if (< (length lst) 3)
                  (list (car lst) (cadr lst) #f)
                  lst)))
    (apply proc lst)))

(define (perform-standard-output result version input-port output-port)
  (begin1
   (cond ((http-server-exec? result)
          (if (exec-output result)
              (output-response output-port version (exec-output result)))
          result)
         (else
          (output-response output-port version result)))
   (force-output output-port)
   (close-output-port output-port)
   (close-input-port input-port)))

(define *fixed-pages* (make-string-table))

(define *fixed-code-handlers* (make-integer-table))

(define (http-register-page! path request-receiver)
  (table-set! *fixed-pages* path request-receiver))

(define (http-register-code-handler! code handler)
  (table-set! *fixed-code-handlers* code handler))

(define (handle-status-code code . params)
  (and-let* ((h (table-ref *fixed-code-handlers* code)))
    (apply h params)))

(define-record-type rtd/request
  (really-make-request version method url query head)
  request?
  (version request*-version)
  (method request*-method)
  (url request*-url)
  (query request*-parameters set-request*-parameters!)
  (head request*-headers))

(define (make-request version method url query head)
  (let* ((method (or (header-assoc 'X-HTTP-Method-Override head) method))
         (param-method query (pluck-alist (_method) query)))
    (really-make-request version (string->normal-symbol (or param-method method)) url query head)))

(define-fluid ($request #f)
  current-request
  with-request)

(define (request-version) (request*-version (current-request)))
(define (request-method) (request*-method (current-request)))
(define (request-url) (request*-url (current-request)))
(define (request-path) (url-path (request-url)))
(define (request-parameters) (request*-parameters (current-request)))
(define (request-headers) (request*-headers (current-request)))
(define (get-parameters) (url-parameters (request-url)))

(define (path->list path)
  (let ((split
         (map (lambda (x)
                (let ((sub (string-split x #\space)))
                  (if (and (pair? sub) (pair? (cdr sub)))
                      sub
                      x)))
              (string-split path #\/))))
    (if (equal? split '(""))
        '("/")
        (cons (string-append "/" (cadr split))
              (cddr split)))))

(assert (path->list "/path/foo/bar") => '("/path" "foo" "bar")
        (path->list "/") => '("/")
        (path->list "/path/foo bar/") => '("/path" ("foo" "bar")))

(define (mime->form-parameters mime)
  (let-string-input-port
      (duct->string (mime->duct mime))
    (url-foldr-parameters cons-parameter '() (current-input-port))))

(define (catch-query mime)
  (case (mime-content-type-type mime)
    ((application/x-www-form-urlencoded)
     (mime->form-parameters mime))
    ((application/jsonrequest application/x-json application/json)
     (json-fold-right cons '() (mime->duct mime)))
    ((text/xml application/xml)
     (let-string-input-port
         (duct->string (mime->duct mime))
       (ssax:xml->sxml (current-input-port) 'xml)))
    (else #f)))

(define (debug-catch-query mime)
  (let ((raw (duct->string (mime->duct mime))))
    (note "mime"
          (mime-content-type-type mime)
          raw)
    (call-with-string-input-port
     raw
     (lambda (port)
       (set-mime-port! mime port)
       (catch-query mime)))))

(define *standard-host* "localhost")

(define (set-standard-host! hostname)
  (set! *standard-host* hostname))

(define (standard-parameters)
  (cons (url-parameters (request-url))
        (request-parameters)))

(define (standard-handler* handler input-port output-port)
  (call/http-version
   input-port
   (lambda (method path version)
     (call-with-values
         (lambda () (parse-url-path path))
       (lambda (path param)
         (let* ((mime (stream-car (port->mime-stream input-port)))
                (head (mime-headers mime))
                (host (or (header-assoc 'host head) *standard-host*)))
           (let ((url (make-url 'http host 80 path param)))
             (with-request
              (make-request version method url (or (catch-query mime) '()) head)
              (lambda ()
                (perform-standard-output
                 (handler)
                 version
                 input-port
                 output-port))))))))))

(define (file-name-extension path-string)
  (string-downcase!
   (let* ((len (string-length path-string))
          (idx (string-index-right path-string #\.)))
     (if idx
         (substring path-string idx len)
         ""))))

(define (file-regular? path)
  (and (accessible? path (access-mode read))
       (let ((info (get-file-info path)))
         (eq? (file-info-type info) (file-type regular)))))

(define (handle-existing-file path)
  (let ((path (string-append "." path)))
   (if (not (file-regular? path))
       #f
       (let-http-response (200 "File")
         (header-cons
          'content-type
          (case-equal
              (file-name-extension path)
            (("css") "text/css")
            (("js") "application/x-javascript")
            (("html" "htm") "text/html")
            (else
             "text/plain"))
          header-null)
         (call-with-input-file
             path
           (lambda (p) (read-line p #f)))))))

(define (tablewise-handler)
  (with-exception-catcher
   (lambda (e p) (or (handle-status-code 500 e) (p)))
   (lambda ()
     (let ((path (request-path)))
       (or (and-let* ((page (table-ref *fixed-pages* path)))
             (page))
           (handle-existing-file path)
           (and-let* ((path (path->list path))
                      (page (table-ref *fixed-pages* (car path))))
             (apply page (cdr path)))
           (handle-status-code 404))))))

(define (standard-handler input output)
  (standard-handler* tablewise-handler input output))

(define-syntax let-multithreaded
  (syntax-rules ()
    ((_ handler)
     (lambda args
       (spawn
        (lambda ()
          (apply handler args)))))))

(define (standard-http-server . ip/port/threaded/handler)
  (let-optionals* ip/port/threaded/handler
      ((ip 'ip)
       (port 3130)
       (threaded #f)
       (handler standard-handler))
    (http-server ip port (if threaded
                             (let-multithreaded handler)
                             handler))))

;;;; New page form
(define-fluid ($http-status '())
  current-http-status
  with-http-status)

(define (make-status code . text)
  (let-optionals* text ((text (status-code->phrase code)))
    (list code #\space text crlf)))

(define (status . rest)
  (set-fluid! $http-status (apply make-status rest))
  "")

(define-fluid ($http-headers '())
  current-http-headers
  with-http-headers)

(define (set-headers! val)
  (set-fluid! $http-headers val)
  "")

(define (header key val)
  (set-headers!
   (cons (cons key val)
         (current-http-headers))))

(define (header-delete key)
  (set-headers!
   (filter (lambda (pair)
             (eq? key (car pair)))
           (current-http-headers))))

(define (header-clear-all)
  (set-headers! '()))

(define-syntax begin-http-response
  (syntax-rules ()
    ((_ . body)
     (http-response 200 () body))))

(define-syntax http-response
  (syntax-rules ()
    ((_ ("expand") status headers body)
     (let-fluids
      $http-status (make-status . status)
      $http-headers (http-response ("headers") headers)
      (lambda ()        
        (let ((res (begin-content-length . body)))
          (list (current-http-status)
                (header-reduce (current-http-headers))
                res)))))
    ((_ (code text) headers body)
     (http-response ("expand") (code text) headers body))    
    ((_ code headers body)
     (http-response ("expand") (code) headers body))
    ((_ ("headers") ((k v) ...))
     (list (cons 'k v) ...))
    ((_ ("headers") ())
     '())))

(assert
 (let-string-output-port
  (output
   (begin-http-response
    (shtml->html
     `(html
       (head (title "foo"))
       (body
        (h1 "foo bar")
        (p "some text" ,(header 'content-type "text/html")))))))) =>
       "200 OK\r
content-type: text/html\r
content-length: 89\r
\r
<html><head><title>foo</title></head><body><h1>foo bar</h1><p>some text</p></body></html>")

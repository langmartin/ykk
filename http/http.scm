(define crlf "\r\n")

(define (output-debug . args)
  (let ((real (current-output-port))
        (body (let-string-output-port
               (apply output args))))
    (display body real)
    (let-current-output-port
        (current-error-port)
      (output "output-debug\n" body "\n\n"))))

(define-record-type :server-exec
  (http-server-exec% thunk output)
  http-server-exec?
  (thunk  exec-thunk)
  (output exec-output))

(define (http-server-exec thunk . out)
  (http-server-exec% thunk out))

(define (http-server-close)
  (http-server-exec (lambda () #t)))

(define (exec thunk)
  ((thunk)))

(define (http-server ip port handler)
  (let ((socket (open-socket port)))
   (dynamic-wind
       (lambda () #t)
       (lambda ()
         (socket-port-number socket)    ; make sure it's open
         (let lp ()
           (let ((result
                  (call-with-values
                      (lambda () (socket-accept socket))
                    handler)))
             (if (http-server-exec? result)
                 (exec (exec-thunk result)) ; extra points if this makes sense
                 (lp)))))
       (lambda () (close-socket socket)))))

;;;; HTTP Client
(define-syntax let-http-response
  (syntax-rules ()
    ((_ (code message) body ...)
     (list
      (list code " " message crlf)
      body ...))
    ((_ code (headers ...) body ...)
     (list
      (list code " " (lookup-http-code-text code))
      (let-headers (headers ...) body ...)))))

(define-syntax let-headers
  (syntax-rules ()
    ((_ () body ...)
     (list body ...))
    ((_ ((key val) (k1 v1) ...)
        body ...)
     (let ((key val))
       (cons (list 'key ": " val crlf)
             (let-headers
              ((k1 v1) ...)
              body ...))))))

(assert
 (let-headers ((foo 3) (bar foo)) 5) =>
 '((foo ": " 3 "\r\n") (bar ": " 3 "\r\n") 5))

(define (vector->content-length vec)
  (let ((len (byte-vector-length vec)))
    (if (zero? len)
        crlf
        (list (list 'content-length ": " len crlf crlf)
              (lambda ()
                (write-block vec 0 len (current-output-port)))))))

(define (begin-content-length . body)
  (vector->content-length
   (let-u8-output-port
    (output body))))

(define-syntax let-header-data
  (syntax-rules ()
    ((_ ((key val) ...))
     (let-header-data (key val) ...))
    ((_) '())
    ((_ (key val) (key1 val1) ...)
     (letrec ((key val))
       (cons (cons 'key val)
             (let-header-data (key1 val1) ...))))))

(define (header-reduce . header-lists)
  (map (lambda (pair)
         (let ((sym val (uncons pair)))
           (list sym ": " val crlf)))
       (reverse
        (apply fold-append
               (lambda (pair acc)
                 (if (assq (car pair) acc)
                     acc
                     (cons pair acc)))
               '()
               header-lists))))

(define (http-keepalive? headers)
  (or (and-let* ((conn (header-assoc 'connection headers)))
        (not (string=? "close" conn)))
      (and-let* ((conn (header-assoc 'proxy-connection headers)))
        (string=? "keep-alive" conn))))

(assert
 (let-string-output-port
  (output
   (let-http-response (200 "Ok")
     (let-headers ((user-agent "scheme48") (host "coptix.com"))
       (let-headers ((content-type "text/plain"))
         (begin-content-length
          "Some text goes here.")))))) =>
          "200 Ok\r
user-agent: scheme48\r
host: coptix.com\r
content-type: text/plain\r
content-length: 20\r
\r
Some text goes here.")

(define (http-form-post/method method url)
  (receive
   (input output)
   (proxy-client
    "HTTP/1.1" method url '()
    (let-headers ((content-type "text/x-url-form-encoded"))
      (begin-content-length
       (url-parameter-string url))))
   (close-output-port output)
   input))

(define (http-form-post url-rec)
  (http-form-post/method "POST" url-rec))

(define (http-get/method method url)
  (receive
   (input output)
   (proxy-client
    "HTTP/1.1" method url
    (header-cons 'accept "*" header-null)
    crlf)
   (close-output-port output)
   input))

(define (http-get url)
  (http-get/method "GET" url))

(define (http-get->mime url)
  (let ((port (http-get/method "GET" url)))
    (call/http-version
     port
     (lambda (version code text)
       (stream-car (port->mime-stream port))))))

(define-syntax let-http-request
  (syntax-rules ()
    ((_ (get ...) body ...)
     (list
      (list get ...)
      body ...))))

;;;; proxy
(define *client-keep-alive* (make-string-table))

(define (store-client-connection host in out)
  (table-set! *client-keep-alive*
              host
              (cons in out)))

(define (fetch-client-connection host)
  (and-let* ((found (table-ref *client-keep-alive*
                               host)))
    (values (car found) (cdr found))))

(define (proxy-client-headers url)
  (let-header-data
   ((host (url-host url))
    (connection "close")
    (accept-encoding "identity"))))

(define *proxy-client-filter*
  '(keep-alive))

(define (client-connect host port)
  (let ((conn (table-ref *client-keep-alive* host)))
    (if conn
        (values (car conn) (cdr conn))
        (socket-client host port))))

(define (proxy-client version method path headers body)
  (let* ((url (if (url? path) path (parse-url path)))
         (host (url-host url))
         (port (url-port url)))
    (receive
     (input-port output-port)
     (client-connect host port)
     (let-current-output-port
         output-port
       (let ((getp (and (url-parameters? url)
                        (cons #\? (url-parameter-string url)))))
         (output
          (let-http-request
           (method " " (url-path url) getp " " version crlf)
           (header-filter
            *proxy-client-filter*
            (header-reduce (proxy-client-headers url) headers)))
          body)
         (force-output (current-output-port))
         (values input-port output-port))))))

(define (proxy-mime port)
  (stream-car (port->mime-stream port)))

(define (proxy-req-body mime)
  (list crlf
        (lambda ()
          (proxy-body mime))))

(define *proxy-reply-headers*
  (let-header-data
   ((transfer-encoding "identity"))))

(define (proxy-body mime)
  (duct-for-each
   display
   ((d/characters)
    (mime->byte-duct mime)))
  (force-output (current-output-port)))

(define (proxy-client-handler thunk)
  (with-exception-catcher
   (lambda (c propagate)
     (let-http-response (500 "Proxy Failure")
       (let-headers ((content-type "text/plain"))
         (begin-content-length
          "500\n"
          "https requests are not yet supported.\n"
          "non-existent hostname?\n"
          (condition-stuff c)))))
   thunk))

(define (proxy-handler version method path port)
  (let* ((url (parse-url path))
         (host (url-host url))          ; proxy req include the host
         (mime (proxy-mime port))
         (head (mime-headers mime)))
    (proxy-client-handler
     (lambda ()
       (receive
        (input output)
        (proxy-client version method url head (proxy-req-body mime))
        (call/http-version
         input
         (lambda (version code text)
           (let* ((mime (proxy-mime input))
                  (head (mime-headers mime)))
             (let-http-response
              (code text)
              (header-reduce *proxy-reply-headers*
                             head)
              (begin-content-length
               (proxy-body mime)
               (if (and #f (http-keepalive? head)) ; disabled
                   (store-client-connection host input output)
                   (begin
                     (close-output-port output)
                     (close-input-port input)))))))))))))

(define (proxy-handler-wrapper)
  (lambda (input-port output-port)
    (call/http-version
     input-port
     (lambda (method path version)
       (perform-standard-output
        (proxy-handler version method path input-port)
        version
        input-port
        output-port)))))

(define (proxy-server . debugging-flag)
  (http-server
   'ip
   3128
   (if (null? debugging-flag)
       proxy-handler-wrapper
       (let-multithreaded proxy-handler))))

;;;; Manual tests (they have side effects, and rely on url content
;; (define (test-get-coptix)
;;  (let ((p (http-get "http://coptix.com")))
;;    (begin1
;;     (read-line p #f)
;;     (close-input-port p))))

;; (define (test-proxy)
;;   (let-string-ports
;;      *request*
;;    (output-response
;;     (current-output-port)
;;     "HTTP/1.1"
;;     (proxy-handler
;;      "HTTP/1.1" "GET" "/index" (current-input-port)))))

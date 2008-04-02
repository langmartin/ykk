;;;; Utility
;; (define-syntax let-list
;;   (syntax-rules ()
;;     ((_ (bindings expr) body ...)
;;      (apply expr (lambda bindings body ...)))))

;; (define-syntax thunk
;;   (syntax-rules ()
;;     ((_ body ...)
;;      (lambda () body ...))))

;; (define (extend this by)
;;   (lambda ()
;;     (this)
;;     (by)))

;; (define (for-each-pair proc lst)
;;   (for-each (lambda (pair)
;;               (proc (car pair)
;;                     (cdr pair)))
;;             lst))

(define (alist-iterator proc)
  (lambda (x)
    (proc (car x) (cdr x))))

;; (define (crlf . port)
;;   (apply disp
;;          (append port
;;                  '(#\return #\newline))))

(define crlf "\r\n")

(define-syntax or-filter
  (syntax-rules ()
    ((_ test?) #f)
    ((_ test? thing else ...)
     (let ((tmp thing))
       (if (test? tmp)
           tmp
           (or-filter test? else ...))))))

(assert (or-filter string? 'foo 'bar 'baz) => #f)

(define-syntax quoted-alist
  (syntax-rules ()
    ((_) '())
    ((_ (key val) (key1 val1) ...)
     (cons (cons 'key val)
           (quoted-alist (key1 val1) ...)))))

(assert (quoted-alist (foo 4) (bar 6)) => '((foo . 4) (bar . 6)))

(define (output-debug . args)
  (let ((real (current-output-port))
        (body (let-string-output-port
               (apply output args))))
    (display body real)
    (let-current-output-port
        (current-error-port)
      (output "output-debug\n" body "\n\n"))))

;;;; Server
(define (http-server-exec? obj)
  (and (pair? obj)
       (eq? http-server-exec? (car obj))))

(define (http-server-exec thunk . out)
  (let-optionals* out ((out #f))
    (list http-server-exec? thunk out)))

(define exec-thunk cadr)
(define exec-output caddr)

(define (http-server-close)
  (http-server-exec (lambda () #t)))

(define (http-server ip port handler)
  (let ((handler (handle-handler handler))
        (socket (open-socket port)))
    (socket-port-number socket)         ; make sure it's open
    (let lp ()
      (let ((result
             (call-with-values
                 (lambda () (socket-accept socket))
               handler)))
        (if (http-server-exec? result)
            (begin
              (close-socket socket)
              ((exec-thunk result)))
            (lp))))))

(define (handle-handler handler)
  (lambda (input-port output-port)
    (call/http-version
     input-port
     (lambda (method path version)
       (let ((result (handler version method path input-port)))
         (cond ((http-server-exec? result)
                (if (exec-output result)
                    (output-response output-port version (exec-output result)))
                result)
               (else
                (output-response output-port version result)
                (force-output output-port)
                (close-output-port output-port)
                (close-input-port input-port))))))))

(define (output-response output-port version response)
  (let-current-output-port
      output-port
    (if (or #t (string-ci=? version "HTTP/1.0")) ; no difference for now
        (output version
                " "
                response))))

;; (define (output-head head)
;;   (for-each-pair (lambda (key val)
;;                    (disp key ": ")
;;                    (if (procedure? val)
;;                        (val)
;;                        (display val))
;;                    (crlf))
;;                  head)
;;   (crlf))

;; (assert
;;  (let-string-ports
;;   "" (output-head '((host . coptix.com) (content-length . 456)))) =>
;;   "host: coptix.com\r\ncontent-length: 456\r\n\r\n")


;; (define merge-headers update-alist)

;; (define headers list)

;; (define (content-length body)
;;   `(content-length . ,(byte-vector-length body)))

(define (call/http-version port proc)
  (let* ((lst (string-split (read-crlf-line port) whitespace? 3))
         (lst (if (< (length lst) 3)
                  (list (car lst) (cadr lst) #f)
                  lst)))
    (apply proc lst)))

;; (define (http-status code text)
;;   (concat code " " text))

;;;; HTTP Client
(define (body->byte-vector body)
  (let-u8-output-port
   (output body)))

(define (output-content-length . body)
  (let* ((vec (body->byte-vector body))
         (len (byte-vector-length vec)))
    (if (zero? len)
        (output crlf)
        (begin
          (output 'content-length ": " len crlf crlf)
          (write-block vec 0 len (current-output-port))))))

(assert
 (let-string-output-port (output-content-length "some stuff" "goes here")) =>
 "content-length: 19\r\n\r\nsome stuffgoes here")

;; (quoted-alist (foo 1) (bar 2))

(assert (let-header-data (foo 1) (bar foo)) => '((foo . 1) (bar . 1)))

;; (define-gambit-style-parameter current-headers '() $current-headers)

;; (define (inject-header-alist alist)
;;   (current-headers
;;    (header-reduce (current-headers)
;;                    alist))
;;   #f)

(define-syntax let-http-response
  (syntax-rules ()
    ((_ (code message) body ...)
     (list
      (list code " " message crlf)
      body ...))))

(define-syntax let-http-request
  (syntax-rules ()
    ((_ (get ...) body ...)
     (list
      (list get ...)
      body ...))))


;; (letrec ((key val) ...)
;;        (list
;;         (let-headers "headers" (key val) ...)
;;         body ...)))
;;     ((_ "headers") '())
;;     ((_ "headers" (key val) (key1 val1) ...)
;;      (cons (list 'key ": " val crlf)
;;            (let-headers "headers" (key1 val1) ...)))

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

(define-syntax let-content-length
  (syntax-rules ()
    ((_ body ...)
     (lambda ()
       (output-content-length
        body ...)))))

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
  (map (alist-iterator
        (lambda (sym val)
          (list sym ": " val crlf)))
       (reverse
        (apply fold-append
               (lambda (pair acc)
                 (if (assq (car pair) acc)
                     acc
                     (cons pair acc)))
               '()
               header-lists))))

;; (header-reduce
;;  '((accept . "*")
;;    (connection . "close")
;;    (host . "www.google-analytics.com")
;;    )
;;  '((cache-control . "no-cache")
;;    (pragma . "no-cache")
;;    (referer . "http://coptix.com/")
;;    (proxy-connection . "keep-alive")
;;    (keep-alive . "300")
;;    (accept-charset . "ISO-8859-1,utf-8;q=0.7,*;q=0.7")
;;    (accept-encoding . "gzip,deflate")
;;    (accept-language . "en-us,en;q=0.5")))

(define (http-keepalive? headers)
  (or (and-let* ((conn (header-assoc 'connection headers)))
        (not (string=? "close" conn)))
      (and-let* ((conn (header-assoc 'proxy-connection headers)))
        (string=? "keep-alive" conn))))

(assert
 (let-string-output-port
  (output
   (let-http-response
    (200 "Ok")
    (let-headers
     ((user-agent "scheme48")
      (host "coptix.com"))
     (let-headers
      ((content-type "text/plain"))
      (let-content-length
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
   (proxy-client "HTTP/1.1" method url '()
                 (let-headers
                  ((content-type "text/x-url-form-encoded"))
                  (let-content-length
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
    (header-cons 'accept "*" null-header)
    crlf)
   (close-output-port output)
   input))

(define (http-get url)
  (http-get/method "GET" url))

(define (test-get-coptix)
 (let ((p (http-get "http://coptix.com")))
   (begin1
    (read-line p #f)
    (close-input-port p))))

;;;; richer standard dispatch
(define *fixed-pages* (make-string-table))

(define (http-register-page! path request-receiver)
  (table-set! *fixed-pages* path request-receiver))

(define-record-type request
  (make-request version method url query)
  request?
  (version request-version)
  (method request-method)
  (url request-url)
  (query request-query set-request-query!))

(define (standard-404 R)
  (let-http-response (404 "Not Found")
    (let-headers ((content-type "text/plain"))
      (let-content-length
       "404\n The path "
       (url-path (request-url R))
       " is not registered.\n\n"
       (request-version R) newline
       (request-method R) newline
       (request-url R) newline
       (request-query R)))))

;; (with-request
;;  (make-request "http/1.0" "GET" "http://coptix.com")
;;  (let-content-length
;;   "404\n The path "
;;   (url-path (request-url))
;;   " is not registered."))

(define (catch-query mime port)
  (case (mime-content-type-type mime)
    ((application/x-www-form-urlencoded)
     (url-foldr-parameters cons '() port))
    ((application/jsonrequest)
     (json-fold-right cons '() port))
    ((text/xml)
     (ssax:xml->sxml port))
    (else
     mime)))

(define *standard-host* "localhost")

(define (set-standard-host! hostname)
  (set! *standard-host* hostname))

(define (standard-handler version method path port)
  (call-with-values
      (lambda () (parse-url-path path))
   (lambda (path param)
     (let* ((mime (proxy-mime))
            (head (mime-headers mime))
            (host (or (header-assoc 'host head)
                      *standard-host*)))
       (let* ((url (make-url 'http host 80 path param))
              (R (make-request version
                               method
                               url
                               (catch-query mime port))))
         (note "url" (url-path (request-url R)))
         (or (and-let* ((page (table-ref *fixed-pages* (url-path url))))
               (page R))
             (standard-404 R)))))))

(define-syntax let-multithreaded
  (syntax-rules ()
    ((_ handler)
     (lambda args
       (spawn
        (lambda ()
          (apply handler args)))))))

(define (standard-http-server . ip/port/threaded)
  (let-optionals* ip/port/threaded ((ip 'ip) (port 3130) (threaded #f))
    (http-server ip port
                 (if threaded
                     (let-multithreaded standard-handler)
                     standard-handler))))

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
  (stream-car (mime-stream port)))

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
     (let-http-response
      (500 "Proxy Failure")
      (let-headers
       ((content-type "text/plain"))
       (let-content-length
        (lambda ()
          (output "500\n"
                  "https requests are not yet supported.\n"
                  "non-existent hostname?\n"
                  (condition-stuff c)))))))
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
              (let-content-length
               (lambda ()
                 (proxy-body mime)
                 (if (and #f (http-keepalive? head)) ; disabled
                     (store-client-connection host input output)
                     (begin
                       (close-output-port output)
                       (close-input-port input))))))))))))))

;; (lambda ()
;;   (output-debug
;;    "reply"
;;    (header-reduce *proxy-reply-headers* head)))

;; (define *request* "Host: coptix.com\r
;; Content-type: text/plain\r
;; content-length: 34\r
;; \r
;; dddddddddddddddddddddddddddddddd\r\n")

;; (define (test-proxy)
;;   (let-string-ports
;;      *request*
;;    (output-response
;;     (current-output-port)
;;     "HTTP/1.1"
;;     (proxy-handler
;;      "HTTP/1.1" "GET" "/index" (current-input-port)))))

(define (proxy-server . debugging-flag)
  (http-server
   'ip
   3128
   (if (null? debugging-flag)
       proxy-handler
       (let-multithreaded proxy-handler))))

;; (output '(("GET" " " "/css/t.css" #f " " "HTTP/1.1" "\r\n")
;;           ((accept ": " "*" "\r\n")
;;            (host ": " "coptix.com" "\r\n")) #f))

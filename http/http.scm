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

(define-syntax define-dynamic-variable
  (syntax-rules ()
    ((proc default real-fluid)
     (begin
       (define real-fluid (make-fluid default))
       (define (proc . new)
         (let-optionals* new ((new #f))
           (if new
               (begin
                 (set-fluid! real-fluid new)
                 new)
               (fluid current-headers$))))))))

(define-syntax quoted-alist
  (syntax-rules ()
    ((_) '())
    ((_ (key val) (key1 val1) ...)
     (cons (cons 'key val)
           (quoted-alist (key1 val1) ...)))))

(assert (quoted-alist (foo 4) (bar 6)) => '((foo . 4) (bar . 6)))

(define (fold-append kons nil lst . lsts)
  (if (null? lst)
      (if (null? lsts)
          nil
          (apply fold-append kons nil (car lsts) (cdr lsts)))
      (apply fold-append kons (kons (car lst) nil) (cdr lst) lsts)))

(assert (fold-append cons '() '(1 2) '(3 4) '(5 6)) => '(6 5 4 3 2 1))

(define (output-debug label . args)
  (let ((real (current-output-port))
        (body (let-string-output-port
               (apply output args))))
    (display body real)
    (let-current-output-port
        (current-error-port)
      (output "output-debug: " label newline)
      (display body))))

;;;; Server
(define (http-server-exec thunk)
  (cons http-server-exec?
        thunk))

(define exec-thunk cdr)

(define (http-server-close)
  (http-server-exec (lambda () #t)))

(define (http-server-exec? obj)
  (and (pair? obj)
       (eq? http-server-exec? (car obj))))

;; (define-record-type response rtd/response
;;   (make-response status head body)
;;   (status r/status)
;;   (head r/head)
;;   (body r/body))

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
            ((exec-thunk result))
            (lp))))))

(define (http-multithreaded-server ip port handler)
  (let ((handler (handle-handler handler))
        (socket (open-socket port)))
    (socket-port-number socket)         ; make sure it's open
    (let lp ()
      (call-with-values
          (lambda () (socket-accept socket))
        (lambda (input output)
          (spawn
           (lambda ()
             (handler input output)))))
      (lp))))

(define (handle-handler handler)
  (lambda (input-port output-port)
    (call/http-version
     input-port
     (lambda (method path version)
       (let ((res (handler version method path input-port)))
         (if (http-server-exec? res)
             res
             (output-response output-port version res)))))))

(define (output-response output-port version response)
  (let-current-output-port
      output-port
    (if (or #t (string-ci=? version "HTTP/1.0")) ; no difference for now
        (output version
                " "
                response))
    (force-output output-port)
    (close-output-port output-port)))

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
  (apply
   proc (string-split (read-crlf-line port)
                      whitespace?
                      3)))

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
       (output-content-length body ...)))))

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

;;;; proxy
(define (get-style? method)
  (case-equal method
    (("GET") #t)
    (else #f)))

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
    (connection "close"))))

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
       (let ((getp (and (get-style? method)
                        (url-parameters? url)
                        (cons #\? (url-parameter-string url)))))
         (output
          ;; "request"
          (let-http-request
           (method " " (url-path url) getp " " version crlf)
           (header-filter
            *proxy-client-filter*
            (header-reduce (proxy-client-headers url) headers))))
         (output body)
         (force-output (current-output-port))
         (values input-port output-port))))))

(define (proxy-mime port)
  (stream-car (mime-stream port)))

(define (proxy-req-body mime)
  (list crlf
        (lambda ()
          (duct-for-each
           display
           (mime->byte-duct mime)))))

(define *proxy-reply-headers*
  (let-header-data
   ((transfer-encoding "identity"))))

(define (proxy-body mime)
  (duct-for-each display
                 ((d/characters)
                  (mime->byte-duct mime))))

(define (proxy-handler version method path port)
  (let* ((url (parse-url path))
         (host (url-host url))          ; proxy req include the host
         (mime (proxy-mime port))
         (head (mime-headers mime)))
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
           #;
           (lambda ()
             (output-debug
              "reply"
              (header-reduce *proxy-reply-headers* head)))
           (header-reduce *proxy-reply-headers* head)
           (let-content-length
            (lambda ()
              (proxy-body mime)
              (if (and #f (http-keepalive? head)) ; disabled
                  (store-client-connection host input output)
                  (begin
                    (close-output-port output)
                    (close-input-port input))))))))))))

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

(define (proxy-server)
  (http-multithreaded-server 'ip 8080 proxy-handler))

;; (output '(("GET" " " "/css/t.css" #f " " "HTTP/1.1" "\r\n")
;;           ((accept ": " "*" "\r\n")
;;            (host ": " "coptix.com" "\r\n")) #f))

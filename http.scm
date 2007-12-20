;;;; Utility
(define-syntax let-list
  (syntax-rules ()
    ((_ (bindings expr) body ...)
     (apply expr (lambda bindings body ...)))))

(define-syntax thunk
  (syntax-rules ()
    ((_ body ...)
     (lambda () body ...))))

(define (extend this by)
  (lambda ()
    (this)
    (by)))

(define (for-each-pair proc lst)
  (for-each (lambda (pair)
              (proc (car pair)
                    (cdr pair)))
            lst))

(define (crlf . port)
  (apply disp
         (append port
                 '(#\return #\newline))))

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

(define (fold-append kons nil lst . lsts)
  (if (null? lst)
      (if (null? lsts)
          nil
          (apply fold-append kons nil (car lsts) (cdr lsts)))
      (apply fold-append kons (kons (car lst) nil) (cdr lst) lsts)))

(assert (fold-append cons '() '(1 2) '(3 4) '(5 6)) => '(6 5 4 3 2 1))

;;;; Server
(define (server-close-object) server-close-object)
(define (server-close-object? obj) (eq? obj server-close-object))

;; (define-record-type response rtd/response
;;   (make-response status head body)
;;   (status r/status)
;;   (head r/head)
;;   (body r/body))

(define (http-server ip port handler)
  (let ((handler (handle-handler handler)))
    (let lp ()
      (let ((result
             (call-with-values
                 (lambda () (socket-accept (open-socket port)))
               handler)))
        (or (server-close-object? result)
            (lp))))))

(define (handle-handler handler)
  (lambda (input-port output-port)
    (call/http-version
     input-port
     (lambda (version method path)
       (handler version method path input-port)))))

(define (output-response output-port version reponse)
  (let-current-output-port
      output-port
    (if (or #t (string-ci=? version "HTTP/1.0")) ; no difference for now
        (output version
                " "
                response)
        (force-output output-port))))

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

(define (call/http-version port proc/3)
  (apply proc/3
         (string-split
          (read-crlf-line port))))

;; (define (http-status code text)
;;   (concat code " " text))

;;;; HTTP Client
(define (body->byte-vector body)
  (let-u8-output-port
   (output body)))

(define (output-content-length . body)
  (let* ((vec (body->byte-vector body))
         (len (byte-vector-length vec)))
    (output 'content-length ": " len crlf crlf)
    (write-block vec 0 len (current-output-port))))

(assert
 (let-string-output-port (output-content-length "some stuff" "goes here")) =>
 "content-length: 19\r\n\r\nsome stuffgoes here")

;; (quoted-alist (foo 1) (bar 2))

(assert (let-header-data (foo 1) (bar foo)) => '((foo . 1) (bar . 1)))

;; (define-gambit-style-parameter current-headers '() $current-headers)

;; (define (inject-header-alist alist)
;;   (current-headers
;;    (reduce-headers (current-headers)
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

(define-syntax let-headers
  (syntax-rules ()
    ((_ ((key val) ...) body ...)
     (letrec ((key val) ...)
       (list
        (list (let-headers "headers" (key val) ...))
        body ...)))
    ((_ "headers") '())
    ((_ "headers" (key val) (key1 val1) ...)
     (cons (list 'key ": " val crlf)
           (let-headers "headers" (key1 val1) ...)))))

(define-syntax let-content-length
  (syntax-rules ()
    ((_ body ...)
     (lambda ()
       (output-content-length body ...)))))

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

;; (define (http-client method url . version)
;;   (define (get? method)
;;     (string=? "GET" method))
;;   (let-optionals* version ((version "HTTP/1.0"))
;;     (let ((url (if-not (url? url) (parse-url url))))
;;       (call-with-values
;;           (lambda () (socket-client (url-host url) (url-port url)))
;;         (lambda (input-port output-port)
;;           (let-current-output-port
;;               output-port
;;             (let* ((params (url-parameters url))
;;                    (params (and (not (null? params))
;;                                 (url-parameter-string url)))
;;                    (urlreq (and (get? method)
;;                                 params
;;                                 (list "?" params))))
;;               (output
;;                (let-http-request
;;                 (method " " (url-path url) urlreq " " version crlf)
;;                 (let-headers
;;                  ((user-agent "scheme48") (host (url-host url)))
;;                  (or (get? method)
;;                      (let-headers
;;                       ((content-type "text/x-url-form-encoded"))
;;                       (let-content-length params))))))
;;               (force-output (current-output-port))
;;               (close-output-port (current-output-port))
;;               input-port)))))))

(define (http-post-form/method method url)
  (let ((url (if (url? url) url (parse-url url))))
    (proxy-client "HTTP/1.1" method url '()
                  (let-headers
                   ((content-type "text/x-url-form-encoded"))
                   (let-content-length
                    (url-parameter-string url))))))

(define (http-get/method method url)
  (proxy-client "HTTP/1.1" method url '() body))

(define (http-get url)
  (http-get/method "GET" url))

(define (http-delete url)
  (http-get/method "DELETE" url))

(define (http-put url)
  (http-post/method "PUT" url))

;; (let ((port (http-get (make-url 'http "coptix.com" 80 "/index.php" '()))
;;        ;; (http-get "http://coptix.com/")
;;        ))
;;   (let ((r (read-line port #f)))
;;     (close-input-port port)
;;     r))

(define (reduce-headers . header-lists)
  (apply
   (fold-append (lambda (pair acc)
                  (if (assq (car pair) acc)
                      acc
                      (cons pair acc)))
                '()
                header-lists)))

(define-syntax let-header-data
  (syntax-rules ()
    ((_) '())
    ((_ (key val) (key1 val1) ...)
     (letrec ((key val))
       (cons (cons 'key val)
             (let-header-data (key1 val1) ...))))))

;;;; Proxy
(define (get-style? method)
  (case-equal method
    (("GET") #t)
    (else #f)))

(define (proxy-client version method path headers body)
  (let ((url (if (url? path) path (parse-url path)) ))
    (call-with-values
        (lambda () (socket-client (url-host url) (url-port url)))
      (lambda (input-port output-port)
        (let-current-output-port
            output-port
          (let ((getp (and (get-style? method)
                           (cons #\?
                                 (url-parameter-string url)))))
            (output
             (let-http-request
              (method " " (url-path url) getp " " version crlf)
              (reduce-headers
               (let-header-data (host (url-host url)))
               headers)
              body))
            (force-output (current-output-port))
            (close-output-port (current-output-port))
            input-port))))))

(define (fcar lst)
  (car (force lst)))

(define (proxy-mime port k)
  (let* ((mime (fcar (mime-stream port)))
         (headers (mime-headers mime))
         (duct (make-bytelen-duct headers (mime-port mime))))
    (k mime headers duct)))

(define (proxy-handler version method path port)
  (proxy-mime
   port
   (lambda (mime headers duct)
     (let ((port (proxy-client
                  version
                  method
                  path
                  headers
                  (list
                   crlf
                   (lambda ()
                     (duct-for-each display duct))))))
       (call/http-version
        port
        (lambda (version code text)
          (proxy-mime
           port
           (lambda (mime headers duct)
             (let-http-response
              (code text)
              headers
              crlf
              (lambda ()
                (duct-for-each display duct)))))))))))

(define *request* "Host: foo.com\r
Content-type: text/plain\r
content-length: 34\r
\r
dddddddddddddddddddddddddddddddd\r\n")

#;
(let-string-ports
    *request*
 (call-with-values
     (lambda () (proxy-handler (current-input-port) "GET" "/foo/bar"))
   (lambda (status headers body)
     (output-response
      (current-output-port)
      "HTTP/1.1"
      status
      header
      body))))

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

;;;; Server
(define (server-close-object) server-close-object)
(define (server-close-object? obj) (eq? obj server-close-object))

(define-record-type response rtd/response
  (make-response status head body)
  (status r/status)
  (head r/head)
  (body r/body))

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
  (lambda (port output-port)
    (let-list ((version method path) (string-split (read-line port)))
      (call-with-values
          (lambda () (handler port method path))
        (lambda (status head body)
          (output-response output-port
                           version
                           status
                           head
                           body))))))

(define-syntax http-send-headers
  (syntax-rules ()
    ((_ (tag val) ...)
     (begin
       (output-head
        (let-foldr* cons-alist '() (tag val) ...))))))


(define (output-content-length body-vector)
  (let ((len (byte-vector-length body-vector)))
      (output 'content-length ": " ))
  (crlf)
  (crlf)
  (write-block ))

(let ((port (http-client "GET" (make-url 'http "coptix.com" 80 "/index.php" '()))))
  (let ((r (read-line port #f)))
    (close-input-port port)
    r))

(define (output-response output-port version status header body)
  (if (or #t (string-ci=? version "HTTP/1.0")) ; no difference for now
      (let* ((body (body->byte-vector body))
             (head
              (merge-headers
               (headers
                (content-length body)
                '(connection . close))
               header)))
        (let-current-output-port
            output-port
          (output version " " status)
          (crlf)
          (output-head head)
          (display body)))))

(define (output-head head)
  (for-each-pair (lambda (key val)
                   (disp key ": ")
                   (if (procedure? val)
                       (val)
                       (display val))
                   (crlf))
                 head)
  (crlf))

(assert
 (let-string-ports
  "" (output-head '((host . coptix.com) (content-length . 456)))) =>
  "host: coptix.com\r\ncontent-length: 456\r\n\r\n")

(define (body->byte-vector body)
  (let-u8-output-port
   (output body)))

(define merge-headers update-alist)

(define headers list)

(define (content-length body)
  `(content-length . ,(byte-vector-length body)))

(define (call/http-version port proc/3)
  (apply proc/3
         (string-split
          (read-crlf-line port))))

(define (http-status code text)
  (concat code " " text))

;;;; Proxy
(define (fcar lst)
  (car (force lst)))

(define (proxy-handler port method path)
  (let ((port (http-client method path)))
    (call/http-version
     port
     (lambda (version code text)
       (let* ((mime (fcar (mime-stream port)))
              (duct (make-bytelen-duct (mime-headers mime)
                                       (mime-port mime))))
         (values
          (http-status code text)
          headers
          (lambda ()
            (duct-for-each display #f duct))))))))

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




(define-syntax letrec-alist*
  (syntax-rules ()
    ((_ (key val) ...)
     (let-foldr* cons-alist '() (key val) ...))))

(define-syntax http-message
  (syntax-rules (let-status let-headers let-content-length)
    ((_ one rest ...)
     (list
      (http-message one)
      (http-message rest ...)))
    ((_ (let-status (code message) body ...))
     (list (list code " " message crlf)
           (http-message body ...)))
    ((_ (let-headers ((key val) ...) body ...))
     (letrec ((key val) ...)
       (list (http-message "headers" (key val) ...)
             (http-message body ...))))
    ((_ "headers" (key val))
     (cons (cons 'key val) '()))
    ((_ "headers" (key val) (key1 val1) ...)
     (cons (cons 'key val)
           (http-message "headers" (key1 val1) ...)))
    ((_ (let-content-length body ...))
     (lambda ()
       (output-content-length
        (body-vector body ...))))))

(define (http-client method url . version)
  (define (get? method)
    (string=? "GET" method))
  (let-optionals* version ((version "HTTP/1.0"))
   (let ((url (if (url? url) url (parse-url url))))
     (call-with-values
         (lambda () (socket-client (url-host url) (url-port url)))
       (lambda (input-port output-port)
         (let-current-output-port
             output-port
           (let* ((params (url-parameters url))
                  (params (and (not (null? params))
                               (url-parameter-string url))))
             (output method " "
                     (url-path url)
                     (and (get? method)
                          params
                          (list "?" params))
                     " "
                     version
                     crlf)

             (http-message
              (let-headers
               ((user-agent "scheme48") (host (url-host url)))
               (if (not (get? method))
                   (let-headers
                    (content-type "text/x-url-form-encoded")
                    (let-content-length
                     params)))
               ))
             
             (let ((body (and (not (get? method))
                              params)))
               (if body
                   )
               (force-output (current-output-port))
               (close-output-port (current-output-port))
               input-port))))))))

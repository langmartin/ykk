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

(define (http:start-server ip port handler)
  (let ((handler (handle-handler handler)))
    (let lp ()
      (let ((result
             (call-with-values
                 (socket-accept
                  (open-socket port))
               handler)))
        (or (server-close-object? result)
            (lp))))))

(define (handle-handler handler)
  (lambda (port output-port)
    (let-list ((version method path) (string-split (read-line port)))
      (call-with-values
          (handler port method path)
        (lambda (status head body)
          (output-response output-port
                           version
                           status
                           head
                           body))))))

(define (http-client method url)
  (let* ((url (if (url? url)
                  url
                  (parse-url url)))
         (port (socket-client (url-host url)
                              (url-port url))))
    (for-each (lambda (x)
                (display x port))
              (list method
                    " "
                    (url-path url)
                    "?"
                    (url-parameter-string url)))
    (crlf port)
    port))

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
     (proxy-handler (current-input-port) "GET" "/foo/bar")
   (lambda (status headers body)
     (output-response
      (current-output-port)
      "HTTP/1.1"
      status
      header
      body))))

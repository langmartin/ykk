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

(define-syntax if-not
  (syntax-rules ()
    ((_ (test? thing) else)
     (let ((tmp thing))
       (if (test? tmp)
           tmp
           else)))))

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
      (let ((response
             (lambda () (handler version method path port))))
        (if-not (server-close-object? response)
                (output-response output-port version response))))))

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
    ((_ "headers" (key val))
     (cons (list 'key ": " val crlf)
           '()))
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

(define (http-client method url . version)
  (define (get? method)
    (string=? "GET" method))
  (let-optionals* version ((version "HTTP/1.0"))
    (let ((url (if-not (url? url) (parse-url url))))
      (call-with-values
          (lambda () (socket-client (url-host url) (url-port url)))
        (lambda (input-port output-port)
          (let-current-output-port
              output-port
            (let* ((params (url-parameters url))
                   (params (and (not (null? params))
                                (url-parameter-string url)))
                   (urlreq (and (get? method)
                                params
                                (list "?" params))))
              (let-http-request
               (method " " (url-path url) urlreq " " version crlf)
               (let-headers
                ((user-agent "scheme48") (host (url-host url)))
                (or (get? method)
                    (let-headers
                     ((content-type "text/x-url-form-encoded"))
                     (let-content-length params)))))
              (force-output (current-output-port))
              (close-output-port (current-output-port))
              input-port)))))))

(let ((port (http-client "GET" (make-url 'http "coptix.com" 80 "/index.php" '()))))
  (let ((r (read-line port #f)))
    (close-input-port port)
    r))

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

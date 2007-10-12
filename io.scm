(define (server-close-object) server-close-object)
(define (server-close-object? obj) (eq? obj server-close-object))

(define-record-type http-request
  (make-http-request version method path host query)
  http-request?
  version
  method
  path
  host
  query)

(define-record-type http-path
  (make-http-path path)
  http-path?
  path)

(define-record-type http-query
  (make-http-query mime port))

(define (start-server port handler)
  (let lp ()
    (let ((result
           (call-with-values
               (socket-accept
                (open-socket port))
             (lambda (in out)
               (handler in out)))))
      (or (server-close-object? result)
          (lp)))))

(define (http-handler in out)
  (produce-http-output
   out
   (dispatch-path
    (consume-http-input in))))

(define (consume-http-input in)
  (call-with-values
      (read-http-version in)
    (lambda (version method path)
      (let ((mime (read-mime-headers in)))
        (make-http-request
         version
         method
         (http-path-parse path)
         (mime-header "host" mime)
         (http-extract-query (mime-header "content-type" mime)))))))

(define (read-http-version port)
  (apply values
         (string-split #\space
                       (read-line port)
                       3)))



(define (read-mime-headers port)
  (define (key acc)
    (if (and (crlf? port) (crlf? port))
        '()
        (case (peek-char? port)
          ((#\space #\tab)
           (val))
          (else
           (let ((key (next-chunk ":" port)))
             (read-char)
             (cons (cons key (val))
                   (key)))))))
  (define (val)
    )
  )

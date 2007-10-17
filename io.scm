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


;; (define (handler host path connection query)
;;   #f)

;; (define (make-connection version method remote-ip browser host)
;;   (lambda (proc)
;;     (proc version method remote-ip browser host)))

(define (http-server ip port handler)
  (let ((handler (make-handler handler)))
    (let lp ()
     (let ((result
            (call-with-values
                (socket-accept
                 (open-socket port))
              handler)))
       (or (server-close-object? result)
           (lp))))))

(define (make-handler handler)
  (lambda (in out)
    (let ((v/m/p (parse-top in)))
      (apply handler in out v/m/p))))

(define (parse-top port)
  (string-split '(#\space) (read-line port) 3))



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

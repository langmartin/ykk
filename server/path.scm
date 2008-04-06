(define (call-path R x)
  (let ((proc args (uncons (string-split x " "))))
    `(lambda ()
       (list (cons (string->symbol proc) args)
             (res)))))

(define (next-path R path)
  (pair-fold-right
   (lambda (x tail)
     `(call-path R ,x ,tail))
   '()
   path))

(define (server-path R)
  (let* ((path (string-split (url-path (request-url R)) "/"))
         (path (if (string-null? (car path))
                   (cdr path)
                   path)))
    (next-path R path)))

(define (path-404 R)
  (let-http-response (200 "Ok")
    (let-headers ((content-type "text/plain"))
      (let-content-length
       (server-path R)))))

(define (go)
  (standard-http-server path-404))

(server-path (make-request
              "http/1.0"
              "get"
              (parse-url "http://localhost:3130/path/test foo/bar")
              #f))

'(call-path r ("path" "test foo" "bar")
            (call-path r ("test foo" "bar")
                       (call-path r ("bar") ())))

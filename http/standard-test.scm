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
       (request-parameters R)))))

(define-syntax http-mng
  (syntax-rules ()
    ((_ text body ...)
     (lambda (R)
       (http-server-exec
        (lambda () body ...)
        (let-http-response (220 "ok")
          (let-headers ((content-type "text/plain"))
            (let-content-length
             text))))))))

(http-register-page!
 "/stop"
 (http-mng "server stopping" #t))

(define (text name)
  `(input (@ (type "text")
             (name ,name))))

(define (code lst)
  `(pre
    ,(let-string-output-port
      (newline)
      (write lst)
      (newline))))

(http-register-page!
 "/test"
 (lambda (R)
   (let-http-response (220 "Ok")
     (let-headers ((content-type "text/html"))
       (let-content-length
        (shtml->html
         `(html
           (head (title "test"))
           (body
            
            (div
             (ul
              (li "method: " ,(request-method R))
              (li "parameters: "
                  ,(code (standard-parameters R)))))
            
            (div
             (form (@ (action "/test?foo=1&bar=2")
                      (method "post"))
                   ,(text "foo[bar]")
                   ,(text "foo[baz]")
                   (input (@ (type "submit")
                             (name "submit")
                             (value "hit me")))))
            ))))))))

(define (go)
  (standard-http-server standard-404))

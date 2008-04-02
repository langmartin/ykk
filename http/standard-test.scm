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
  (standard-http-server))

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
 "restart"
 (http-mng "server restarting" (standard-http-server)))

(http-register-page!
 "stop"
 (http-mng "server stopping" #t))

(http-register-page!
 "foo"
 (lambda (R)
   (let-http-response (220 "Ok")
     (let-headers ((content-type "text/plain"))
       (let-content-length
        "some text goes here\n"
        "that's what I'm saying")))))

(define (go)
  (standard-http-server))

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

(define (include-file file)
  (call-with-input-file
      file
    (lambda (p)
      (read-line p #f))))

(http-register-page!
 "/test"
 (lambda (R)
   (let-http-response (220 "Ok")
     (let-headers ((content-type "text/html"))
       (let-content-length
        (shtml->html
         `(html
           (head
            (title "test")
            (script
             (@ (src "http://webtools.php5.iago/js/jquery-1.2.2.min.js")))
            (script
             (@ (src "http://rco.abla2/secure/checkout/js-ext/jquery.json.js"))))
           (body
            (div ,(code (standard-parameters R)))
            (div (form (@ (action "/test?foo=1&bar=2")
                          (method "post"))
                       ,(text "foo[bar]")
                       ,(text "foo[baz]")
                       (input
                        (@ (type "submit") (name "submit") (value "hit me")))))

            (div (@ (onclick "javascript:ajaxTest('json')")
                    (id "json"))
                 "json")

            (div (@ (onclick "javascript:xmlTest()")
                    (id "xml"))
                 "xml")

            (script (@ (type "text/javascript"))
                    ,(include-file "http/standard-test.js"))
            ))))))))

(http-register-page!
 "/ajax"
 (lambda (R)
   (let-http-response (220 "Ok")
     (let-headers ((content-type "text/plain"))
       (let-content-length
        (lambda ()
          (write (standard-parameters R))))))))

(define (go)
  (standard-http-server standard-404))

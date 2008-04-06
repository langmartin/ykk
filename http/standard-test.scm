(define (standard-404)
  (let-http-response (404 "Not Found")
    (let-headers ((content-type "text/plain"))
      (let-content-length
       "404\n The path "
       (req-path)
       " is not registered.\n\n"
       (req-version) newline
       (req-method) newline
       (req-url) newline
       (req-parameters)))))

(define-syntax let-server-command-page
  (syntax-rules ()
    ((_ text body ...)
     (lambda ()
       (http-server-exec
        (lambda () body ...)
        (let-http-response (220 "ok")
          (let-headers ((content-type "text/plain"))
            (let-content-length
             text))))))))

(http-register-page!
 "/stop"
 (let-server-command-page
  "server stopping"
  #t))

(define-syntax let-content-type-page
  (syntax-rules ()
    ((_ type body ...)
     (let-http-response (220 "ok")
       (let-headers ((content-type type))
         (let-content-length
          body ...))))))

(define-syntax let-html-page
  (syntax-rules ()
    ((_ body ...)
     (let-content-type-page "text/html" body ...))))

(define-syntax let-text-page
  (syntax-rules ()
    ((_ body ...)
     (let-content-type-page "text/plain" body ...))))

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
 (lambda ()
   (let-html-page
    (shtml->html
     `(html
       (head
        (title "test")
        (script
         (@ (src "http://webtools.php5.iago/js/jquery-1.2.2.min.js")))
        (script
         (@ (src "http://rco.abla2/secure/checkout/js-ext/jquery.json.js"))))
       (body
        (div ,(code (standard-parameters)))
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
        ))))))

(http-register-page!
 "/ajax"
 (lambda ()
   (let-text-page
    (let-headers ((content-type "text/plain"))
      (lambda ()
        (write (standard-parameters)))))))

(define (go)
  (standard-http-server standard-404))

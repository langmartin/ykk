(define-syntax let-server-command-page
  (syntax-rules ()
    ((_ text body ...)
     (lambda ()
       (http-server-exec
        (lambda () body ...)
        (let-http-response (220 "ok")
          (let-headers ((content-type "text/plain"))
            (begin-content-length
             text))))))))

(http-register-code-handler!
 404
 (lambda ()
   (let-http-response (404 "Not Found")
    (let-headers ((content-type "text/plain"))
      (begin-content-length
       "404\n The path "
       (request-path)
       " is not registered.\n\n"
       (request-version) newline
       (request-method) newline
       (request-url) newline
       (request-parameters))))))

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
         (begin-content-length
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
    (lambda ()
      (write (standard-parameters))))))

(http-register-page!
 "/path"
 (lambda (foo bar)
   (let-text-page
    (lambda ()
      (write (standard-parameters)))
    newline
    "my path is " foo " and " bar)))

(http-register-page!
 "/break"
 (lambda ()
   (error "broken")))

(define (go)
  (standard-http-server))

;; (let-http-response
;;     (status 500)
;;   (header foo "bar")
;;   (header baz "blit")
  
;;   (begin-http-body
;;    type: text/html
;;    "thinngs"
;;    (header location "http://foo.com")
;;    (lambda ()
;;      (stuff))))

;; (let-http-response
;;     200
;;   ((accepts "*"))
;;   (begin-content-length
;;    (shtml->html
;;     `(*top*
;;       (head)
;;       (body
;;        (p "thing"))))))

;; (let-http-response (200 "I found it!")
;;   (let-headers ((thing "bar"))
;;     (begin-content-length
;;      "thing"
;;      "baz")))


;;;; Responses
(define (moved-permanently href)
  (redirect 301 href))

(define (redirect-found href)
  (redirect 302 href))

(define (see-other href)
  (redirect 303 href))

(define (temporary-redirect href)
  (redirect 307 href))

(define (bad-request . message)
  (apply reset/error-page 400 message))

(define (forbidden . reason)
  (apply reset/error-page 403 reason))

(define (not-found . message)
  (apply reset/error-page 404 message))

(define (method-not-allowed . allowed)
  (reset-page
   status: 405
   headers: ((allowed allowed))
   (apply simple-error
          405
          (request-method) "is not allowed."
          "Allowed methods are:" allowed)))

(define (server-error . message)
  (apply reset/error-page 500 message))

(define (not-implemented . message)
  (apply reset/error-page 501 message))

(define-syntax redirect
  (syntax-rules ()
    ((_ code href)     
     (reset-page
      status: code
      headers: ((location href))
      (redirect-template code href)))))

(define (redirect-template code href)
  (simple-error
   code
   "Follow this link if you are not automatically redirected."
   `(a (@ (href ,href)) ,href)))

;;;; Some simple response page forms
(define (reset/error-page code . explaination)
  (reset-page
   status: code
   (apply simple-error code explaination)))

(define (error-page code . explaination)
  (page
   status: code
   (apply simple-error code explaination)))

(define (simple-error code . body)
  (http-error-template
   code
   `(div ,@(reify-error-message body))))

(define (http-error-template code . body)
  (let ((phrase (status-code->phrase code)))
    `(html
      (head
       (title ,phrase))
      (body
       (h1 ,(number->string code 10) ": " ,phrase)
       ,@body))))

(define (reify-error-message message)
  (intersperse " " (map-in-order error->shtml message)))

(define (error->shtml foo)
  (cond ((or (pair? foo) (string? foo))
         foo)
        ((symbol? foo)
         (symbol->string foo))
        ((number? foo)
         (number->string foo 10))
        (else
         (output->string foo))))

;;;; Tests
(begin
  (assert (simulate-request
           method: "post"  
           (page
            (method-case
             ((get) "success"))))
          => (simulate-request
              method: "post"
              (page
               (method-not-allowed 'get))))  

  (assert
   (let-string-output-port
    (error-page 505 "foo"))
   => "505 HTTP Version not supported\r\ncontent-type: text/html\r\ncontent-length: 134\r\n\r\n<html><head><title>HTTP Version not supported</title></head><body><h1>505: HTTP Version not supported</h1><div>foo</div></body></html>")

  (simulate-request
   (page
    (see-other "http://foo.com/")))
  )

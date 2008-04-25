(define-syntax define-resource
  (syntax-rules ()
    ((_ path handler)
     (http-register-page! path handler))))

(define-syntax page
  (syntax-rules ()
    ((_ . arguments)
     (in-reset-extent
      (response
       around-body: shtml->html
       . arguments)))))

(define-syntax reset-page
  (syntax-rules ()
    ((_ . arguments)
     (make-reset-page
      (lambda ()        
        (page . arguments))))))

(define-syntax*
  (simulate-request
   (url: url "http://localhost/")
   (query: query '())
   (method: method "get")
   (headers: headers '())
   . body)
  
  (with-request
   (make-request "HTTP/1.1" method (parse-url url) query headers)
   (lambda ()
     (let-string-output-port . body))))

(define-syntax*
  (response
   (status: status 200)
   (type: content-type "text/html")
   (headers: headers ())
   (around-body: around identity)
   body)
  
  (expand-response status content-type headers (around body)))

(define-syntax method-case
  (lambda (form rename compare)
    (let ((cases (cdr form)))

      (define %case (rename 'case))
      (define %method-not-allowed (rename 'method-not-allowed))
      (define %request-method (rename 'request-method))
      (define %quote (rename 'quote))

      (define (quote-list lst)
        (map (lambda (item)
               (list %quote item))
             lst))

      (define (allowed)
        (quote-list (apply append (map car cases))))

      `(,%case (,%request-method)
               ,@cases
               ,@(if (assq 'else cases)
                     '()
                     `((else (,%method-not-allowed ,@(allowed)))))))))

;; Deprecated
(define-syntax page-response
  (syntax-rules ()
    ((_ item)
     (page-response (220 "ok") item))
    ((_ code item)
     (page-response code "text/html" item))
    ((_ code type (expr ...))
     (page-response "construct" code type (shtml->html (expr ...))))
    ((_ code type content)
     (page-response "construct" code type content))
    ((_ "construct" code type content)
     (response
      status: code
      type: type
      content))))

;;;; Implemntation Detail
(define-syntax expand-response
  (lambda (form rename compare)
    (let ((status (second form))
          (type (third form))
          (headers (map-car desyntaxify (fourth form)))
          (body (fifth form)))      

      (define %respond (rename 'http-response))

      (define (all-headers)        
        (if (assq 'content-type headers)
            headers
            (cons (list 'content-type type)
                  headers)))

      (define (status-line)
        (cond ((or (number? status) (string? status))
               `(,status ,(status-code->phrase status)))
              (else
               status)))

      `(,%respond
        ,(status-line)
        ,(all-headers)
        (,body)))))

(define-condition
  %make-reset-page (error)
  reset-page?)

(define (make-reset-page thunk)
  (%make-reset-page thunk))

(define (reset-page-thunk c)
  (car (condition-stuff c)))

(define (call-with-page-reset-extent thunk)
  (with-exception-catcher
   handle-reset-page
   thunk))

(define (handle-reset-page c prop)
  (if (reset-page? c)
      ((reset-page-thunk c))
      (prop)))

(define-syntax in-reset-extent
  (syntax-rules ()
    ((_ body)
     (call-with-page-reset-extent
      (lambda () body)))))

;;;; Tests
(begin
  (assert
   (simulate-request
    (page "hello"))
   => "200 OK\r\ncontent-type: text/html\r\ncontent-length: 5\r\n\r\nhello")

  (assert
   (simulate-request
    (page
     `(div
       "this is going well..."
       ,(reset-page status: 500 "error"))))
   => "500 Internal Server Error\r\ncontent-type: text/html\r\ncontent-length: 5\r\n\r\nerror")  
  )
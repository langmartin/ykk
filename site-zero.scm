(define-syntax define-template
  (syntax-rules (=>)
    ((_ (name arg-type) => ret-type body ...)
     (define (name arg-type)
       body
       ...))))

(define-template (page :content) => :http-response
  (http-response
   (200 "Ok")
   (let-headers
    ((content-type "text/html; charset=\"utf-8\""))
    (let-content-length
     (shtml->html
      (html
       (head (title (content title))
             (html-headers head))
       (body
        (div (@ (id "body"))
             (html-table page: content)
             (div (@ (id "footer"))
                  (p "this is just a sample"))))))))))

(define-template (csv :content)
  (http-response
   (200 "Ok")
   (let-headers
    ((content-type "text/css; charset=\"utf-8\""))
    (let-content-length
     (csv-sheet csv: :content)))))

(define (csv-sheet generic lstlst)
  (map (lambda (x)
         (list (map (lambda (x)
                      (generic x)))
               newline))
       lstlst))

(define (html-table generic lstlst)
  (map (lambda (x)
         `(tr ,@(map (lambda (x)
                       `(td ,(generic x))))))
       lstlst))

(define *template*)

(define (identity x) x)

(define (assoc-cdr x alist)
  (cond ((assoc x alist) => cdr)
        (else #f)))

(define (template-match path)
  (let lp ((path path) (template identity))
    (if (null? path)
        (values template path)
        (let ((next (assoc-cdr (car path) *template*)))
          (if (not next)
              (values template path)
              (lp (cdr path) (lambda (x)
                               (next (template x)))))))))

(define (error-404 path)
  (http-response
   (404 "Not Found")
   (let-headers
    ((content-type "text/plain; charset=\"utf-8\""))
    (let-content-length
     (lambda ()
       (write path))
     " is not found."))))

(define (dispatch version method path port)
  (let* ((url (parse-url path))
         (mime (stream-car (mime-stream port))))
    (receive
     (template path)
     (template-match path)
     (if (not template)
         (error-404 path)
         (let ((data (dispatch-data path)))
           (if (not data)
               (error-404 path)
               (template data)))))))

;; (define-syntax define-template
;;   (syntax-rules (=>)
;;     ((_ (name arg-type) => ret-type body ...)
;;      (define (name arg-type)
;;        body
;;        ...))))

(define-syntax define-template
  (syntax-rules ()
    ((_ (name content) body ...)
     (begin
       (define (name arg)
         body
         ...)
       (set! *template* (cons (cons 'name name)))))))

(define-template (page content)
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

(define-template (csv content)
  (http-response
   (200 "Ok")
   (let-headers
    ((content-type "text/css; charset=\"utf-8\""))
    (let-content-length
     (csv-sheet csv: content)))))

;;;; page parts
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

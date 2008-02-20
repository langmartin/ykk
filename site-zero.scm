(define *template* '())

(define (identity x) x)

(define (template-match path)
  (let lp ((template identity) (path path))
    (if (null? path)
        (values template path)
        (let ((next (assoc (car path) *template*)))
          (if next
              (lp (lambda (x)
                    ((cdr next)
                     (template x)))
                  (cdr path))
              (values template path))))))

(define (error-404 path)
  (http-response
   (404 "Not Found")
   (let-headers
    ((content-type "text/plain; charset=\"utf-8\""))
    (let-content-length
     (lambda ()
       (write path))
     " is not found."))))

(define (url-path->list path)
  (if (string-null? path)
      '()
      (let* ((lst0 (string-split path '(#\/)))
             (lst0 (if (string-null? (car lst0)) (cdr lst0) lst0))
             (lst (reverse lst0))
             (end (car lst))
             (lst (cdr lst))
             (idx (string-index-right end #\.)))
        (if idx
            (cons (substring end (+ idx 1) (string-length end))
                  (reverse
                   (cons (substring end 0 idx)
                         lst)))
            lst0))))

(assert
 (url-path->list "/foo/bar/baz.html") => '("html" "foo" "bar" "baz")
 (url-path->list "foo") => '("foo")
 (url-path->list "/foo") => '("foo")
 (url-path->list "") => '())

(define (dispatch version method path0 port)
  (let* ((url (parse-url path0))
         (mime (stream-car (mime-stream port)))
         (path (url-path->list (url-path url))))
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
    ((_ (name arg ...) body ...)
     (define-template name
       (lambda (arg ...) (body ...))))
    ((_ name body ...)
     (begin
       (define name body ...)
       (set! *template* (cons (cons 'name name)
                              *template*))))))

(define-template (page content)
  (let-http-response
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
             (html-table page content)
             (div (@ (id "footer"))
                  (p "this is just a sample"))))))))))

(define-template (csv content)
  (let-http-response
   (200 "Ok")
   (let-headers
    ((content-type "text/css; charset=\"utf-8\""))
    (let-content-length
     (csv-sheet csv content)))))

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


;; Web Server (thanks Lang)  ----------

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
     (let-http-response code
       (let-headers ((content-type type))
         (let-content-length content))))))

(define (dispatch url)
  (let ((host (url-host url))
        (path (url-path url))
        (query (url-parameters url)))
    (with-exception-catcher
     (lambda ()
       (handle-500 host path query))
     (lambda ()
       ((case-posix-regex path
          (".css$" load-file)
          ("^/forms/test" forms/test)
          ("^/forms/*" forms)
          ("^/$" home)
          (".*" handle-404))
        host path query)))))

(define (form-server)
  (dispatch-server dispatch 4128))

(define (handle-404 host path query)
  (page-response (404 "Not Found") (string-append "page not found: " path)))

(define (handle-500 host path query)
  (page-response (500 "Internal Server Error") "An Internal Error Occurred"))

;; Pages -------------------------------

(define *root* "/Users/james/projects/scheme/ykk/forms")

(define-syntax page-fragment
  (syntax-rules ()
    ((_ form)
     `form)))

(define-syntax page
  (syntax-rules ()
    ((_ host path query body-forms ...)
     (page-response
      (page-fragment
       (html ,(header)
             (body (div (@ (class "outer"))
                        ,(bread-crumb host path)
                        body-forms ...
                        ,(footer)))))))))

(define (map-path fn path)
  (reverse
   (let loop ((lst (string-tokenize (string-substitute "/" " " path)))
              (acc '())
              (path ""))
     (if (null? lst)
         acc
         (let ((new-path (string-append path "/" (car lst))))
           (loop (cdr lst)
                 (cons (fn (car lst) new-path) acc)
                 new-path))))))

(define (bread-crumb host path)  
  (let* ((anchors (map-path (lambda (name path)
                              (page-fragment (a (@ (href ,path)) ,name))) path)))
    (page-fragment (div (@ (class "breadcrumb"))
                        (a (@ (href "/")) ,host)
                        ,@anchors))))

(define (header . title)
  (page-fragment
   (head (title ,(if-car title "ykk devel"))
         (link (@ (rel "stylesheet") (type "text/css") (href "forms.css"))))))

(define (footer)
  (page-fragment (div (@ (class "footer")) "served by ykk")))

(define (home host path query)
  (page host path query
        (ul (li (a (@ (href "/forms")) "forms")))))

(define (forms host path query)
  (page host path query
        (h1 "Forms")))

(define (forms/test)
  "yay")

(define (load-file host path query)
  (let ((path (string-trim path #\/)))
    (with-exception-catcher
     (lambda (c prop)
       (handle-404 host path query))
     (lambda ()
       (page-response
        (with-input-from-file (string-append *root* "/" path)
          (lambda ()
            (list->string (read-all read-char)))))))))

;; Util --------------------------------

;; String
(define (string-substitute s1 s2 str)
  (let loop ((str str))
    (let ((idx (string-contains str s1)))
      (if idx
          (loop (string-append
                 (substring/shared str 0 idx)
                 s2
                 (substring/shared str (+ idx (string-length s1)))))
          str))))

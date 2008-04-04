
;; Web Server

(define at-repl? #t)

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

(define request-path url-path)
(define request-query url-parameters)
(define request-host url-host)

(define (dispatch req)
  (let ((go (lambda ()
              ((case-posix-regex (request-path req)
                 (".css$" load-file)
                 ("^/forms/test" forms/test)
                 ("^/forms/*" forms)
                 ("^/$" home)
                 (".*" handle-404)) req))))
    (if at-repl?
        (go)
        (with-exception-catcher
            (lambda (e p)
              (handle-500 req e))
          (lambda ()
            (go))))))

(define (form-server)
  (dispatch-server dispatch 4128))

(define (handle-404 req)
  (page-response (404 "Not Found") (string-append "page not found: " (request-path req))))

(define (handle-500 req error)
  (page-response (500 "Internal Server Error")
                 (string-append "An Internal Error Occurred: "
                                (let-string-output-port
                                 (display-condition error (current-output-port))))))

;; HTML

(define-syntax page
  (syntax-rules ()
    ((_ req body-forms ...)
     (page-response
      `(html ,(header)
             (body (div (@ (class "outer"))
                        ,(bread-crumb req)
                        body-forms ...
                        ,(footer))))))))

;; Templates

(define *root* "/Users/james/projects/scheme/ykk/forms")

(define (bread-crumb req)
  (let* ((anchors (cons `(a (@ (href "/")) ,(request-host req))
                        (map-path (lambda (name path)
                                    `(a (@ (href ,path)) ,name)) (request-path req)))))
    `(div (@ (class "breadcrumb"))
         ,@(intersperse " - " anchors))))

(define (header . title)
  `(head (title ,(if-car title "ykk devel"))
         (link (@ (rel "stylesheet") (type "text/css") (href "/forms.css")))))

(define (footer)
  `(div (@ (class "footer")) "served by ykk"))

;; Pages

(define (home req)
  (page req (h3 "YKK Playground")
        (ul (li (a (@ (href "/forms")) "forms")))))

(define (forms req)
  (page req (h3 "Forms")
        (ul (li "test form"))))

(define (forms/test req)
  (page req "yay"))

;; Util

(define (load-file req)
  (let ((path (string-trim (request-path req) #\/)))
    (with-exception-catcher
     (lambda (c prop)
       (handle-404))
     (lambda ()
       (page-response
        (with-input-from-file (string-append *root* "/" (request-path req))
          (lambda ()
            (list->string (read-all read-char)))))))))

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

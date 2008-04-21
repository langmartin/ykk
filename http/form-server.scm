;;;; Web Server

(define-syntax http-mng
  (syntax-rules ()
    ((_ text body ...)
     (lambda ()
       (http-server-exec
        (lambda () body ...)
        (let-http-response (220 "ok")
          (let-headers ((content-type "text/plain"))
            (let-content-length
             text))))))))

(define at-repl? #t)

(define (handle-404)
  (page-response (404 "Not Found") (string-append "page not found: " (request-path))))

(define (handle-500 error)
  (if at-repl?
      (signal-condition error)
      (page-response (500 "Internal Server Error")
                     (string-append "An Internal Error Occurred: "
                                    (let-string-output-port
                                     (display-condition error (current-output-port)))))))

(http-register-code-handler! 404 handle-404)
(http-register-code-handler! 500 handle-500)

(define (form-server)
  (display "Starting server on port 3130 [/stop to stop, /restart to restart]...\n")
  (standard-http-server))

(http-register-page!
 "/stop"
 (http-mng "stopping server..." #t))


;;;; Templates

;; FIXME!
(define *root* "/Users/james/projects/scheme/ykk/forms")

(define-syntax page
  (syntax-rules ()
    ((_ body-forms ...)
     (page-response
      `(html ,(header)
             (body (div (@ (class "outer"))
                        ,(bread-crumb)
                        (div (@ (class "inner"))
                             body-forms ...)
                        ,(footer))))))))

(define (bread-crumb)
  (let* ((anchors (cons `(a (@ (href "/")) ,(url-host (request-url)))
                        (map-path (lambda (name path)
                                    `(a (@ (href ,path)) ,name))
                                  (url-path (request-url))))))
    `(div (@ (class "breadcrumb"))
         ,@(intersperse " - " anchors))))

(define (header . title)
  `(head (title ,(if-car title "ykk devel"))
         (link (@ (rel "stylesheet") (type "text/css") (href "/forms.css")))))

(define (footer)
  `(div (@ (class "footer")) "served by ykk"))

;;;; Pages

(http-register-page!
 "/forms.css"
 (lambda ()
   (load-file (url-path (request-url)))))

(http-register-page!
 "/"
 (lambda ()
   (page
     (h3 "YKK Playground")
     (ul
      (li (a (@ (href "/form-test")) "form-test"))
      (li (a (@ (href "/form-plist")) "form-plist"))))))

(http-register-page!
 "/form-test"
 (lambda ()
   (page
     (h4 "forms/test")
     ,(forms/test))))

(define (forms/test)
  (form
   (div (text (@ (class "foo-text")
                 (name "name")
                ;(match "blank email")
                 (default "hello"))))
   (div (text (@ (name "question"))))
   (div (textarea (@ (name "paragraph"))))
   (div (radio (@ (name "test")
                  (default "no"))
               (option (@ (value "yes")) "Yes")
               (option (@ (value "no")) "No")))
   (div (select (@ (name "state")
                   (default "VA"))
                (option "TN")
                (option "VA")))
   (div (checkbox (@ (name "notify")
                     (default "true")
                     (value "notify-on")))
        "Does this work for you?")
   (div (submit (@ (name "Go")
                   (value "Go"))))))


;;;; Util

(define (load-file path)
  (let ((path (string-trim path #\/)))
    (with-exception-catcher
     (lambda (c prop)
       (handle-404))
     (lambda ()
       (page-response
        (with-input-from-file (string-append *root* "/" path)
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

(define (string-substitute s1 s2 str)
  (let loop ((str str))
    (let ((idx (string-contains str s1)))
      (if idx
          (loop (string-append
                 (substring/shared str 0 idx)
                 s2
                 (substring/shared str (+ idx (string-length s1)))))
          str))))


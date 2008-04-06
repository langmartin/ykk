
;;;; Web Server

(define-syntax http-mng
  (syntax-rules ()
    ((_ text body ...)
     (lambda (R)
       (http-server-exec
        (lambda () body ...)
        (let-http-response (220 "ok")
          (let-headers ((content-type "text/plain"))
            (let-content-length
             text))))))))

(define at-repl? #t)

(define (handle-404 R)
  (page-response (404 "Not Found") (string-append "page not found: " (url-path (request-url R)))))

(define (handle-500 R error)
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
 (http-mng "stopping server..." (exit 0)))

(http-register-page!
 "/restart"
 (http-mng "restart server..." (exit 1)))

;;;; Templates

(define *root* "/Users/james/projects/scheme/ykk/forms")

(define-syntax page
  (syntax-rules ()
    ((_ R body-forms ...)
     (page-response
      `(html ,(header)
             (body (div (@ (class "outer"))
                        ,(bread-crumb R)
                        (div (@ (class "inner"))
                             body-forms ...)
                        ,(footer))))))))

(define (bread-crumb R)
  (let* ((anchors (cons `(a (@ (href "/")) ,(url-host (request-url R)))
                        (map-path (lambda (name path)
                                    `(a (@ (href ,path)) ,name))
                                  (url-path (request-url R))))))
    `(div (@ (class "breadcrumb"))
         ,@(intersperse " - " anchors))))

(define (header . title)
  `(head (title ,(if-car title "ykk devel"))
         (link (@ (rel "stylesheet") (type "text/css") (href "/forms.css")))))

(define (footer)
  `(div (@ (class "footer")) "served by ykk"))

;;;; Forms

(define (forms/prototype)
  (form->shtml
   `(form
      (text (@ (class "foo-text")
               (name "name")
               ;(match "blank email")
               (default "hello")))
      (text (@ (name "question")))
      (textarea (@ (name "paragraph")))
      (radio (@ (name "test")
                (default "yes"))
             (option (@ (value "yes")) "Yes")
             (option (@ (value "no")) "No"))
      ;(select (@ (name "state"))
      ;        (option "TN")
      ;        (option "VA"))
      )))

;; Input types

(define (input attrs type name . value)
  (let-optionals value ((value ""))
    `(input (@ (type ,type)
               (name ,name)
               (value ,value)
               ,@attrs))))

(define (text attrs name . default)
  (let-optionals default ((default ""))
    (input attrs "text" name default)))

(define (textarea attrs name . default)
  (let-optionals default ((default ""))
    `(textarea (@ (name ,name)
                  ,@attrs)
               ,default)))

; options is a list of strings or pairs in
; the form of (value text)
(define (radio attrs name options . default)
  (let-optionals default ((default ""))
    `(div (@ ,@attrs)
          ,@(map
             (lambda (o)
               (let ((text (if (pair? o)
                               (cadr o)
                               o))
                     (value (if (pair? o)
                                (car o)
                                o)))
                 `(div ,(input '() "radio" name value)
                       ,text)))
             options))))

(define (sxml/text item)
  (let* ((attrs (sxml-attlist item))
         (name attrs (alist-remove-value 'name attrs))
         (default attrs (alist-remove-value 'default attrs)))
    (text attrs name (or default ""))))

(define (sxml/textarea item)
  (let* ((attrs (sxml-attlist item))
         (name attrs (alist-remove-value 'name attrs))
         (default attrs (alist-remove-value 'default attrs)))
    (textarea attrs name (or default ""))))

(define (sxml/radio item)
  (let* ((attrs (sxml-attlist item))
         (name attrs (alist-remove-value 'name attrs))
         (default attrs (alist-remove-value 'default attrs))
         (sxml/options (sxpath-run '(option) item))
         (options (map (lambda (o)
                         (let* ((text (sxml-first-text o))
                                (value (sxml-attlist-ref "value" o text)))
                           (list value text)))
                       sxml/options)))
    (radio attrs name options default)))

(define (make-transformer f)
  (lambda item
    (f item)))

(define (form->shtml tree)
  (pre-post-order
   tree
   `((text *preorder* . ,(make-transformer sxml/text))
     (textarea *preorder* . ,(make-transformer sxml/textarea))
     (radio *preorder* . ,(make-transformer sxml/radio))
     (*text* . ,(lambda (tag str) str))
     (*default* . ,(lambda x x)))))

;; SXML

(define-condition sxml-transform-error (error) sxml-transform-error?)

(define (sxpath-run expr nodes)
  ((sxpath expr) nodes))

(define (sxpath-first-result query)
  (car query))

(define (sxml-attlist node)
  (sxpath-run `(@ *) node))

(define (sxml-attlist-ref name node . default)
  (let-optionals default ((default 'error))
    (let* ((name (string->symbol name))
           (xp-query (sxpath-run `(@ ,name) node)))
      (if (and (null? xp-query) (eq? default 'error))
          (sxml-transform-error "failed referencing attribute in nodeset" name node)
          (if (null? xp-query)
              default
              (let ((el (cadr (sxpath-first-result xp-query))))
                (if (and (pair? el) (eq? (car el) '*text*))
                    (cadr el)
                    el)))))))

(define (sxml-first-text node)
  (let ((xp-query (sxpath-run `(// *text*) node)))
    (if (null? xp-query)
        #f
        (car xp-query))))

;;;; Pages

(http-register-page!
 "/forms.css"
 (lambda (R)
   (load-file R)))

(http-register-page!
 "/"
 (lambda (R)
   (page R
     (h3 "YKK Playground")
     (ul (li (a (@ (href "/forms")) "forms"))))))

(http-register-page!
 "/forms"
 (lambda (R)
   (page R
     (h3 "Forms")
     (ul (li (a (@ (href "/forms/prototype")) "a prototype"))))))

(http-register-page!
 "/forms/prototype"
 (lambda (R)
   (page R
     (h4 "forms/prototype")
     ,(forms/prototype))))

;;;; Util

(define (alist-remove-value key alist . method)
  (let-optionals method ((ass* assq))
    (let ((entry alist (alist-remove key alist ass*)))
      (values (if (pair? entry) (cadr entry) entry)
              alist))))

(define (alist-remove key alist . method)
  (let-optionals method ((ass* assq))
    (let ((entry (ass* key alist)))
      (values entry
              (reverse
               (fold (lambda (el acc)
                       (if (eq? el entry)
                           acc
                           (cons el acc)))
                     '()
                     alist))))))

(define (load-file R)
  (let ((path (string-trim (url-path (request-url R)) #\/)))
    (with-exception-catcher
     (lambda (c prop)
       (handle-404 R))
     (lambda ()
       (page-response
        (with-input-from-file (string-append *root* "/" (url-path (request-url R)))
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

;;;; "Type Tree"
(define-record-type/primitive :folder
  nongenerative: ua8db299f-732b-4947-ba86-074e4a384bba
  (sticky (type :boolean)))

(define-record-type/primitive :person
  nongenerative: u2a019585-f45a-4ea9-9001-ee1186d9df51
  (name   (type :string))
  (age    (type :number))
  (gender (type :symbol)))

(define-record-type/primitive :place
  nongenerative: ufb76a3cd-903c-413f-86a0-3f36e74513c2
  (address (type :string))
  (city    (type :string))
  (state   (type :symbol))
  (zip     (type :string)))

(define-record-type/primitive :thing
  nongenerative: u5ac8fe2a-0a7a-475f-a336-d3441cc1517d
  (color   (type :string))
  (weight  (type :number))
  (texture (type :symbol)))

(define-syntax ref-table
  (syntax-rules ()
    ((_ item ...)
     (list (cons 'item item) ...))))

(define *allow-types*
  (ref-table :folder :person :place :thing))

(define (lookup-type name)
  (alist-ref *allow-types* name))

(define (type-options)
  (map-in-order (compose symbol->string car) *allow-types*))

(define (type->option type)
  (cond ((find (predicate-eq cdr type) *allow-types*) => car)
        (else #f)))

;;;; Top
(define (persist-once name thunk)
  (if (not (persistent-symbol name))
      (persistent-symbol-set! name (thunk))))

(define (top)
  (persistent-symbol 'top))

(define (scanned-top)
  (scan (top)))

(define (update-scanned-top new-top)
  (persistent-symbol-set!
   'top
   (scanned->source new-top))
  new-top)

(persist-once
 'top
 (lambda ()
   (source:root (source:node :folder
                             (plist (sticky #f))))))

;; (persistent-symbol-set!
;;  'top
;;  (source:root (source:node :folder
;;                            (plist (sticky #f)))))

;;;; Manipulation
(define (zip-up/update-top z)
  (update-scanned-top
   (zip-all-the-way-up z)))

(define (perform op z)
  (zip-up/update-top
   (move 'up (op (z-item z)) z)))

(define (perform/go op z href-proc)
  ;; FIXME: this might as well be a destructive operation at this
  ;; point... how do we use it functionally?
  (perform op z)
  (see-other (href-proc "")))

(define (perform/path op p)
  (perform op (resolve (scanned-top) p)))

(define (insert g child)
  (replace-children
   g
   (add-child child (graph-children g))))

(define (source:update-source node new-source)
  (sharing-record-update node source::node (code new-source)))

(define (update-source g new-source)
  (replace-node
   g
   (source:update-source
    (source:graph-node (scanned->source g))
    new-source)))

;;;; Plist Utilities

;;;; Path Utilities
(define (list->absolute-path p)
  (make-absolute (list->path p)))

(define (list->path p)
  (map-in-order (compose string->symbol obj->path-segment) p))

(define (join-path . segments)
  (string-join (map-in-order obj->path-segment segments) "/"))

(define (obj->path-segment obj)
  (cond ((string? obj)
         (string-trim-right obj #\/))
        ((symbol? obj)
         (obj->path-segment (symbol->string obj)))
        ((number? obj)
         (obj->path-segment (number->string obj 10)))
        ((not obj)
         "")
        (else
         (error 'wrong-type-argument
                "obj->path-segment: expecting string, symbol, number, or #f"
                obj))))

(define (name->string name)
  (cond ((symbol? name) (symbol->string name))
        ((not name) "")
        (else
         (error 'wrong-type-argument
                "name->string: expecting symbol or #f"))))

(assert (list->absolute-path '("foo" "bar")) => '(#f foo bar))
(assert (name->string #f) => "")
(assert (name->string 'foo) => "foo")

;;;; Other Utilities
(define (set-parameters path params)
  (let ((path old-params (parse-url-path path)))
    (add-parameters-to-path
     path
     (merge-alists proj-1 old-params params))))

(define (add-parameters-to-path path params)
  (let-string-output-port
   (output path #\?)
   (for-each (lambda (p)
               (if (pair? p)
                   (begin
                    (display-urlencode (car p))
                    (display #\=)
                    (display-urlencode (cdr p)))
                   (display p)))
    (intersperse #\& params))))

(define (display-urlencode s)
  (with-current-input-port
      (value->input-port s)
    (lambda ()
      (for-each-display (urlencode read-char)))))

(define (value->input-port v)
  (cond ((input-port? v)
         v)
        ((string? v)
         (make-string-input-port v))
        (else
         (make-string-input-port
          (let-string-output-port (display v))))))

(define (output->string . stream)
  (let-string-output-port
   (apply output stream)))

;;;; Testing
(define-syntax* (simulate-request
                 (url: url "http://localhost/")
                 (query: query '())
                 (method: method "get")
                 (headers: headers '())
                 . body)
  (with-request
   (make-request "HTTP/1.1" method (parse-url url) query headers)
   (lambda ()
     (output->string . body))))

;;;; Infrastructure
(define-syntax* (response
                 (status: status 200)
                 (type: content-type "text/html")
                 (headers: headers ())
                 (around-body: around identity)
                 body)
  (expand-response status content-type headers (around body)))

(define-syntax expand-response
  (lambda (form rename compare)
    (let ((status (cadr form))
          (type (caddr form))
          (headers (map-car desyntaxify (cadddr form)))
          (body (cadr (cdddr form))))

      (define %let-http-response (rename 'let-http-response))
      (define %let-headers (rename 'let-headers))
      (define %let-content-vector (rename 'let-content-vector))
      (define %status-code->phrase (rename 'status-code->phrase))

      (define (all-headers)
        (if (assq 'content-type headers)
            headers
            (cons (list 'content-type type)
                  headers)))

      (define (status-line)
        (cond ((pair? status)
               status)
              ((or (number? status) (string? status))
               (list status (status-code->phrase status)))
              (else
               `(,status (,%status-code->phrase ,status)))))

      `(,%let-http-response ,(status-line)
        (,%let-headers ,(all-headers)
         (,%let-content-vector ,body))))))

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
      (lambda () (page . arguments))))))

(define (method-not-allowed . allowed)
  (let ((allowed (string-join (map symbol->string allowed) " ")))
    (reset-page
     status: 405
     headers: ((allowed allowed))
     (method-not-allowed-page (request-method) allowed))))

(define (method-not-allowed-page method allowed)
  (http-error-template
   405
   `(p ,method " is not allowed."
       " Allowed methods are: " ,allowed)))

(define (bad-request . message)
  (apply reset/simple-error-page 400 message))

(define (reset/simple-error-page code . message)
  (reset/error-page
   code
   (apply output->string (intersperse #\space message))))

(define (reset/error-page code explaination)
  (reset-page
   status: code
   (http-error-template code `(p ,explaination))))

(define (error-page code explaination)
  (page
   status: code
   (http-error-template code `(p ,explaination))))

(define (http-error-template code . body)
  (let ((phrase (status-code->phrase code)))
    `(html
      (head
       (title ,phrase))
      (body
       (h1 ,(number->string code 10) ": " ,phrase)
       ,@body))))

;; --------------------
;; Tests

(let ((simulate (lambda (method)
                  (simulate-request
                   method: method
                   (page
                    (method-case
                     ((get) "success")))))))

  (assert (simulate "get") =>
          (output->string
           (page "success")))

  (assert (simulate "post") =>
          (output->string
           (page
            status: 405
            headers: ((allowed "get"))
            (method-not-allowed-page "post" "get")))))

;;;; Resources
(define-syntax define-resource
  (syntax-rules ()
    ((_ path handler)
     (http-register-page! path handler))))

(define-syntax define-resolving-resource
  (syntax-rules ()
    ((_ (path . formals) body ...)
     (define-resource path
       (call-handler/resolved-cursor
        '()
        (lambda formals
          body ...))))))

(define (call-handler/resolved-cursor root handler)
  (let ((root (list->absolute-path root)))
    (lambda path
      (safe-call/resolved
       handle-path-error
       (scanned-top)
       (append root path)
       handler))))

(define (safe-call/resolved handle-error z path success)
  (with-exception-catcher
   (lambda (c prop)
     (if (path-error? c)
         (handle-error c)
         (prop)))
   (lambda ()
     (success (resolve z path) path))))

(define (handle-path-error c)
  (cond ((path-does-not-exist? c)
         (error-page 404 "Path does not exist."))
        (else
         (error-page 500 "Unrecognized path error."))))

(define (path-does-not-exist? c)
  (and (path-error? c)
       (condition-stuff-car-is? c 'path-does-not-exist)))

(define (condition-stuff-car-is? c maybe)
  (eq? (car (condition-stuff c)) maybe))

(define (see-other href)
  (reset-page
   status: 303
   headers: ((location href))
   ""))

(begin
  (assert (safe-call/resolved never? (scanned-top) '(foozle) proj-0) => #f)
  (assert (graph-zipper? (safe-call/resolved never? (scanned-top) '(#f) proj-0)))
  )


;;;;
(define (header/footer-wind thunk)
  (page
   `(html
     (head
      (title "Plist Editor")
      ,(css "/static/reset-fonts-grids-base.css")
      ,(css "/static/plist.css"))
     (body
      (div (@ (id "doc"))
           (div (@ (id "hd"))
             (h1 "Plist Editor"))           
           (div (@ (id "bd")) ,(thunk))
           (div (@ (id "ft"))))))))

(define (css href)
  `(link (@ (rel "stylesheet")
            (type "text/css")
            (href ,href))))

(define (nav-list attributes items)
  `(ul ,(add-class attributes "nav-list")
     ,@(map-in-order make-nav-list-item items)))

(define (make-nav-list-item item)
  (let ((name href (unlist item)))
    `(li (a (@ (href ,href))
           ,name))))

(define (new-item-href href)
  (set-parameters href '((new . true))))

(define (parameterized-request-path)
  (set-parameters (request-path) (get-parameters)))

;; --------------------
;; Data Tree

(define-resolving-resource ("/data" z path)
  (header/footer-wind
   (lambda ()
     (method-case
      ((get)
       (tree->shtml (z-item z) (apply join-path path)))))))

(define (tree->shtml tree path)
  `(ul (@ (class "data-tree"))
       (p (@ (class "item"))
          (a (@ (href ,(plist-editor-href path)))
             ,(plist-title tree)))
       ,(if (leaf? tree)
            `(p (@ (class "leaf")))
            `(div (@ (class "children"))
                  ,@(map-children
                     (lambda (z)
                       (tree->shtml
                        tree
                        (join-path path (graph-name tree))))
                     tree)))))

(define (map-children proc g)
  (fold-children (lambda (c acc)
                   (cons (proc c) acc))
                 (graph-children g)))

(define (plist-editor-href rel)
  (join-path "/plist" rel))

(define (data-tree-href rel)
  (join-path "/data" rel))

;; --------------------
;; Plist Editor

(define-resolving-resource ("/plist" z path)
  (header/footer-wind
   (lambda ()
     (let ((path (apply join-path path)))       
       (method-case
        ((get) (get-plist z))
        ((post) (post-plist z)))))))

(define (get-plist z)
  (let* ((item (z-item z))
         (new type (bind-alist (new type) (get-parameters)))
         (title (plist-title item)))
    (cond (new
           (new-plist-item-form
            title (and type (request-environment-ref type))))
          (else          
           (plist-form
            title (graph-type item) (graph-forms item) '())))))

(define (post-plist path z)
  (let* ((item (z-item z))
         (type (graph-type item))
         (relevant-data (schemify-parameters (remove-parameters 'do-save)))
         (good errors (validate-plist-data type relevant-data)))
    (if (null? errors)
        (perform/go (cut update-source <> `(plist ,@(map-in-order pair->list good)))
                    z data-tree-href)
        (plist-form path (plist-title item) type relevant-data errors))))

(define (string->form s)
  (let-string-input-port s (read)))

(define schemify-parameters
  (cute map-in-order (cut fmap-cdr string->form <>) <>))

(define (request-environment-ref name)
  (with-exception-catcher
   (lambda (c prop)
     (if (unbound-variable-error? c)
         (bad-request "undefined value" name)
         (prop)))
   (lambda ()
     (environment-ref (interaction-environment) name))))

(define (unbound-variable-error? c)
  (and (error? c)
       (string=? "unbound variable" (car (condition-stuff c)))))

(define (validate-plist-data type data)
  (let ((slots (rtd-slots type)))
    (let loop ((data data) (good '()) (errors '()))
      (cond ((null? data)
             (values (reverse good) (reverse errors)))
            ((specification-of slots (caar data)) =>
             (lambda (spec)
               (let ((name value (uncons (car data)))
                     (type (specification-type spec)))
                 (if (scheme-form-conforms-to-type? type value)
                     (loop (cdr data)
                           (cons (car data) good)
                           errors)
                     (loop (cdr data)
                           good
                           (alist-cons name (data-does-not-conform-to-type-error-message type name value)
                                       errors))))))
            (else
             (loop (cdr data)
                   (cons (car data) good)
                   errors))))))

(define (specification-type spec)
  (cond ((get-specification-attribute spec 'type eq?) => attribute-value)
        (else #f)))

(define (data-does-not-conform-to-type-error-message type name value)
  (let-string-output-port
   (output "The value of " name " does not conform to type " (type-name type))))

(define (plist-form title type data errors)
  `(div (@ (id "plist-editor"))
     ,(form->shtml
       `(form (@ (action ,(request-path))
                 (method "post"))
              ,(form-errors->shtml errors)
              (fieldset
              (legend "Editing: " ,title)
              (ul (@ (class "plist-items"))
                  ,@(map (render-plist-entry type) data)))
              (div (@ (class "buttons"))
                   ,(button-anchor `(@ (href ,(new-item-href (parameterized-request-path))))
                       "New Item")                   
                   (submit (@ (name "do-save")
                              (value "Save")))
                   (submit (@ (name "_method")
                              (value "Delete"))))))))

(define (new-plist-item-form title type)
  `(div (@ (id "new-item-form"))
     ,(form->shtml
       `(form (@ (action ,(request-path))
                 (method "get"))
          ,@(all-parameters->hidden-inputs)
          (fieldset
            (legend "New Item In: " ,title)
            (ul (@ (class "new-item-inputs"))
              ,(apply list-field
                      maybe-select
                      `(@ (label "Type")
                          (default ,(type->option type)))
                      (type-options))))))))

(define (list-field make-field attributes . rest)
  (let* ((label name rest (pluck-attributes (label name) attributes))
         (label (or label (string->label name)))
         (name (or name (string->name label))))
    `(li (@ (class ,(join-classes "field" (string->identifier label))))
       (label (@ (for ,name)) ,label)
       ,(apply make-field
               (update-attributes rest `((name ,name)))
               rest))))

(define (maybe-select attributes . options)
  (let ((submitted default (bind-attributes (submitted default) attributes)))
    (if )))

(define (all-parameters->hidden-inputs)
  (reverse
   (fold parameter->hidden-input
         (fold parameter->hidden-input
               '()
               (request-parameters))
         (get-parameters))))

(define (parameter->hidden-input param inputs)
  (cons `(input (@ (type "hidden")
                   (name ,(maybe-symbol->string (car param)))
                   (values ,(cdr param))))
        inputs))

(define (maybe-symbol->string obj)
  (cond ((string? obj)
         obj)
        ((symbol? obj)
         (symbol->string obj))
        (else
         (error 'wrong-type-argument
                "maybe-symbol->string: expecting string or symbol"
                obj))))

(define (button-anchor attributes name)
  (let ((href attributes (pluck-spec (href) (cdr attributes))))    
    `(input ,(update-attributes
              `(@ (value ,name)
                  (type "button")
                  (onclick ,(string-append "document.location.href='" href "';")))
              attributes))))

(define (form-errors->shtml errors)
  (if (null? errors)
      ""
      `(ul (@ (class "form-errors"))
          ,@(map-in-order
             (lambda (e)`(li ,(cdr e)))
             errors))))

(define-generic plist-title &plist-title (item))

(define-method &plist-title ((item :graph))
  (if (root? item)
      "/"
      (name->string (graph-name item))))

(define-method &plist-title ((item :graph-zipper))
  (plist-title (z-item item)))

(define (render-plist-entry type)
  (lambda (entry)
    (let ((name value (uncons entry)))
      `(li (@ (class ,(classify-plist-entry name value)))
         ,(label-plist-entry name value)
         ,(render-plist-input name value)))))

(define (classify-plist-entry name value)
  "simple")

(define (label-plist-entry name value)
  (let ((string-name (symbol->string name)))
    `(label (@ (for ,string-name))
       ,string-name ": ")))

(define (render-plist-input name value)
  (let ((string-name (symbol->string name)))
    `(text (@ (name ,string-name)
              (default ,(plist-value->attribute-value value))))))

(define (plist-value->attribute-value v)
  (concat v))

(define (choose-parameters names)
  (choose-keys names (request-parameters)))

(define (remove-parameters names)
  (remove-keys names (request-parameters)))
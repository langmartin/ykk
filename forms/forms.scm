
;; High level interface

(define-syntax form
  (syntax-rules ()
    ((_ expr ...)
     (form->shtml
      '(form expr ...)))))

;; Input types

(define (input attrs (type :string) (name :string) . value)
  (let-optionals value ((value #f))
    `(input (@ (type ,type)
               (name ,name)
               (value ,(or value ""))
               ,@attrs))))

(define (text attrs (name :string) . default)
  (let-optionals default ((default #f))
    (input attrs "text" name default)))

(define (textarea attrs (name :string) . default)
  (let-optionals default ((default #f))
    `(textarea (@ (name ,name)
                  ,@attrs)
               ,(or default ""))))

;; options is a list of strings or pairs in
;; the form of (value text)
(define (map-options f lst)
  (map (lambda (o)
         (call-with-values
           (lambda ()
             (if (pair? o)
                 (values (car o) (cdr o))
                 (values o o)))
           f))
       lst))

(define (radio attrs (name :string) (options :pair) . default)
  (let-optionals default ((default #f))
    `(div (@ ,@attrs)
          ,@(map-options
             (lambda (value text)
               `(div ,(input (if (equal? default value)
                                 '((checked "checked"))
                                 '())
                             "radio" name value)
                     ,text))
             options))))

(define (select attrs (name :string) (options :pair) . default)
  (let-optionals default ((default #f))
    `(select (@ (name ,name)
                ,@attrs)
             ,@(map-options
                (lambda (value text)
                  (let* ((attr `((value ,value))))
                    `(option (@ ,@(if (equal? default value)
                                      (cons '(selected "true") attr)
                                      attr))
                             ,text)))
                options))))

(define (checkbox attrs (name :string) (value :string) . default)
  (let-optionals default ((default #f))
    (input (if (and default (not (eq? default "false")))
               (cons '(checked "checked") attrs)
               attrs)
           "checkbox" name value)))

(define (submit attrs (name :string) (value :string))
  (input attrs "submit" name value))


;; SXML -> SHTML

(define (grab-options item)
  (let ((sxml/options (sxpath-run '(option) item)))
    (map (lambda (o)
           (let* ((text (sxml-first-text o))
                  (value (bind-spec (value) (sxml-attlist o))))
             (cons (or value text) text)))
         sxml/options)))

(define (sxml-text item)
  (let-sxml-pluck-attlist item (attrs name default)
    (text attrs name default)))

(define (sxml-textarea item)
  (let-sxml-pluck-attlist item (attrs name default)
    (textarea attrs name default)))

(define (sxml-radio item)
  (let-sxml-pluck-attlist item (attrs name default)
    (radio attrs name (grab-options item) default)))

(define (sxml-select item)
  (let-sxml-pluck-attlist item (attrs name default)
    (select attrs name (grab-options item) default)))

(define (sxml-checkbox item)
  (let-sxml-pluck-attlist item (attrs name default value)
    (checkbox attrs name value default)))

(define (sxml-submit item)
  (let-sxml-pluck-attlist item (attrs name value)
    (submit attrs name value)))

(define (make-transformer f)
  (lambda item
    (f item)))

(define (form->shtml tree)
  (pre-post-order
   tree
   `((text . ,(make-transformer sxml-text))
     (textarea . ,(make-transformer sxml-textarea))
     (radio . ,(make-transformer sxml-radio))
     (checkbox . ,(make-transformer sxml-checkbox))
     (select . ,(make-transformer sxml-select))
     (submit . ,(make-transformer sxml-submit))
     (*text* . ,(lambda (tag str) str))
     (*default* . ,(lambda x x)))))


;; Tests

(define (run-tests . giddy)
  (let-optionals giddy ((giddy? #f))
    (assert (sxml-text '(text (@ (class "teh") (default "junk")
                                 (name "foo"))))
            => '(input (@ (type "text") (name "foo")
                          (value "junk") (class "teh"))))

    (assert (sxml-textarea '(textarea (@ (class "teh") (default "junk")
                                         (name "foo"))))
            => '(textarea (@ (name "foo") (class "teh")) "junk"))
    
    (assert (sxml-radio '(radio (@ (class "teh") (default "bar")
                                   (name "foo"))
                                (option "bar")
                                (option (@ (value "Baz")) "baz")))
            => '(div (@ (class "teh"))
                     (div (input (@ (type "radio") (name "foo")
                                    (value "bar") (checked "checked"))) "bar")
                     (div (input (@ (type "radio") (name "foo")
                                    (value "Baz"))) "baz")))

    (assert (sxml-select '(select (@ (class "teh") (default "bar")
                                     (name "foo"))
                                  (option "bar")
                                  (option (@ (value "Baz")) "baz")))
            => '(select (@ (name "foo") (class "teh"))
                        (option (@ (selected "true") (value "bar")) "bar")
                        (option (@ (value "Baz")) "baz")))

    (assert (sxml-checkbox '(checkbox (@ (class "teh") (name "foo")
                                         (value "bar") (default "true"))))
            => '(input (@ (type "checkbox") (name "foo")
                          (value "bar") (checked "checked")
                          (class "teh"))))

    (assert (sxml-submit '(submit (@ (class "submit") (value "Go")
                                     (name "submit"))))
            => '(input (@ (type "submit") (name "submit")
                          (value "Go") (class "submit"))))

    (assert (form
             (div (text (@ (class "foo-text")
                           (name "name")
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
                             (value "Go")))))
            => '(form
                 (div (input (@ (type "text") (name "name")
                                (value "hello") (class "foo-text"))))
                 (div (input (@ (type "text") (name "question")
                                (value ""))))
                 (div (textarea (@ (name "paragraph")) ""))
                 (div (div (@)
                           (div (input (@ (type "radio") (name "test")
                                          (value "yes")))
                                "Yes")
                           (div (input (@ (type "radio") (name "test")
                                          (value "no") (checked "checked")))
                                "No")))
                 (div (select (@ (name "state"))
                              (option (@ (value "TN")) "TN")
                              (option (@ (selected "true") (value "VA")) "VA")))
                 (div (input (@ (type "checkbox") (name "notify")
                                (value "notify-on") (checked "checked")))
                      "Does this work for you?")
                 (div (input (@ (type "submit") (name "Go")
                                (value "Go"))))))
    (if giddy? (display "Yay!\n"))))

(run-tests)

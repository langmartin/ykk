;;;;
;;;; Forms
;;;;

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

; options is a list of strings or pairs in
; the form of (value text)
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

(define (checkbox attrs (name :string) . value/default)
  (let-optionals value/default ((value #f) (default #f))
    (input (if (and default (not (eq? default "false")))
               (cons '(checked "checked") attrs)
               attrs)
           "checkbox" name (or value "true"))))

(define (submit attrs (name :string) (value :string))
  (input attrs "submit" name value))


;; Input SXML Tags

(define-condition form-error (error) form-error?)

(define *inputs* '())

(define (register-input! name transformer)
  (set! *inputs* (cons (cons name transformer)
                            *inputs*)))

(define (input-transformers)
  *inputs*)

(define (input-transformer type)
  (alist-ref *inputs* type))

(define (input-names)
  (map car *inputs*))

(define (input? form)
  (and (pair? form)
       (memq (car form) (input-names))
       #t))

;; todo - combine input functions with tag definitions
(define-syntax define-input-tag
  (syntax-rules ()
    ((_ (name item attrs keys ...) arglist)
     (register-input!
      'name
      (lambda item
        (let-sxml-pluck-attrs item (attrs validate keys ...)
          (apply name arglist)))))))

(define (grab-options item)
  (let ((sxml/options (sxpath-run '(option) item)))
    (map (lambda (o)
           (let* ((text (sxml:text o))
                  (value (or (sxml:attr o 'value)
                             text)))
             (cons value text)))
         sxml/options)))

(define-input-tag (text item attrs name default)
  (list attrs name default))

(define-input-tag (textarea item attrs name default)
  (list attrs name default))

(define-input-tag (radio item attrs name default)
  (list attrs name (grab-options item) default))

(define-input-tag (select item attrs name default)
  (list attrs name (grab-options item) default))

(define-input-tag (checkbox item attrs name value default)
  (list attrs name value default))

(define-input-tag (submit item attrs name value)
  (list attrs name value))

;; The Transformation

(define (form->shtml tree)
  (pre-post-order
   tree
   `(,@(input-transformers)
     (*text* . ,(lambda (tag str) str))
     (*default* . ,(lambda x x)))))

;; Validation, processing, etc.

(define (replace-nodes form pred)
  (replace-range pred
                 (lambda (n) (list n))
                 form))

(define (select-input form name)
  (let ((selection (txpath-run
                    (string-append "descendant-or-self::*[@name='" name "']")
                    form)))
    (if (null? selection)
        (form-error "No input found by the name of" name form)
        (car selection))))

(define (select-inputs form)
  (apply append
         (map (lambda (name)
                (txpath-run
                 (string-append "descendant-or-self::" (symbol->string name))
                 form))
              (input-names))))

(define (modify-input form name proc)
  (modify-inputs
   form
   (lambda (n)
     (and (equal? (sxml:attr name 'name) name)
          (proc n)))))

(define (modify-inputs form proc)
  (replace-nodes
   form
   (lambda (n)
     (and-let* ((input? n)
                (result (proc n)))
       result))))

(define-fluid
  (form-data-fluid '())
  form-data
  with-form-data
  let-form-data)

(define (maybe-string thing)
  (if (symbol? thing)
      (symbol->string thing)
      thing))

(define (raw-populate form)
  (fold (lambda (piece form)
          (let* ((name (maybe-string (car piece)))
                 (val (cdr piece)))
            (modify-inputs
             form
             (lambda (n)
               (and (equal? (sxml:attr n 'name) name)
                    (list (or (sxml:add-attr n `(submitted ,val))
                              n)))))))
        form
        (form-data)))

(define (populate form)
  (let ((form (raw-populate form)))
    (modify-inputs
     form
     (lambda (n)
       (and (eq? (sxml:node-name n) 'checkbox)
            (not (sxml:attr n 'submitted))
            (list (sxml:add-attr n `(submitted "false"))))))))

(define (unpopulate form)
  (map (lambda (n)
         (cons (sxml:attr n 'name)
               (sxml:attr n 'submitted)))
       (txpath-run "descendant-or-self::*[@submitted]" form)))

(define-syntax validate/combine
  (syntax-rules ()
    ((_ comb validators)
     (fold (lambda (v acc)
          (lambda (name value)
            (comb (v name value)
                  (acc name value))))
        (lambda (name value) #f)
        validators))))

(define (validate/and-combine . validators)
  (validate/combine or validators))

(define (validate/or-combine . validators)
  (validate/combine and validators))

(define (validator-environment-ref name)
 (with-exception-catcher
  (lambda (c prop)
    (if (unbound-variable-error? c)
        (form-error "undefined validator" name)
        (prop)))
  (lambda ()
    (environment-ref (interaction-environment) (if (string? name)
                                                   (string->symbol name)
                                                   name)))))
(define (unbound-variable-error? c)
 (and (error? c)
      (string=? "unbound variable" (car (condition-stuff c)))))

(define (string->compound-validator str)
  (let ((validators (string-tokenize str)))
    (apply validate/and-combine (map (lambda (s)
                                      (validator-environment-ref (string-append "validate-" s)))
                                    validators))))

(define (maybe-compound-validator thing)
  (if (string? thing)
      (string->compound-validator thing)
      thing))

(define (validate form)
  (modify-inputs
   form
   (lambda (n)
     (and-let* ((input-validate (maybe-compound-validator (sxml:attr n 'validate)))
                (name (sxml:attr n 'name))
                (value (sxml:attr n 'submitted))
                (result (input-validate name value)))
       (list (or (sxml:add-attr n `(error ,result))
                 n))))))

(define (good-form? form)
  (null? (form-errors form)))

(define (form-errors form)
  (map (lambda (n)
         (sxml:attr n 'error))
       (txpath-run "descendant-or-self::*[@error]" form)))

;; Primitive Validators

(define (validate-required name value)
  (and (not (string-null? value))
       (string-append "`" name "' is required")))

(define (validate-email name value)
  (and (not (rx:exact-match? (rx:sequence
                              (rx:repeat rx:graphic)
                              (rx:text "@") (rx:repeat rx:graphic)
                              (rx:text ".") (rx:repeat rx:graphic))
                             value))
       (string-append "`" name "' must be an email")))


;; Tests
;;

(define (validate-question name value)
  (and (not (equal? value "How are you?"))
       (string-append "`" name "' must be the right question")))

(define form `(form
               (div (text (@ (class "foo-text")
                             (name "name")
                             (default "hello")
                             (validate "email"))))
               (div (text (@ (name "question")
                             (validate ,validate-question))))
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

(define form-w/data (let-form-data '((name . "James") (question . "How are you?"))
                      (populate form)))

(define form-validated (validate form-w/data))

(assert (form->shtml '(text (@ (class "teh") (default "junk")
                               (name "foo"))))
        =>
        '(input (@ (type "text") (name "foo")
                   (value "junk") (class "teh"))))

(assert (form->shtml '(textarea (@ (class "teh") (default "junk")
                                   (name "foo"))))
        =>
        '(textarea (@ (name "foo") (class "teh")) "junk"))

(assert (form->shtml '(radio (@ (class "teh") (default "bar")
                                (name "foo"))
                             (option "bar")
                             (option (@ (value "Baz")) "baz")))
        =>
        '(div (@ (class "teh"))
              (div (input (@ (type "radio") (name "foo")
                             (value "bar") (checked "checked"))) "bar")
              (div (input (@ (type "radio") (name "foo")
                             (value "Baz"))) "baz")))

(assert (form->shtml '(select (@ (class "teh") (default "bar")
                                 (name "foo"))
                              (option "bar")
                              (option (@ (value "Baz")) "baz")))
        =>
        '(select (@ (name "foo") (class "teh"))
                 (option (@ (selected "true") (value "bar")) "bar")
                 (option (@ (value "Baz")) "baz")))

(assert (form->shtml '(checkbox (@ (class "teh") (name "foo")
                                   (value "bar") (default "true"))))
        =>
        '(input (@ (type "checkbox") (name "foo")
                   (value "bar") (checked "checked")
                   (class "teh"))))

(assert (form->shtml '(submit (@ (class "submit") (value "Go")
                                 (name "submit"))))
        =>
        '(input (@ (type "submit") (name "submit")
                   (value "Go") (class "submit"))))


(assert (form->shtml form)
        =>
        '(form
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

(assert form-validated
        =>
        `(form
          (div (text (@ (error "`name' must be an email")
                        (submitted "James") (class "foo-text")
                        (name "name") (default "hello")
                        (validate "email"))))
          (div (text (@ (submitted "How are you?") (name "question")
                        (validate ,validate-question))))
          (div (textarea (@ (name "paragraph"))))
          (div (radio (@ (name "test") (default "no"))
                      (option (@ (value "yes")) "Yes")
                      (option (@ (value "no")) "No")))
          (div (select (@ (name "state") (default "VA"))
                       (option "TN")
                       (option "VA")))
          (div (checkbox (@ (submitted "false") (name "notify")
                            (default "true") (value "notify-on")))
               "Does this work for you?")
          (div (submit (@ (name "Go") (value "Go"))))))

(assert (good-form? form-validated) => #f)


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

;;;; Manipulation
(define (zip-up/update-top z)
  (update-scanned-top
   (zip-all-the-way-up z)))

(define (perform z op)
  (zip-up/update-top
   (move 'up (op (z-item z)) z)))

(define (perform/path p op)
  (perform (resolve (scanned-top) p) op))

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

(perform/path
 "/"
 (cut update-source <>
      `(plist (sticky ,#t))))

;; (perform/path
;;  "/"
;;  (cut insert <>
;;       (edge 'first-person
;;             (node :person (plist (name "Lang")
;;                                  (age 31)
;;                                  (gender 'male))))))

;; (map->list graph-name (scanned-top))

;; (graph-name (z-item (resolve (scanned-top) "/first-person")))

(define-syntax insert-test
  (syntax-rules ()
    ((_ tech-name ?name ?age ?gender)
     (perform/path
      "/"
      (cut insert <>
           (edge tech-name
                 (node :person (plist (name ?name)
                                      (age ?age)
                                      (gender ?gender)))))))))


(define (source:update-source node new-source)
  (sharing-record-update node source::node (code new-source)))

(define (update-source g new-source)
  (replace-node
   g
   (source:update-source
    (source:graph-node (scanned->source g))
    new-source)))

#;
(perform/path
 "/james-long"
 (cut update-source <>
      `(plist (name "billy") (age 25) (gender 'poop))))

#;
(map->list graph-name (scanned-top))



(define (plist-editor path)
  (let* ((path (cons #f (map string->symbol path)))
         (z (resolve (scanned-top) path))
         (node (z-item z))
         (type (graph-type node))
         (slots (description-specifications (rtd-slots type)))
         (values (graph-forms node))
         (req (request-parameters)))
    (if (string-ci= (request-method) "post")
        (let ((name age gender tail (bind-alist (name age gender) req)))
          (perform z
                   (cut update-source <> `(plist (name ,name)
                                                 (age ,age)
                                                 (gender ,gender))))
          `(ul (li "name " ,name)
               (li "age " ,age)
               (li "gender " ,gender)))
        (form->shtml
         `(form (@ (action ,(request-path))
                   (method "post"))
            (ul
             ,(map (lambda (spec)
                     (let ((type (cadr (assq 'type (cdr spec))))
                           (field-name (symbol->string (car spec)))
                           (field-value (concat (cdr (assq (car spec) values)))))
                       `(li ,field-name
                            ": "
                            (text (@ (name ,field-name)
                                     (default ,field-value)))
                            )))
                   slots)
             (submit (@ (name "Name")
                        (value "Go")))))))))

;(insert-test 'james-long "James" 23 'male)

(http-register-page!
 "/forms-plist"
 (lambda path
   (page
     (h4 "plist editor")
     ,(plist-editor path))))

(http-register-page!
 "/static"
 (lambda path
   (p 'called)
   (page-response    
    (200 "OK") "text/plain"
    (h4 "hello")))
 )

,open thread threads-internal

(define *server* #f)

(define (start!)
  (set! *server* (spawn form-server)))

(define (terminate!)
  (if *server*
      (begin
        (terminate-thread! *server*)
        (set! *server* #f)
        'terminated)
      'no-server-running))

(terminate!)
(start!)

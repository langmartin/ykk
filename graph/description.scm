;;; Construction
(define (description-tag)
  description-tag)

(define (make-description lst)
  (record description-tag lst))

(define (description? foo)
  (and (record? foo)
       (eq? description-tag (record-ref foo 0))))

(define (list->description lst)
  (make-description
   (unfold-list->description-source lst)))

(define (make-specification label spec)
  (if (or (null? spec)
          (pair? spec))
      (cons (remove-keyword-indication label)
            spec)
      (error 'malformed-specification
             `(,label ,spec))))

(define (specification? foo)
  (and (pair? foo)
       (symbol? (car foo))
       (or (null? (cdr foo))
           (pair? (cadr foo)))))

(define (list->specification source)
  (make-specification
   (remove-keyword-indication (car source))
   (unfold-list->specification-source (cdr source))))

(define (attribute? foo)
  (and (pair? foo)
       (symbol? (car foo))
       (not (null? (cdr foo)))
       (null? (cddr foo))))

(define (make-attribute label value)
  (list label value))

(define (make-attribute-from-source label value)
  (make-attribute (remove-keyword-indication label) value))

;; --------------------
;; Parsing

(define (unfold-list->description-source source)
  (reverse
   (unfold-list->alist
    take-specification
    make-description-specification
    '()
    source)))

(define (take-specification source)
  (cond
   ;; ((LABEL VALUE ...) REST ...)
   ((pair? (car source))
    (values (caar source) (cdar source) (cdr source)))
   ;; (NAME REST ...)
   ((not (keyword? (car source)))
    (values (car source) '() (cdr source)))
   ;; (LABEL)
   ((null? (cdr source))
    (values #f #f source))
   ;; (KEY: VALUE REST ...)
   (else
    (values (car source) (cadr source) (cddr source)))))

(define (make-description-specification label lst)
  (make-specification label (unfold-list->specification-source lst)))

(define (unfold-list->specification-source source)
  (reverse
   (unfold-list->alist
    take-attribute
    make-attribute-from-source
    '()
    source)))

(define (take-attribute source)
  (cond ((keyword? (car source))
         ;; (KEY: VALUE REST ...) | (KEY:) | (KEY1: KEY2: REST ...)
         (if (or (null? (cdr source))
                 (keyword? (cadr source)))
             (values (car source) #t (cdr source))
             (values (car source) (cadr source) (cddr source))))
        ((pair? (car source))
         ;; ((LABEL VALUE) REST ...)
         (if (or (not (pair? (cdar source)))
                 (null? (cdar source))
                 (not (null? (cddar source))))
             (values #f #f source)
             (values (caar source) (cadar source) (cdr source))))
        (else
         (values #f #f source))))

;; --------------------
;; Scheme48

(define-simple-type :description (:record) description?)

(define-method &disclose ((d :description))
  `(description ,@(specification-names d)))

(define (specification-names d)
  (map-in-order car (description-specifications d)))

;;;; Referencing
(define (description-specifications (d :description))
  (record-ref d 1))

(define (description-length d)
  (length (description-specifications d)))

(define (description-specification-index d name)
  (alist-key-index eq? name (description-specifications d)))

(define (specification-of d name)
  (assq name (description-specifications d)))

(define specification-name car)

(define specification-attributes cdr)

(define (specification-length spec)
  (length (specification-attributes spec)))

(define (get-specification-attribute spec name compare)
  (find (lambda (attr)
          (compare (attribute-name attr)
                   name))
        (specification-attributes spec)))

(define attribute-name car)

(define attribute-value cadr)

;;;; Utilities
(define (descriptions-equal? a b)
  (equal? (description-specifications a)
          (description-specifications b)))

(define (map-specification-attributes proc spec)
  (make-specification
   (specification-name spec)
   (map-in-order
    (lambda (a)
      (make-attribute
       (attribute-name a)
       (proc (attribute-name a) (attribute-value a))))
    (specification-attributes spec))))

(define (project-description description . rest)
  (apply project-specifications (description-specifications description) rest))

(define (project-specifications specs guard project . project-args)
  (reverse
   (fold (lambda (spec acc)
           (cond ((apply project spec project-args) guard
                  => (cut cons <> acc))                 
                 (else
                  acc)))
         '()
         specs)))

(define (project-specification spec attribute-names)
  (cons (specification-name spec)
        (project-alist-onto attribute-value
                            (lambda x #f)
                            attribute-names
                            (specification-attributes spec))))

(define (descriptions-consistent? specs-consistent? d1 d2)
  (let ((spec1 (description-specifications d1))
        (spec2 (description-specifications d2)))
    (if (not (= (specification-length spec1)
                (specification-length spec2)))
        (invalid 'inconsitent-description-lengths
                 d1 d2)
        (fold (lambda (s1 s2 valid?)
                (and (or (and (eq? (specification-name s1)
                                   (specification-name s2))
                              (specs-consistent? s1 s2))
                         (invalid 'inconsistent-specification
                                  `(,s1 in ,d1)
                                  `(,s2 in ,d2)))
                     valid?))
              #t
              spec1
              spec2))))

(define (invalid . args)
  (apply warn args)
  #f)

(define (combine-descriptions-by-name template default . descriptions)
  (make-description
   (apply merge-alists
          (cons (mediate-combination template default)
                (map-in-order description-specifications descriptions)))))

(define (mediate-combination template default)
  (lambda (orig update)
    (make-specification
     (specification-name orig)
     (merge-attributes/template
      template
      default
      (specification-attributes orig)
      (specification-attributes update)))))

(define (merge-attributes/template template default attr-list1 attr-list2)
  (apply merge-alists/template
         (list make-attribute attribute-value template default attr-list1 attr-list2)))

;;;; Tests
(begin
  (assert (list->specification '(foo a: 1 b: 2 (c 3)))
          => '(foo (a 1) (b 2) (c 3)))

  (let ((d1 (list->description `(a b: (c: 1 d: 2) (e (f 1) (g 2)))))
        (d2 (list->description `((a) (b (c 1) (d 2)) (e f: 1 g: 2)))))    

    (assert (description-specifications d1)
            => '((a) (b (c 1) (d 2)) (e (f 1) (g 2))))

    (assert (description-specifications d2)
            => '((a) (b (c 1) (d 2)) (e (f 1) (g 2))))

    (assert (description-length d1) => 3)

    (assert (description-specification-index d1 'b) => 1)    
    
    (assert (descriptions-consistent? (lambda x #t) d1 d2))
    
    (assert (descriptions-equal? d1 d2))

    (assert (project-description
             d1
             (lambda (item) 
               (let ((name c f (unlist item)))
                 (or c f)))
             project-specification
             '(c f))            
            => '((b 1 #f) (e #f 1))))  

  (let ((d1 (list->description '((a foo: 1 bar: 2) (b foo: 1 baz: 2 quux: 5))))
        (d2 (list->description '((b foo: 10 bar: 30 quux: 4) (c foo: 11))))
        (choose-greater (lambda (a b) (if (> a b) a b)))
        (choose-even (lambda (a b) (if (even? a) a b))))
    (assert
     (description-specifications
      (combine-descriptions-by-name
       `((quux ,choose-even))
       choose-greater
       d1 d2))
     => '((a (foo 1) (bar 2))
          (b (foo 10) (baz 2) (quux 4) (bar 30))
          (c (foo 11)))))
  )
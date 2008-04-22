(define-syntax type-description
  (syntax-rules ()
    ((_ source)
     (syntax/normalize-description
      source
      (syntax/make-description)))
    ((_ source template)
     (syntax/normalize-description
      source
      (syntax/update-attribute-values
       template
       (syntax/make-description))))))

;;;; Accessors
(define-syntax define-specification-attribute-accessors
  (syntax-rules ()
    ((_ (name attribute default) ...)
     (begin
       (define (name spec)
         (cond ((assq attribute (specification-attributes spec))
                => attribute-value)
               (else default)))
       ...))))

(define-specification-attribute-accessors
  (specification-type     'type     :value))

;;;; Normalization
(define (default-description d specification-defaults)
  (make-description
   (default-specifications
    (description-specifications d)
    specification-defaults)))

(define (default-specifications specs defaults)  
  (map-in-order (lambda (spec)
                  (make-specification
                   (specification-name spec)
                   (merge-alists
                    proj-0
                    (specification-attributes spec)
                    defaults)))                
                specs))

;;;; Consistency
(define (invalid . args)
  (apply warn args)
  #f)

(define (values-consistent/description? d check-values)
  (let ((specs (description-specifications d)))
    (if (not (= (length specs) (length check-values)))
        (invalid 'wrong-number-of-values
                 `(expecting: ,@specs)
                 `(values: ,@check-values))        
        (fold (lambda (spec value valid?)
                (and (or (value-consistent/specification? spec value)
                         (invalid 'inconsistent-value
                                  spec
                                  value))                     
                     valid?))
              #t
              specs
              check-values))))

(define (value-consistent/specification? spec value)
  ((type-predicate (specification-type spec)) value))

;;;; Combination
(define (combine-descriptions . descriptions)
  (apply combine-descriptions-by-name
         (append (list `((type ,choose-more-specific-type))
                       proj-1)
                 descriptions)))

(define (choose-more-specific-type a b)
  (if (same-type? a b)
      a
      (let ((less more (if (more-specific-type? a b) (values b a) (values a b))))
        (if (strict-subtype? more less)
            more
            (begin
              (warn 'incompatible-type-specifications
                    `(,more is-not-a-subtype-of ,less))
              a)))))

;;;; Tests
(begin
  (let ((d1 (type-description (a (b foo: 1 type: :number))))        
        (d2 (type-description ((a type: :symbol) (b type: :complex initform: #f) c))))    
    (assert (description-specifications (default-description d1 `((type ,:value))))
            => `((a (type ,:value))                 
                 (b (foo 1)
                    (type ,:number))))    
    
    (let ((spec (car (description-specifications d1))))
      (assert (specification-name spec) => 'a)
      (assert (specification-type spec) => :value))    

    (assert (values-consistent/description? d1 (list 'foo 12)))

    (assert (description-specifications (combine-descriptions d1 d2))
            => `((a (type ,:symbol))
                 (b (foo 1)
                    (type ,:complex)
                    (initform #f))
                 (c)))
    
    (let-syntax ((suppress-warnings
                  (syntax-rules ()
                    ((_ body ...)
                     (with-handler
                      (lambda (c propagate)
                        (if (not (warning? c))
                            (propagate c)))
                      (lambda () body ...))))))
      
      (suppress-warnings
       (assert (not (values-consistent/description? d1 (list 'foo 'bar)))))

      (suppress-warnings
       (assert (description-specifications
                (combine-descriptions d1 (type-description ((b type: :pair))))) 
               => (description-specifications d1))))))
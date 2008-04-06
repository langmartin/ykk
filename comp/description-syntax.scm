(define-syntax syntax/normalize-description
  (lambda (form rename compare)
    (let ((source (cadr form))
          (k (caddr form)))
      (continue (unfold-list->description-source source) k))))

(define-syntax syntax/normalize-specification
  (lambda (form rename compare)
    (let ((source (cadr form))
          (k (caddr form)))
      (continue (unfold-list->specification-source source) k))))

(define-syntax syntax/update-attribute-values
  (lambda (form rename compare)
    (let ((description (cadr form))
          (template (map-car desyntaxify (caddr form)))  ;++
          (k (cadddr form)))

      (define (maybe-template label value)
        (cond ((assq label template)
               => (lambda (found)
                    (continue value (cadr found))))
              (else value)))

      (define (apply-template spec)
        (map-specification-attributes maybe-template spec))

      (continue (map-in-order apply-template description) k))))

(define-syntax syntax/quote-non-literal
  (lambda (form rename compare)
    (quote-non-literal (cadr form))))

(define-syntax syntax/make-description
  (syntax-rules ()
    ((_ (spec ...))
     (make-description
      (list (quote-specification-labels spec) ...)))))

(define-syntax quote-specification-labels
  (syntax-rules ()
    ((_ (label))
     (list 'label))
    ((_ (label (attribute value) ...))
     (list 'label (list 'attribute value) ...))))

(begin
  (assert (syntax/quote-non-literal a) => 'a)
  (assert (syntax/quote-non-literal 'a) => 'a)
  (assert (syntax/quote-non-literal 1) => 1)
  (assert (syntax/quote-non-literal (a b)) => '(a b))

  (assert
   (description-specifications
    (syntax/normalize-description
     (a b: (c: 1 d: 'foo) (e (f '(a b c)) (g cdr)))
     (syntax/make-description)))
   => `((a) (b (c 1) (d foo)) (e (f (a b c)) (g ,cdr)))

   (assert
    (description-specifications
     (syntax/update-attribute-values
      ((a) (b (c 1) (f 'foo)) (e (f (a b c)) (g cdr)))
      ((f (syntax/quote-non-literal)))
      (syntax/make-description)))
    => `((a) (b (c 1) (f foo)) (e (f (a b c)) (g ,cdr)))))
  )

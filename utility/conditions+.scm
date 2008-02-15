(define-syntax define-condition
  (syntax-rules ()
    ((_ name supers pred?)
     (begin
       (define-condition-type 'name 'supers)
       (define pred? (condition-predicate 'name))
       (define (name . args)
         (apply signal (cons 'name args)))))))

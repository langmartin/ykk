(define-syntax define-condition
  (syntax-rules ()
    ((_ name supers pred?)
     (begin
       (define-condition-type 'name 'supers)
       (define pred? (condition-predicate 'name))
       (define (name . args)
         (apply signal (cons 'name args)))))))

(define-fluid ($condition #f)
  really-raise
  with-condition
  let-condition)

(define (raise-condition . args)
  (apply (really-raise) args))
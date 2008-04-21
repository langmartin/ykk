(define (with-exception-catcher catcher thunk)
  ((call-with-current-continuation
    (lambda (k)
      (lambda ()
        (with-handler
         (lambda (c propagate)
           (k (lambda () (catcher c propagate))))
         thunk))))))

(define-syntax define-condition
  (syntax-rules ()
    ((_ name supers pred?)
     (begin
       (define-condition-type 'name 'supers)
       (define pred? (condition-predicate 'name))
       (define (name . args)
         (apply signal (cons 'name args)))))))

(define-fluid ($condition error)
  really-raise
  with-condition
  let-condition)

(define (raise-condition . args)
  (apply (really-raise) args))
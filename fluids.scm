(define-syntax define-fluid
  (syntax-rules ()
    ((_ (name init) current with)
     (begin
       (define name (make-fluid init))
       (define (current)
         (fluid name))
       (define (with value thunk)
         (let-fluid name value thunk))))))

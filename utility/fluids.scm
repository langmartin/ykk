(define-syntax define-fluid
  (syntax-rules ()
    ((_ (name init) current with)
     (begin
       (define name (make-fluid init))
       (define (current)
         (fluid name))
       (define (with value thunk)
         (let-fluid name value thunk))))
    ((_ (name init) current with let)
     (begin
       (define-fluid (name init) current with)
       (define-syntax let
         (syntax-rules ()
           ((let value . body)
            (with value (list->thunk body)))))))))

(define-syntax list->thunk
  (syntax-rules ()
    ((_ (body ...))
     (lambda () body ...))))
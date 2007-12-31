(define-syntax if-car
  (syntax-rules ()
    ((_ rest default)
     (if (null? rest)
         default
         (car rest)))))

(define-syntax if-cdr
  (syntax-rules ()
    ((_ lst)
     (if (pair? lst)
         (cdr lst)
         '()))))

(define-syntax let-optionals*
  (syntax-rules ()
    ((_ rest ((name val))
        body ...)
     ((lambda (name)
        body ...)
      (if-car rest val)))
    ((_ rest (e1 e2 ...) body ...)
     (begin
       (let-optionals* rest (e1)
                       (let-optionals* (if-cdr rest) (e2 ...)
                                       body ...))))))

(define-syntax let-optionals
  (syntax-rules ()
    ((_ arg ...)
     (let-optionals* arg ...))))

(assert
 (let-optionals '(a b) ((port "a") (string "b")) (list port string)) => '(a b))

(define-syntax call/datum-rest
  (syntax-rules ()
    ((_ rest test? default receiver)
     (if (test? (car rest))
         (receiver (car rest) (cdr rest))
         (receiver default rest)))))

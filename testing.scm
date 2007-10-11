(define functional '())

(define effecting '())

(define (define-test* which thunk expected . test)
  (let ((test? (optional test equal?)))
    (set! which
          (cons (lambda ()
                  (let ((result (thunk)))
                    (or (test? result expected)
                        (error "failed test. got " result
                               ", expected " expected))))
                which))))

(define-syntax define-test
  (syntax-rules ()
    ((_ expr which expected/test ...)
     (define-test*
       which
       (lambda () expr)
       expected/test
       ...))))

(define-syntax define-functional-test
  (syntax-rules ()
    ((_ args ...)
     (define-test functional args ...))))

(define-syntax define-effecting-test
  (syntax-rules ()
    ((_ args ...)
     (define-test effecting args ...))))

(define (run-functional-tests)
  (for-each (lambda (x) (x))
            functional))

(define (run-effecting-tests)
  (for-each (lambda (x) (x))
            effecting))

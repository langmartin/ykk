(define functional '())

(define effecting '())

(define-syntax if-car
  (syntax-rules ()
    ((_ rest default)
     (if (null? rest)
         default
         (car rest)))))

(define (define-test* which thunk expected . test)
  (let ((test? (if-car test equal?)))
    (set! which
          (cons (lambda ()
                  (let ((result (thunk)))
                    (or (test? result expected)
                        (error "failed test. got " result
                               ", expected " expected))))
                which))))

(define-syntax define-test
  (syntax-rules ()
    ((_ which expr expected/test ...)
     (define-test*
       which
       (lambda () expr)
       expected/test
       ...))))

(define-syntax define-some-tests
  (syntax-rules ()
    ((_ which (args ...))
     (define-test which args ...))
    ((_ which call1 call2 ...)
     (begin
       (define-some-tests which call1)
       (define-some-tests which call2 ...)))))

(define-syntax define-functional-test
  (syntax-rules ()
    ((_ args ...)
     (define-test 'functional args ...))))

(define-syntax define-functional-tests
  (syntax-rules ()
    ((_ args ...)
     (define-some-tests 'functional args ...))))

(define-syntax define-effecting-test
  (syntax-rules ()
    ((_ args ...)
     (define-test 'effecting args ...))))

(define-syntax define-effecting-tests
  (syntax-rules ()
    ((_ args ...)
     (define-some-tests 'effecting args ...))))

(define (run-functional-tests)
  (for-each (lambda (x) (x))
            functional))

(define (run-effecting-tests)
  (for-each (lambda (x) (x))
            effecting))

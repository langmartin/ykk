;;;; This is all inactive for now. I've moved the functional tests
;;;; into the assert macro. There doesn't seem much cause for
;;;; re-running functional tests. A sufficient framework will take
;;;; more thought, and should extend assert.

(define functional '())

(define effecting '())

(define-syntax if-car
  (syntax-rules ()
    ((_ rest default)
     (if (null? rest)
         default
         (car rest)))))

(define (define-test* which lst thunk expected . test)
  (let ((test? (if-car test equal?)))
    (set! which
          (cons (lambda ()
                  (let ((result (thunk)))
                    (or (test? result expected)
                        (error "failed test. got " result
                               ", expected " expected))))
                lst))))

(define-syntax define-test
  (syntax-rules ()
    ((_ assertion tree)
     (push-tree! tests
                (lambda ()
                  assertion)))))

(define-syntax define-test
  (syntax-rules ()
    ((_ which val expr expected/test ...)
     (define-test*
       which
       val
       (lambda () expr)
       expected/test
       ...))))

(define-syntax define-some-tests
  (syntax-rules ()
    ((_ which val (args ...))
     (define-test which val args ...))
    ((_ which val call1 call2 ...)
     (begin
       (define-some-tests which val call1)
       (define-some-tests which val call2 ...)))))

(define-syntax define-functional-test
  (syntax-rules ()
    ((_ args ...)
     (define-test 'functional functional args ...))))

(define-syntax define-functional-tests
  (syntax-rules ()
    ((_ args ...)
     (define-some-tests 'functional functional args ...))))

(define-syntax define-effecting-test
  (syntax-rules ()
    ((_ args ...)
     (define-test 'effecting effecting args ...))))

(define-syntax define-effecting-tests
  (syntax-rules ()
    ((_ args ...)
     (define-some-tests 'effecting effecting args ...))))

(define (run-functional-tests)
  (for-each (lambda (x) (x))
            functional))

(define (run-effecting-tests)
  (for-each (lambda (x) (x))
            effecting))

(define-functional-test 2 2)

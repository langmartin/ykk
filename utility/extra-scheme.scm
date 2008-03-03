(define (maybe-force foo)
  (if (promise? foo)
      (force foo)
      foo))

(define promise? procedure?)

(define (always? . x) #t)
(define (never? . x) #f)

;; proj-N procedures to go with SRFI-61

(define-syntax define-proj
  (syntax-rules ()
    ((_ (proj access) ...)
     (begin (define (proj . all) (access all)) ...))))

(define-proj
  (proj-0 car)
  (proj-1 cadr)
  (proj-2 caddr))

;;;; Tests
(begin
  (assert (promise? (delay 'foo)))
  (assert (maybe-force 'foo) => 'foo)
  (assert (maybe-force (delay 'foo)) => 'foo)
  (assert (always? #f))
  (assert (never? #t) => #f)
  (assert (call-with-values (lambda () (values 0 1 2 3)) proj-0) => 0)
  (assert (call-with-values (lambda () (values 0 1 2 3)) proj-1) => 1)
  (assert (call-with-values (lambda () (values 0 1 2 3)) proj-2) => 2))
(define-syntax unless
  (syntax-rules ()
    ((_ test body ...)
     (or test
         (begin
           body ...)))))

(define-syntax when
  (syntax-rules ()
    ((_ test body ...)
     (unless (not test)
             body ...))))

(define-syntax begin1
  (syntax-rules ()
    ((_ e1 e2 ...)
     (let ((result e1))
       e2 ...
       result))))

;; (define (call-while test? thunk)
;;   (let lp ()
;;     (if (test? (thunk))
;;         (lp)
;;         #f)))

;; (define-syntax while
;;   (syntax-rules ()
;;     ((_ test? expr)
;;      (call-while test?
;;                  (lambda ()
;;                    expr)))))

;; (define-syntax until
;;   (syntax-rules ()
;;     ((_ test? expr)
;;      (call-while (lambda (x)
;;                    (not (test? x)))
;;                  (lambda ()
;;                    expr)))))

(define-syntax case-equal
  (syntax-rules (else)
    ((_ key (else body ...))
     (begin body ...))
    ((_ key ((datum ...) body ...))
     (and (member key '(datum ...))
          (begin body ...)))
    ((_ key clause1 clause2 ...)
     (or (case-equal key clause1)
         (case-equal key clause2 ...)))))

(define (make-not proc)
  (lambda x (not (apply proc x))))

(define-syntax if-bind
  (syntax-rules ()
    ((_ bind expr then else ...)
     (let ((bind expr))
       (if bind
           then
           else ...)))))

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

;; COMPOSE a series of unary procedures
;;
;; Conceptual example:
;; (compose foo bar baz) => (lambda (x) (foo (bar (baz x))))
;;
;; Actual result:
;; (compose foo bar baz)
;; =>
;; (lambda x
;;   (call-with-values
;;       (lambda ()
;;         (call-with-values
;;             (lambda ()
;;               (call-with-values
;;                   (lambda () (apply values x))
;;                 baz))
;;           bar))
;;     foo))
(define-syntax compose
  (syntax-rules ()
    ((_ "call" x (procedure))
     (call-with-values (lambda () x) procedure))
    ((_ "call" x (p1 . p2))
     (compose "call" (compose "call" x (p1)) p2)) 
    ((_ "reverse" x (p1) rev)
     (compose "call" x (p1 . rev)))
    ((_ "reverse" x (p1 . p2) rev)
     (compose "reverse" x p2 (p1 . rev)))   
    ((_ . procedures)
     (lambda x (compose "reverse" (apply values x) procedures ())))))

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
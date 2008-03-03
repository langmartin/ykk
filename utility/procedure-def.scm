;; COMPOSE a series of unary procedures
;;
;; (compose foo bar baz) => (lambda (x) (foo (bar (baz x))))
(define-syntax compose
  (syntax-rules ()
    ((_ "expand" x (proc))
     (proc x))
    ((_ "expand" x (p1 p2 ...))
     (compose "expand" (p1 x) (p2 ...)))
    ((_ "reverse" x (proc) (rev ...))
     (compose "expand" x (proc rev ...)))
    ((_ "reverse" x (p1 p2 ...) (rev ...))
     (compose "reverse" x (p2 ...) (p1 rev ...)))
    ((_ proc ...)
     (lambda (x) (compose "reverse" x (proc ...) ())))))

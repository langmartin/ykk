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
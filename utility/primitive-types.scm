(define (maybe-symbol? foo)
  (or (not foo)
      (symbol? foo)))

(define-simple-type :maybe-symbol (:symbol :boolean) maybe-symbol?)

(define (pair-or-null? foo)
  (or (null? foo) (pair? foo)))

(define (maybe-sexpr? foo)
  (or (not foo) (sexpr? foo)))

(define-simple-type :maybe-sexpr () maybe-sexpr?)

(define sexpr? pair-or-null?)
(define-simple-type :sexpr (:maybe-sexpr) sexpr?)

(define (code-block? code)
  (and (sexpr? code)
       (memq (car code) '(begin plist))))
(define-simple-type :code-block (:sexpr) code-block?)

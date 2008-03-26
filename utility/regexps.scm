
(define-syntax perform-case-regex
  (syntax-rules (else)
    ((_ perform var)
     #f)
    ((_ perform var (else expr))
     expr)
    ((_ perform var (pattern expr) next ...)
     (if (perform pattern var)
         expr
         (perform-case-regex perform var next ...)))))

(define-syntax case-regex
  (syntax-rules ()
    ((_ expr ...)
     (perform-case-regex
      (lambda (p v)
        (match p v))
      expr ...))))

(define-syntax case-posix-regex
  (syntax-rules ()
    ((_ expr ...)
     (perform-case-regex
      (lambda (p v)
        (regexp-match (make-regexp p)
                       v 0 #f #t #t))
      expr ...))))

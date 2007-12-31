(define-syntax assert
  (syntax-rules (=>)
    ((_ expr => expected)
     (let ((result expr))
       (or (equal? result expected)
           (error (concat (concat-write 'expr) " => " expected)
                  result))))
    ((_ e1 => r1 e2 ...)
     (begin (assert e1 => r1)
            (assert e2 ...)))
    ((_ expr)
     (let ((result expr))
       (or result (error (concat-write 'expr) #f))))
    ((_ e1 e2 ...)
     (begin (assert e1)
            (assert e2 ...)))))

(define (concat-for-each writer things)
  (call-with-string-output-port
   (lambda (port)
     (call-with-current-output-port
      port
      (lambda ()
        (for-each writer things))))))

(define (concat . things)
  (concat-for-each display things))

(define (concat-write . things)
  (concat-for-each write things))

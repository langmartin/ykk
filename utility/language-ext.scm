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

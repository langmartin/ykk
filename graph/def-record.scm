(define-syntax def-record
  (syntax-rules ()
    ((_ type
        (constructor
         pred
         update
         apply)
        accessor
        ...)
     (begin
       (define-record-type type
         (constructor accessor ...)
         predicate
         (accessor accessor)
         ...)
       (define (update accessor inst ...)
         (constructor (or accessor (accessor inst))
                      ...))
       (define (apply proc obj)
         (proc (accessor obj) ...))))))

(define-syntax def-discloser
  (syntax-rules ()
    ((_ predicate ((obj s48type)) body ...)
     (begin
       (define-simple-type s48type (:value) predicate)
       (define-method &disclose ((obj s48type))
         body ...)))))

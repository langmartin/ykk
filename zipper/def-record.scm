(define-syntax def-record
  (syntax-rules ()
    ((_ type
        constructor
        predicate
        (re-constructor apply)
        accessor
        ...)
     (begin
       (define-record-type type
         (constructor accessor ...)
         predicate
         (accessor accessor)
         ...)
       (define (apply proc obj)
         (proc (accessor obj) ...))
       (define (re-constructor inst accessor ...)
         (constructor (or accessor (accessor inst))
                      ...))))))

(define-syntax def-discloser
  (syntax-rules ()
    ((_ predicate ((obj s48type)) body ...)
     (begin
       (define-simple-type s48type (:value) predicate)
       (define-method &disclose ((obj s48type))
         body ...)))))

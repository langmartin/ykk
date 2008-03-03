(define-syntax define-tagged-list-data
  (syntax-rules ()
    ((_ simple-type construct (make tag) pred? null? me->list list->me)
     (begin
       (define (make . args)
         (list->me args))
       (define (construct entry me)
         (list->me (cons entry (me->list me))))
       (define (pred? foo)
         (and (pair? foo) (eq? (car foo) tag)))
       (define (null? foo)
         (and (pred? foo) (null? (cdr foo))))       
       (define-simple-type simple-type (:pair) pred?)
       (define (me->list (me pred?))
         (cdr me))
       (define (list->me list)
         (cons tag list))))    
    ((_ simple-type construct make pred? null? me->list list->me)
     (define-tagged-list-data simple-type
       construct
       (make 'make)
       pred?
       null?
       me->list
       list->me))))
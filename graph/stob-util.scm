(define :stob z:vector)
(define make-stob zprimitive-vector)
(define stob-ref zvector-ref)
(define stob-set! zvector-set!)
(define stob? zvector?)
(define stob-length zvector-length)

;;;; Allocation
(define (allocate static-identifier . rest)
  (apply zallocate
         (and static-identifier (symbol->string static-identifier))
         rest))

(define (allocate->stob static-identifier thunk)
  (proj-0 (allocate static-identifier thunk identity)))

(define (allocate/verify static-identifier thunk verify commit)
  (let ((stob allocated? (allocate static-identifier thunk commit)))
    (if (and static-identifier (not allocated?))
        (verify stob)
        stob)))

;;;; Accessors
(define-syntax define-stob-accessors
  (syntax-rules ()
    ((_ (name index) ...)
     (begin
       (define (name r)
         (stob-ref r index))
       ...))
    ((_ check? (name index) ...)
     (begin
       (define (name (r check?))
         (stob-ref r index))
       ...))))

;;;; Inspection
(define (stob->list stob)
  (vector-fold-right
   cons
   '()
   stob))

(assert (stob->list (allocate/verify #f (lambda () (make-stob 1 2 3)) always? identity))
        => '(1 2 3))
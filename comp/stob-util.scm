;; Temporary stuff
(define zrecord record)

(define (zrecord-ref stob i)
  (record-ref (stob-data stob) i))

(define (zrecord-set! stob i value)
  (record-set! (stob-data stob) i value))

(define (zrecord? foo)
  (and (pair? foo)       
       (record? (stob-data foo))))

(define make-stob zrecord)
(define stob-ref zrecord-ref)
(define stob-set! zrecord-set!)
(define stob? zrecord?)

;;;; Allocation
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
  (let* ((r (cdr stob))
         (size (record-length r)))
    (let loop ((i (- size 1)) (acc '()))
      (if (= i -1)
          acc
          (loop (- i 1) (cons (record-ref r i) acc))))))
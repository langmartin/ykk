(define-record-type rtd/zvector
  (make-vector* id vector)
  vector?
  (id vector-id)
  (vector r5-vector))

(define-record-discloser rtd/zvector
  (lambda (vec)
    `(zv ,(r5-vector vec))))

(define (restore-vector id r5-vec)
  (let* ((vec (make-vector* id r5-vec)))
    (record-vector vec)
    (bury id vec)
    vec))

(define (restore-vector-update id k obj)
  (vector-set! (exhume id) k obj))

(define (make-vector-general proc . args)
  (restore-vector (uuidgen) (apply proc args)))

(define (make-vector len . fill)
  (apply make-vector-general r5:make-vector len fill))

(define (vector . elements)
  (apply make-vector-general r5:vector elements))

(define (vector-length obj)
  (r5:vector-length (r5-vector obj)))

(define (vector-ref vector k)
  (r5:vector-ref (r5-vector vector) k))

(define (vector-for-each-index proc vector)
  (let ((len (vector-length vector)))
    (let lp ((idx 0))
      (if (not (= idx len))
          (begin
            (proc idx)
            (lp (+ 1 idx)))))))

(define (vector-fold proc nil vector)
  (let ((len (vector-length vector)))
    (let lp ((idx 0) (acc nil))
      (if (= idx len)
          acc
          (lp (+ idx 1)
              (proc (vector-ref vector idx)
                    acc))))))

(define (vector-fold-right proc nil vector)
  (let ((len (vector-length vector)))
    (let lp ((idx 0))
      (if (= idx len)
          nil
          (proc (vector-ref vector idx)
                (lp (+ idx 1)))))))

(define (vector-for-each proc vector)
  (vector-fold (lambda (x acc)
                 (proc x))
               #f
               vector))

(define (vector-map proc vector)
  (vector-fold-right (lambda (x acc)
                       (r5:cons (proc x)
                                acc))
                     '()
                     vector))

(define (vector->list vector)
  (vector-fold-right r5:cons '() vector))

(define (vector->zlist vector)
  (vector-fold-right cons '() vector))

(define (list->vector lst)
  (apply vector lst))

(define (vector-copy vec)
  (let ((out (make-vector (vector-length vec))))
    (vector-for-each-index
     (lambda (idx)
       (r5:vector-set!
        out idx (vector-ref vec)))
     vec)))

(define (vector-set! vec k obj)
  (let* ((new (vector-copy vec))
         (new (r5:vector-set! (r5-vector new) k obj)))
    (record-vector-update vec k obj)
    new))

(define (record-vector vec)
  (write (cons (vector-id vec)
               (vector-map record-car vec))
         (log))
  (newline (log)))

(define (record-vector-update vec k obj)
  (write (list (vector-id vec)
               k
               (record-car obj))
         (log))
  (newline (log)))

(define (restore-vector-update id k obj)
  (let ((vec (exhume id)))
    (vector-set! vec id obj)))

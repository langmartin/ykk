(define (vector-fold-index proc nil vec)
  (fold-numbers proc nil 0 (vector-length vec) 1))

(define (large-equal one two)
  (call-with-current-continuation
   (lambda (false)
     (vector-fold-index
      (lambda (idx acc)
        (if (not acc)
            (false #f)
            (equal? (vector-ref one idx)
                    (vector-ref two idx))))
      #t
      one))))

(define (small-equal one two)
  (vector-fold-index
   (lambda (idx acc)
     (and acc
         (equal? (vector-ref one idx)
                 (vector-ref two idx))))
   #t
   one))

(define (equal? one two)
  (if (and (and (vector? one) (vector? two))
           (= (vector-length one) (vector-length two)))
      (small-equal one two)
      (r5:equal? one two)))

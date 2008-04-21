;;;; srfi-43
(define (vector-fold-index proc nil vector)
  (fold-numbers
   proc
   nil
   0
   (vector-length vector)
   1))

(define (vector-fold proc nil vector)
  (vector-fold-index (lambda (idx acc)
                       (proc (vector-ref vector idx)
                             acc))
                     nil
                     vector))

(define (vector-fold-right-index proc nil vector)
  (fold-right-numbers
   proc
   nil
   0
   (vector-length vector)
   1))

(define (vector-fold-right proc nil vector)
  (vector-fold-right-index
   (lambda (idx acc)
     (proc (vector-ref vector idx) acc))
   nil
   vector))

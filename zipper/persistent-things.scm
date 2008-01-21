(define-record-type zcons
  (cons car cdr)
  pair?
  (car car)
  (cdr cdr))

(def-discloser pair? ((cell :zcons))
  `(zc ,(car cell) ,(cdr cell)))

(define (list? obj)
  (or (pair? obj)
      (null? obj)))

(define (list . args)
  (r5:fold-right cons '() args))

(define (identity x) x)

(define (fold-pair->fold fold-pair cons nil lst)
  (fold-pair (lambda (lst acc)
               (cons (car lst) acc))
             nil
             lst))

(define (fold-pair cons nil lst)
  (if (null? lst)
      nil
      (fold-pair cons
                 (cons lst nil)
                 (cdr lst))))

(define (fold cons nil lst)
  (fold-pair->fold
   fold-pair cons nil lst))

(assert (fold (lambda (x acc) (even? x)) #f (list 2 4 6 8 10)))

(define (fold-pair-right cons nil lst)
  (if (null? lst)
      nil
      (cons lst
            (fold-pair-right cons nil (cdr lst)))))

(define (fold-right cons nil lst)
  (fold-pair->fold
   fold-pair-right cons nil lst))

(assert (fold-right (lambda (x acc) (even? x)) #f (list 2 4 6 8 10)))

(define (for-each proc lst)
  (fold (lambda (x acc)
           (proc x))
         #f
         lst))

(assert (for-each even? (list 2 4 6 8 10)))

(define (assoc/predicate pred? lst tag)
  (call-with-current-continuation
   (lambda (found)
     (for-each (lambda (pair)
                 (if (pred? tag (car pair))
                     (found pair)))
               lst)
     #f)))

(define (assq lst tag)
  (assoc/predicate eq? lst tag))

(define (list-tail lst index)
  (if (zero? index)
      lst
      (list-tail (cdr lst)
                  (- index 1))))

(define (map/cons* proc cons lst)
  (if (null? lst)
      lst
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (proc head))
              (tail1 (map/cons* proc cons tail)))
          (if (and (eq? head1 head) (eq? tail1 tail))
              lst
              (cons head1 tail1))))))

(define (map proc lst)
  (map/cons* proc cons lst))

(assert (map (lambda (x) (+ 1 x)) (list 1 2 3 4)) => (list 2 3 4 5))

(define (filter pred? lst)
  (map/cons* (lambda (x)
               (if (pred? x) x #f))
             (lambda (x tail)
               (if x (cons x tail) tail))
             lst))

(assert (filter even? (list 1 2 3 4)) => (list 2 4))

(define (depth-first handle tree)
  (cond ((null? tree) tree)
        ((handle tree) => identity)
        ((not (pair? tree)) tree)
        (else
         (let ((mapped (map (lambda (kid)
                              (depth-first handle kid))
                            tree)))
           (if (eq? mapped tree)
               tree
               mapped)))))

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





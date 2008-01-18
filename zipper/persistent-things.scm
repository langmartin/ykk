(define-record-type zcons
  (cons car cdr)
  pair?
  (car car)
  (cdr cdr))

(def-discloser pair? ((cell :zcons))
  `(Z ,(car cell) ,(cdr cell)))

(define (list? obj)
  (or (pair? obj)
      (null? obj)))

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

(define (fold-pair-right cons nil lst)
  (if (null? lst)
      nil
      (let ((tail (cdr lst))
            (folded
             (fold-right cons nil (cdr lst))))
        (cons (car lst)
              (if (eq? tail folded)
                  tail
                  folded)))))

(define (fold-right cons nil lst)
  (fold-pair->fold
   fold-pair-right cons nil lst))

(define (for-each proc lst)
  (fold (lambda (x acc)
           (proc x))
         #f
         lst))

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

(define (map* proc lst)
  (if (null? lst)
      lst
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (proc head))
              (tail1 (map* proc tail)))
          (if (and (eq? head1 head) (eq? tail1 tail))
              lst
              (cons head1 tail1))))))

(define (depth-first handle tree)
  (cond ((null? tree) tree)
        ((handle tree) => identity)
        ((not (pair? tree)) tree)
        (else
         (let ((mapped (map* (lambda (kid)
                                (depth-first handle kid))
                              tree)))
           (if (eq? mapped tree)
               tree
               mapped)))))

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

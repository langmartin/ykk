(define (identity x) x)

(define (fold-pair->fold fold-pair cons nil lst)
  (fold-pair (lambda (lst acc)
               (cons (car lst) acc))
             nil
             lst))

(define (map* proc lst)
  (if (null? lst)
      lst
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (proc head))
              (tail1 (map* proc tail)))
          (if (and (eq? head1 head) (eq? tail1 tail))
              lst
              (cons head1 tail1))))))

(define (for-each proc lst)
  (fold (lambda (x acc)
           (proc x))
         #f
         lst))

(define (list-tail lst index)
  (if (zero? index)
      lst
      (list-tail (cdr lst)
                  (- index 1))))

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

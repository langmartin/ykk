(define-record-type logging-cons
  (make-logging-cons lcar lcdr)
  logging-cons?
  logging-cons-id
  lcar
  lcdr)

(define (lnull? lcons)
  (and (logging-cons? lcons)
       (null? (lcdr lcons))))

(define (lpair? lcons)
  (and (logging-cons? lcons)
       (not (null? (lcdr lcons)))))

(define (lcons lcar lcdr)
  (let ((id (uuidgen)))
    (make-logging-cons id lcar lcdr)))

(define (llist lcar lcdr)
  (lcons lcar
         (lcons lcdr '())))

(define (map* f lst)
  (if (lnull? lst)
      lst
      (let ((head (lcar lst)) (tail (lcdr lst)))
        (let ((head1 (f head))
              (tail1 (map* f tail)))
          (if (and (eq? head1 head) (eq? tail1 tail))
              lst
              (lcons head1 tail1))))))

(define (depth-first handle tree)
  (cond ((lnull? tree) tree)
        ((handle tree) =>
         (lambda (new-tree) new-tree))
        ((not (lpair? tree)) tree)
        (else
         (let ((mapped (map* (lambda (kid)
                               (depth-first handle kid))
                             tree)))
           (if (eq? mapped tree)
               tree
               mapped)))))

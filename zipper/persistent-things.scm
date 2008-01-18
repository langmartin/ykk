(define-record-type cons-cell
  (cons car cdr)
  pair?
  (car car)
  (cdr cdr))

(define null? r5:null?)

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

(define (map* proc lst)
  (if (null? lst)
      lst
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (proc head))
              (tail1 (map* proc tail)))
          (if (and (eq? head1 head) (eq? tail1 tail))
              lst
              (cons head1 tail1))))))

(define (list-tail lst index)
  (if (zero? index)
      lst
      (list-tail (cdr lst)
                  (- index 1))))

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

;;;; stuff that was in persistent-immutable
(define *top* (vector))

(define (top-log)
  (display (cons-id *top*) (current-log))
  (newline (current-log)))

(define (top-ref tag)
  (cond ((assq tag *top*) => cdr)
        (else #f)))

(define (top-set tag val)
  (let ((new
         (fold (lambda (x top)
                 (if (eq? tag (car x))
                     (cons (cons tag val)
                           top)
                     (cons x top)))
               '()
               *top*)))
    (set! *top* new)
    (top-log)))

(define (top-del tag)
  (fold (lambda (x top)
          (if (eq? tag (car x))
              top
              (cons x top)))
        '()
        *top*))

;;;; lists
(define-record-type rtd/zcons
  (cons-cons id next car cdr)
  pair?
  (id cons-id)
  (next cons-next)
  (car cons-car)
  (cdr cons-cdr cons-set-cdr!))

(define-record-discloser rtd/zcons
  (lambda (cons)
    `(zc ,(cons-car cons)
         ,(cdr cons))))

(define null '())

(define null? r5:null?)

(define (cons car cdr)
  (let* ((cdr-loc (and (pair? cdr)
                       (cons-id cdr)))
         (cell (cons-cons (uuidgen)
                          cdr-loc
                          car
                          cdr)))
    (bury (cons-id cell) cell)
    (write-cell cell)
    cell))

(define (list . arguments)
  (r5:fold-right cons
                 null
                 arguments))

(define (list? obj)
  (or (null? obj)
      (pair? obj)))

(define car cons-car)

(define (cdr obj)
  (or (cons-cdr obj)
      (let* ((loc (cons-next obj))
             (val (if (not loc) null (exhume loc))))
        (cons-set-cdr! obj val)
        val)))

(define (write-cell cell)
  (or (not (current-log))
      (let-current-output-port
          (current-log)
        (write (list
                're-cons
                (cons-id cell)
                (cons-next cell)
                (disclose-object (cons-car cell))))
        (newline))))

(define (re-cons id next car)
  (bury id
        (cons-cons
         id
         next
         (if next car '())
         #f)))

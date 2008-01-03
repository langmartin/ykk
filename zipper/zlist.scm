(define-record-type zcons-record rtd/zcons
  (zcons-cons id next car cdr)
  zpair?
  (id zcons-id)
  (next zcons-next)
  (car zcons-car)
  (cdr zcons-cdr zcons-set-cdr!))

(define-record-discloser rtd/zcons
  (lambda (zcons)
    `(zcons ,(zcons-car zcons)
            ,(zcdr zcons))))

(define *cdr* (make-table))

(define (exhume id)
  (table-ref *cdr* id))

(define (bury cell)
  (table-set! *cdr* (zcons-id cell) cell))

;;;; Inteface definitions
(define znull (cons 'znull))

(define (znull? obj)
  (eq? znull obj))

(define (zlist? obj)
  (or (znull? obj)
      (zpair? obj)))

(define (zcons zcar zcdr)
  (let* ((cdr-loc (and (zpair? zcdr)
                       (zcons-id zcdr)))
         (cell (zcons-cons (uuidgen)
                           cdr-loc
                           zcar
                           zcdr)))
    (bury cell)
    (zlist-logging-proc cell)
    cell))

(define (zcdr obj)
  (or (zcons-cdr obj)
      (let* ((loc (zcons-next obj))
             (val (if (not loc) znull (exhume loc))))
        (set-zcons-cdr! obj val)
        val)))

;;;; srfi-1+ procs
(define (zmap* proc lst)
  (if (znull? lst)
      lst
      (let ((head (zcar lst)) (tail (zcdr lst)))
        (let ((head1 (proc head))
              (tail1 (zmap* proc tail)))
          (if (and (eq? head1 head) (eq? tail1 tail))
              lst
              (zcons head1 tail1))))))

(define zmap zmap*)

(assert (let* ((lst1 '(1 2 3 4 5 6 7 8))
               (lst2 (zmap* (lambda (x)
                             (if (= x 4)
                                 8
                                 x))
                           lst1)))
          (eq? (zmap* (lambda (x) x) lst2) lst2)))

(define (identity x) x)

(define (zdepth-first handle tree)
  (cond ((znull? tree) tree)
        ((handle tree) =>
         identity)
        ((not (zpair? tree)) tree)
        (else
         (let ((mapped (zmap* (lambda (kid)
                                (zdepth-first handle kid))
                              tree)))
           (if (eq? mapped tree)
               tree
               mapped)))))

(define (zlist-tail lst index)
  (if (zero? index)
      lst
      (zdrop (zcdr lst)
             (- index 1))))

(define (zfold-pair->fold fold-pair cons nil lst)
  (fold-pair (lambda (lst acc)
               (cons (zcar lst) acc))
             nil
             lst))

(define (zfold-pair cons nil lst)
  (if (znull? lst)
      nil
      (zfold-pair cons
                  (cons lst nil)
                  (zcdr lst))))

(define (zfold cons nil lst)
  (fold-pair->fold
   zfold-pair cons nil lst))

(define (zfold-pair-right cons nil lst)
  (if (znull? lst)
      nil
      (let ((tail (zcdr lst))
            (folded
             (zfold-right cons nil (zcdr lst))))
        (cons (zcar lst)
              (if (eq? tail folded)
                  tail
                  folded)))))

(define (zfold-right cons nil lst)
  (fold-pair->fold
   zfold-pair-right cons nil lst))

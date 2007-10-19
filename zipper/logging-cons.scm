;; ,open define-record-types srfi-1 tables util uuid i/o

(define-record-type logging-cons rtd/logging-cons
  (rec-cons id car cdr)
  lpair?
  (id rec-id)
  (car rec-car)
  (cdr rec-cdr))

(define-record-discloser rtd/logging-cons
  (lambda (lcons)
    `(lcons ,(lcar lcons)
            ,(lcdr lcons))))

(define (*log*) (current-error-port))

(define (initialize-log file-name)
  (set! *log*
        (lambda ()
          (force (delay (open-output-file file-name))))))

(define *lcdr* (make-table))

(define lnil (rec-cons 'nil 'nil '()))
(table-set! *lcdr* 'nil lnil)

(define (lcons lcar lcdr)
  (let* ((id (uuidgen))
         (cdr-loc (if (lpair? lcdr)
                      (rec-id lcdr)
                      lcdr))
         (cell (rec-cons id lcar cdr-loc)))
    (table-set! *lcdr* id cell)
    (log-logging-cons cell)
    cell))

(define (lcar obj)
  (if (lpair? obj)
      (rec-car obj)
      (car obj)))

(define (lcdr obj)
  (if (lpair? obj)
      (let ((cdr-loc (rec-cdr obj)))
        (if (pair? cdr-loc)
            cdr-loc
            (table-ref *lcdr* (rec-cdr obj))))
      (cdr obj)))

(define (lnull? obj)
  (or (null? obj)
      (eq? obj lnil)))

(define (llist? obj)
  (or (lnull? obj)
      (lpair? obj)))

(define (llist . args)
  (fold-right lcons lnil args))

(define (log-logging-cons cell . port)
  (let-optionals* port ((port (*log*)))
    (write (list (rec-id cell)
                 (rec-car cell)
                 (rec-cdr cell))
           port)
    (newline port)))

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

(define (testing)
  (let* ((lst1 '(1 2 3 4 5 6 7 8))
         (lst2 (map* (lambda (x)
                       (if (= x 4)
                           8
                           x))
                     lst1)))
    (eq? (map* (lambda (x) x) lst2) lst2)))

(assert (testing))

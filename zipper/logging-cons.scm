(define-record-type logging-cons rtd/logging-cons
  (rec-cons id cdr-tag car cdr)
  lpair?
  (id rec-id)
  (cdr-tag rec-cdr-tag)
  (car rec-car)
  (cdr rec-cdr set-rec-cdr!))

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

(define (exhume tag)
  (table-ref *lcdr* tag))

(define (entomb cell)
  (table-set! *lcdr* (rec-id cell) cell))

(define (log-logging-cons cell . port)
  (let-optionals* port ((port (*log*)))
    (write (list (rec-id cell)
                 (rec-car cell)
                 (or (rec-cdr-tag cell)
                     (rec-cdr cell)))
           port)
    (newline port)))

;;;; Inteface definitions
(define lnil (rec-cons 'nil #f 'nil '()))
(entomb lnil)

(define (lcons lcar lcdr)
  (let* ((cdr-loc (and (lpair? lcdr)
                       (rec-id lcdr)))
         (cell (rec-cons (uuidgen)
                         cdr-loc
                         lcar
                         lcdr)))
    (entomb cell)
    (log-logging-cons cell)
    cell))

(define (lcar obj)
  (if (lpair? obj)
      (rec-car obj)
      (car obj)))

(define (lcdr obj)
  (if (lpair? obj)
      (or (rec-cdr obj)
          (let ((val (exhume (rec-cdr-tag obj))))
            (set-rec-cdr! obj val)
            val))
      (cdr obj)))

;; lpair? defined in the record

(define (lnull? obj)
  (or (null? obj)
      (eq? obj lnil)))

(define (llist? obj)
  (or (lnull? obj)
      (lpair? obj)))

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

;; (assert (testing))

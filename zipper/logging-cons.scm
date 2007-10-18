(define-record-type logging-cons rtd/logging-cons
  (make-logging-cons id car cdr)
  lpair?
  (id logging-cons-id)
  (car lcar)
  (cdr lcdr))

(define-record-discloser rtd/logging-cons
  (lambda (lcons)
    `(lcons ,(lcar lcons)
            ,(lcdr lcons))))

(define lnil (list '()))

(define (lnull? lcons)
  (eq? lcons lnil))

(define (lcons lcar lcdr)
  (let* ((id (uuidgen))
         (cell (make-logging-cons id lcar lcdr)))
    (log-logging-cons cell)
    cell))

(define (llist . args)
  (fold-right lcons
              lnil
              args))

(define (log-logging-cons lcons . port)
  (let-optionals* port ((port (current-output-port)))
   (define (show obj)
     (let ((datum (lpair? obj)))
       (if datum
           (display (logging-cons-id obj) port)
           (write obj port))
       (display #\: port)))
   (and (show lcons)
        (show (lcar lcons))
        (show (lcdr lcons)))
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

;; (define lst1 (llist 4 1 2 3))

;; (define (testing)
;;   (map* (lambda (x)
;;           (if (= x 4)
;;               8
;;               x))
;;         lst1))

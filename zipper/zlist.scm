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

(define *cdr* (make-table))

(define (exhume id)
  (table-ref *cdr* id))

(define (bury cell)
  (table-set! *cdr* (cons-id cell) cell))

;;;; Inteface definitions
(define null '())

(define null? r5:null?)

(define (cons car cdr)
  (let* ((cdr-loc (and (pair? cdr)
                       (cons-id cdr)))
         (cell (cons-cons (uuidgen)
                          cdr-loc
                          car
                          cdr)))
    (bury cell)
    (zlist-log cell)
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

;;;; logging
(define *log* #f)

(define (initialize-log file-name)
  (set! *log*
        (open-output-file file-name)))

(define (zlist-log cell)
  (or (not *log*)
      (writ (cons-id cell)
            (cons-next cell)
            (cons-car cell)
            newline)))

(define (replay-log-port port)
  (let-current-input-port
      port
    (let lp ()
      (let ((next (read)))
        (if (eof-object? next)
            #t
            (begin
              (replay-cell next)
              (lp)))))))

(define (replay-cell lst)
  (apply (lambda (id next car)
           (bury (cons-cons
                  id
                  next
                  (if next car '())
                  #f)))
         lst))

;;;; srfi-1+ procs
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

;; ,open escapes signals fluids records threads
;; ,load new-shift.scm
;; ,open define-record-types

(define call/cc call-with-current-continuation)

(define (map* proc lst)
  (if (null? lst)
      '()
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (proc head))
              (tail1 (map* proc tail)))
          (cond ((eq? head1 keep-element)
                 lst)
                ((drop-element? head1)
                 (trunc lst))
                ((and (eq? head1 head) (eqtail? tail1 tail))
                 lst)
                (else
                 (cons head1 tail1)))))))

(define (map* proc lst)
  (if (null? lst)
      '()
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (proc head)) (tail1 (map* proc tail)))
          (if (drop-element? head1)
              (trunc lst)
              )))))

(define (map* proc lst)
  (if (null? lst)
      '()
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (proc head)) (tail1 (map* proc tail)))
          (let ((tail1 (if (and (pair? tail1)
                                (eq? (car tail1) drop-element))
                           (cdr tail1)
                           tail1)))
            (if (or (eq? head1 keep-element)
                    (and (eq? head1 head)
                         (eqtail? tail1 tail)))
                lst
                (cons head1 tail1)))))))

(define (map* f lst)
  (if (null? lst)
      '()
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (f head))
              (tail1 (map* f tail)))
          (if (and (eq? head1 head) (eq? tail1 tail))
              lst
              (cons head1 tail1))))))

(define (depth-first handle tree)
  (cond ((null? tree) tree)
        ((handle tree) =>
         (lambda (new-tree) new-tree))
        ((not (pair? tree)) tree)
        (else
         (let ((mapped (map* (lambda (kid)
                               (depth-first handle kid))
                             tree)))
           (if (eq? mapped tree)
               tree
               mapped)))))

;;;; So we've got a working depth-first, yeah?

(define-record-type zipper :zipper
  (zipper tree k)
  zipper?
  (tree z-tree)
  (k z-k))

(define-record-discloser :zipper
  (lambda (z)
    `(zipper ,(z-tree z))))

(define keep-element (list 'keep-element))
(define drop-element (list 'drop-element))

(define (drop-element? obj)
  (eq? drop-element obj))

(define (trunc lst)
  (cdr lst))

(define eqtail? eq?)

(define (zip-alist lst)
  (reset (map* (lambda (pair)
                 (shift f (zipper pair f)))
               lst)))

(define (zip-up cursor)
  (if (not (zipper? cursor))
      cursor
      (zip-up ((z-k cursor)
               (z-tree cursor)))))

(define (manipulate-entry proc-k-v proc-lst lst)
  (let lp ((cursor (zip-alist lst)))
    (if (not (zipper? cursor))
        (proc-lst lst)
        (let ((zk (z-k cursor))
              (ztree (z-tree cursor)))
          (let ((result (proc-k-v (car ztree)
                                  (cdr ztree))))
            (if result
                (zip-up (zk result))
                (lp (zk keep-element))))))))

(define (append-entry key value lst)
  (manipulate-entry
   (lambda (k1 v1)
     (and (eqv? k1 key)
          (cons key
                (cons value
                      (if (list? v1) v1 (list v1))))))
   (lambda (lst)
     (cons (cons key
                 value)
           lst))
   lst))

(define matches? eqv?)

(define (delete-entry key lst)
  (manipulate-entry
   (lambda (k1 v1)
     (and (matches? k1 key)
          drop-element))
   (lambda (lst)
     lst)
   lst))


;;;; Sat
(define db '((todo (1 "think") (2 "foo"))
             (things (foo (var) baz bip lsdk khi))
             (more 5 6 7 (5) (6 7 (9)))))


(define cc '())

(define (get-value k . val)
  (set! cc (cons k cc))
  (or (null? val)
      (k (car val)))
  #f)

(define (test tree)
  (and (string? tree)
       (string=? "foo" tree)))

(define (foo)
  (depth-first (lambda (tree)
                 (and (test tree)
                      (call/cc get-value)))
               (force db)))

(define handle
  (let* ((stack '())
         (push (lambda (proc)
                 (set! stack (cons proc stack))))
         (pop (lambda ()
                (set! stack (cdr stack)))))
    (define (todo tree)
      (and (eqv? tree 'todo)
           (push append)))
    (define (append tree)
      )))

(define (append-todo pri desc)
  (let* ((flag #f)
         ())
    (depth-first (lambda (tree)
                   ()))))

(define (prn x)
  (write x)
  (newline)
  #f)

;;;; Construction
(define cons-alist alist-cons)

(define (list->alist lst)
  (reverse (fold-two alist-cons '() lst)))

(define (fold-two proc nil lst)
  (let lp ((lst lst) (acc nil))
    (cond ((null? lst) acc)
          ((not (pair? (cdr lst)))
           (error 'fold-two
                  "degenerate pair; no CADR for CAR"
                  `(car: ,(car lst))))
          (else (lp (cddr lst)
                    (proc (car lst) (cadr lst) acc))))))

(assert (list->alist '(1 2 3 4)) => '((1 . 2) (3 . 4)))
 
(define (unfold-list->alist take-pair make-entry seed plist)
  (let loop ((lst plist) (acc seed))
    (cond ((null? lst)
           acc)
          ((take-pair lst) proj-0
           => (lambda (key value rest)
                (loop rest
                      (cons (make-entry key value)
                            acc))))
          (else
           (error 'unfold-list->alist
                  "degenerate source list"
                  lst)))))

;;;; Merging
(define (update-alist orig update . assoc-method)
  (let-optionals* assoc-method ((assq assq))
    (map* (lambda (old)
            (or (assq (car old) update)
                old))
          orig)))

(define (update-force-alist orig update . assoc-method)
  (let-optionals* assoc-method ((assq assq))
   (fold (lambda (x acc)
           (if (assq (car x) acc)
               acc
               (cons x acc)))
         '()
         (append (reverse update)
                 (reverse orig)))))

(define (merge-alists/template kons kdr template default . alists)
  (really-merge-alists
   (lambda (orig update)
     (let ((key (car orig)))
       (kons key
             ((cond ((assq key template) => cadr)
                    (else default))              
              (kdr orig)
              (kdr update)))))   
   alists))

(define (merge-alists proc . alists)
  (really-merge-alists proc alists))

(define (really-merge-alists proc alists)
  (cond ((null? alists)
         alists)
        ((null? (cdr alists))
         (car alists))
        ((null? (cddr alists))
         (reverse (merge-two-alists proc (cadr alists) (car alists))))
        (else
         (reverse (fold (cut merge-two-alists <> <>)
                        (reverse (car alists))
                        (cdr alists))))))

(define (merge-two-alists proc update orig)
  (let loop ((orig orig) (add update) (acc '()))
    (cond ((and (null? orig) (null? add))
           acc)
          ((null? add)
           (loop (cdr orig)
                 add
                 (cons (car orig) acc)))           
          ((null? orig)
           (loop orig
                 (cdr add)
                 (cons (car add)
                       acc)))           
          ((remove-key (caar orig) add) proj-0
           => (lambda (pair new-add)
                (loop (cdr orig)
                      new-add
                      (cons (proc (car orig) pair)
                            acc))))          
          (else
           (loop (cdr orig)
                 add
                 (cons (car orig)
                       acc))))))

(define (remove-key key alist)
  (let ((found remaining (partition (cut car-eq? key <>) alist)))
    (if (null? found)
        (values #f alist)
        (values (car found) remaining))))

(define (car-eq? thing pair)
  (eq? thing (car pair)))

;;;; Miscellaneous
(define-syntax let-foldr*
  (syntax-rules ()
    ((_ cons nil (tag val))
     (cons 'tag val nil))
    ((_ cons nil (tag val) (tag1 val1) ...)
     (letrec ((tag val))
       (cons 'tag tag
             (let-foldr* cons nil (tag1 val1) ...))))))

(define (alist? lst)
  (and (pair? lst)
       (pair? (car lst))))

(define (alist-key-index = key lst)
  (list-index (lambda (pair)
                (= (car pair) key))
              lst))

(define (map-car proc lst)
  (map-in-order
   (lambda (pair)
     (cons (proc (car pair))
           (cdr pair)))
   lst))

;;;; Keyword Arguments
(define (alist-has-keys? alist keys)
  (if (null? keys)
      #t
      (and (assq (car keys) alist)
           (alist-has-keys? alist (cdr keys)))))

(define (alist-has-only-keys? alist keys)
  (lset<= eq? (map car alist) keys))

(define (alist-has-exactly-keys? alist keys)
  (and (= (length alist) (length keys))
       (alist-has-keys? alist keys)))

(define (project-alist-onto project default keys alist)
  (reverse
   (fold (lambda (key acc)
           (cons (cond ((assq key alist) => project)
                       (else (default key)))
                 acc))
         '()
         keys)))

(define (partition-alist keys alist)
  (partition (lambda (pair)
               (memq (car pair) keys))
             alist))

(define-syntax bind-alist*
  (syntax-rules ()
    ((_ proc (keys ...) alist)
     (apply values (project-alist-onto proc
                                       (lambda (x) #f)
                                       '(keys ...)
                                       alist)))))

(define-syntax bind-spec
  (syntax-rules ()
    ((_ (keys ...) alist)
     (bind-alist* cadr (keys ...) alist))))

(define-syntax bind-alist
  (syntax-rules ()
    ((_ (keys ...) alist)
     (bind-alist* cdr (keys ...) alist))))

(define-syntax pluck-alist*
  (syntax-rules ()
    ((_ proc (keys ...) alist)
     (let ((mapped (project-alist-onto proc
                                       (lambda (x) #f)
                                       '(keys ...)
                                       alist))
           (rest (remove (lambda (item)
                           (memq (car item) '(keys ...)))
                         alist)))
       (apply values (append mapped (list rest)))))))

(define-syntax pluck-spec
  (syntax-rules ()
    ((_ (keys ...) alist)
     (pluck-alist* cadr (keys ...) alist))))

(define-syntax pluck-alist
  (syntax-rules ()
    ((_ (keys ...) alist)
     (pluck-alist* cdr (keys ...) alist))))

(assert (values->list (pluck-spec (a b c) '((a 1) (b 2) (c 3))))
        => '(1 2 3 ()))

(assert (values->list (pluck-alist (a b c) '((a . 1) (b . 2) (c . 3))))
        => '(1 2 3 ()))

(define (keyword-projector/defaults keys/defaults . project)
  (let* ((keys (map-in-order maybe-car keys/defaults))
         (normal (map-in-order keys->alist keys/defaults))
         (required (remove pair? keys/defaults))
         (project (if (null? project) cadr (car project))))

    (define (default key)
      (cond ((assq key normal) => cadr)
            (else (raise-condition 'no-value-found-for-required-keyword
                                   key))))    

    (lambda (alist . maybe-default)
      (let ((default (if (null? maybe-default)
                         default
                         ((lambda (f) (compose f default)) (car maybe-default)))))        
        (cond ((not (alist-has-keys? alist required))
               (raise-condition 'missing-required-keyword
                                `(required: ,@required)
                                `(given: ,@alist)))
              ((not (alist-has-only-keys? alist keys))
               (raise-condition 'unexpected-keyword
                                `(allowed: ,@keys)
                                `(given: ,@alist)))
              (else
               (project-alist-onto project default keys alist)))))))

(define (keyword-partitioner/defaults project . keys/defaults)

  (define (make-partitioner partitions)    
    (lambda (alist . default)
      (let ((default (if (null? default) #f (car default))))        
        (let loop ((p (car partitions)) (rest (cdr partitions)) (alist alist) (acc '()))
          (if (null? rest)
              ;; The final step requires no partitioning, only projection
              (apply values
                     (reverse (cons (apply-projector p alist default)
                                    acc)))
              (let ((partitioned remainder (apply-partition p alist)))
                (loop (car rest)
                      (cdr rest)
                      remainder
                      (cons (apply-projector p partitioned default) acc))))))))  

  (define (make-partition keys/defaults)
    (cons (map-in-order maybe-car keys/defaults)
          (keyword-projector/defaults keys/defaults project)))

  (define (apply-partition partition alist)
    (partition-alist (car partition) alist))

  (define (apply-projector partition alist default)
    (if default
        ((cdr partition) alist default)
        ((cdr partition) alist)))
  
  (cond ((null? keys/defaults)
         (error 'wrong-number-of-arguments
                'keyword-partitioner/defaults))
        ((null? (cdr keys/defaults))
         (keyword-projector/defaults (car keys/defaults) project))
        (else
         (make-partitioner (map-in-order make-partition keys/defaults)))))

(define (maybe-car foo)
  (if (pair? foo)
      (car foo)
      foo))

(define (keys->alist foo)
  (if (pair? foo)
      foo
      (list foo)))

;;;; Referencing / Unstructuring

;; Note: argument order is opposite of ASSOC to conform with the
;;  argument order of most *-REF procedures.
(define (alist-ref lst name . default)
  (cond ((assq name lst) => cdr)
        (else (if (null? default)
                  #f
                  (car default)))))

(define (keylst-null keylst val)
  (fold-right
   (lambda (key tail)
     (list (cons key
                 (if (null? tail) val tail))))
   '()
   keylst))

(define (alist-key-index = key lst)
  (list-index (lambda (pair)
                (= (car pair) key))
              lst))

(define-syntax unalist
  (syntax-rules ()
    ((_ alist pattern ...)
     (unalist-proc alist '(pattern ...)))))

(define (alist-tree-insert* keylst val alist)
  (let ((key (car keylst)))
    (map* (lambda (pair)
            (let ((head (car pair)) (tail (cdr pair)))
              (if (eq? key head)
                  (cons key
                        (if (alist? tail)
                            (alist-tree-insert (cdr keylst) val tail)
                            (cons val
                                  (if (atom? tail) (list tail) tail))))
                  pair)))
         alist)))

(define (alist-tree-insert keylst val alist)
  (if (null? alist)
      (keylst-null keylst val)
      (let ((merged (alist-tree-insert* keylst val alist)))
        (if (eq? merged alist)
            (cons (car (keylst-null keylst val))
                  alist)
            merged))))

(assert
 (alist-tree-insert
  '(zup) 4
  (alist-tree-insert
   '(foo bar) 3 '((baz . 4) (foo . ((bar . 3) (b . 5) (a . 6))) (zup . 5))))
 => '((baz . 4) (foo (bar 3 3) (b . 5) (a . 6)) (zup 4 5)))

(assert
 (cdr (assq 'foo (alist-tree-insert '(foo bar) 3 '())))
 => '((bar . 3)))

(assert
 (alist-tree-insert '(foo bar) 3 '((baz . 4)))
 => '((foo . ((bar . 3))) (baz . 4)))

(define (unalist-proc alist pattern)
  (apply values
         (reverse
          (let recur-fold ((lst alist) (p pattern) (path '()) (acc '()))
            (if (null? p)
                acc
                (fold (lambda (sub-p acc)
                        (if (pair? sub-p)
                            (recur-fold (unalist-ref (car sub-p) lst path pattern)
                                        (cdr sub-p)
                                        (cons (car sub-p) path)
                                        acc)
                            (cons (unalist-ref sub-p lst path pattern)
                                  acc)))
                      acc
                      p))))))

(define (unalist-ref key lst path pattern)
  (cond ((assq key lst) => cdr)
        (else (error 'unalist
                     "pattern does not match data"
                     `(pattern: ,pattern)
                     `(path: ,@(reverse path))))))

;;;; Tests
(begin
  (assert (list->alist '(1 2 3 4)) => '((1 . 2) (3 . 4)))
  
  (assert (fold-two alist-cons '() '(1 2 3 4))
          => (unfold-list->alist
              (lambda (lst)
                (values (car lst) (cadr lst) (cddr lst)))
              cons
              '()
              '(1 2 3 4)))
  
  (assert
   (update-alist '((a . 1) (b . 2) (c . 3)) '((b . 42) (d . 3))) =>
   '((a . 1) (b . 42) (c . 3)))
  
  (assert
   (update-force-alist
    '((a . 1) (b . 2) (c . 3)) '((b . 42) (d . 3))) =>
    '((a . 1) (c . 3) (b . 42) (d . 3)))

  (assert (with-exception-catcher
           (lambda (c prop) 'got-error)
           (lambda () (fold-two alist-cons '() '(1 2 3))))
          => 'got-error)
    
  (let ((lst `((a . ((b . 1) (c . 2)))
               (d . 3))))
    (call-with-values (lambda () (unalist lst (a b c) d))
      (lambda v
        (assert v => '(1 2 3)))))
  
  (let ((lst1 '((a . 1) (b . 2) (c . 3)))
        (lst2 '((a . 10) (c . 30) (d . 40))))
    
    (assert
     (merge-alists proj-1 lst1 lst2)     
     => '((a . 10) (b . 2) (c . 30) (d . 40)))
    
    (assert
     (merge-alists/template cons cdr `((a ,proj-0)) proj-1 lst1 lst2)
     => '((a . 1) (b . 2) (c . 30) (d . 40))))

  (assert
   ((keyword-projector/defaults `(b a c (d #f)))
    `((a 1) (b 2) (c 3)))
   => '(2 1 3 #f))

  (let* ((projector (keyword-partitioner/defaults cadr `(a (b #f)) `(c (d #f) e)))
         (a b (projector `((c 1) (d 2) (a 3) (e 4)))))    
    (assert a => '(3 #f))
    (assert b => '(1 2 4))))

;; (assert
;;  (let ((entry rest (alist-remove 'one '((one 1)
;;                                         (two 2)
;;                                         (three 3)))))
;;    (list entry rest))
;;  => '((one 1) ((two 2) (three 3))))

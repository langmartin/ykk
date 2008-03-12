(define (plist->alist lst)
  (fold-two alist-cons '() lst))

(define (list->alist lst)
  (reverse (plist->alist lst)))

(define (update-alist orig update)
  (map (lambda (old)
         (or (assq (car old) update)
             old))
       orig))

(define (update-force-alist orig update)
  (fold (lambda (x acc)
          (if (assq (car x) acc)
              acc
              (cons x acc)))
        '()
        (append (reverse update)
                (reverse orig))))

(define (fold-two proc nil lst)
  (let lp ((lst lst) (acc nil))
    (cond ((null? lst) acc)
          ((not (pair? (cdr lst)))
           (error 'fold-two
                  "degenerate pair; no CADR for CAR"
                  `(car: ,(car lst))))
          (else (lp (cddr lst)
                    (proc (car lst) (cadr lst) acc))))))

(define cons-alist alist-cons)

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

(begin
  (assert (plist->alist '(1 2 3 4)) => '((3 . 4) (1 . 2)))
  (assert (list->alist '(1 2 3 4)) => '((1 . 2) (3 . 4)))
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
          => 'got-error))

(define (list->alist lst)
  (let lp ((lst lst))
    (if (null? lst)
        '()
        (cons (cons (car lst)
                    (cadr lst))
              (lp (cddr lst))))))

(assert (list->alist '(1 2 3 4)) => '((1 . 2) (3 . 4)))


(define (update-alist orig update)
  (map (lambda (old)
         (or (assq (car old) update)
             old))
       orig))

(assert
 (update-alist '((a . 1) (b . 2) (c . 3)) '((b . 42) (d . 3))) =>
 '((a . 1) (b . 42) (c . 3)))

(define (update-force-alist orig update)
  (fold (lambda (x acc)
          (if (assq (car x) acc)
              acc
              (cons x acc)))
        '()
        (append (reverse update)
                (reverse orig))))

(assert
 (update-force-alist
  '((a . 1) (b . 2) (c . 3)) '((b . 42) (d . 3))) =>
  '((a . 1) (c . 3) (b . 42) (d . 3)))

(define (fold-two proc nil lst)
  (let lp ((lst lst) (acc nil))
    (if (null? lst)
        acc
        (lp (cddr lst)
            (proc (car lst)
                  (cadr lst)
                  acc)))))

(define (cons-alist key val nil)
  (cons (cons key val) nil))

(define-syntax let-foldr*
  (syntax-rules ()
    ((_ cons nil (tag val))
     (cons 'tag val nil))
    ((_ cons nil (tag val) (tag1 val1) ...)
     (letrec ((tag val))
       (cons 'tag tag
             (let-foldr* cons nil (tag1 val1) ...))))))

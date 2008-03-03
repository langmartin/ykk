;;;; Graph Paths
;;;;
;;;; Path queries and actions implemented on tree traversal
;;;; primitives.

(define-condition
  path-error (graph-error)
  path-error?)

;;;; Paths
(define path->list
  (let ((cs (char-set-complement (char-set #\/))))
    (lambda (path)
      (if (string? path)
          (let ((p (map-in-order string->symbol (string-tokenize path cs))))
            (if (absolute? path)
                (make-absolute p)
                p))
          path))))

(define (absolute? p)
  (cond ((string? p) (string-prefix? "/" p))
        ((pair? p) (not (car p)))
        ((null? p) #f)
        (else (path-error 'wrong-type-argument 'absolute? p))))

(define relative? (compose not absolute?))

(define (make-absolute p)
  (cond ((absolute? p) p)
        ((string? p) (string-append "/" p))
        (else (cons #f p))))

;;;; Movement
(define-syntax define-movements
  (syntax-rules ()
    ((_ (name dir) ...)
     (begin
       (define (name z)
         (move dir (z-item z) z))
       ...))))

(define-movements
  (up 'up)
  (really-down 'down)
  (in-order 'in-order)
  (really-prev 'prev-in-order)
  (really-next 'next-in-order))

(define (down z)
  (if (z-graph? z)
      (really-down z)
      (in-order z)))

(define (first-child z)
  (if (z-graph? z)
      (move-to-child down z down)           
      (down (parent z))))

(define (parent z)
  (let loop ((z (up z)))
    (if (z-child? z)
        (loop (up z))
        z)))

(define (prev z)
  (sibling really-prev z))

(define (next z)
  (sibling really-next z))

(define (sibling sib z)
  (if (z-graph? z)
      (move-to-child sib (up z) down)
      (move-to-child sib z identity)))

(define (move-to-child move z with-child)
  (cond ((move z) z-child? => with-child)
        (else #f)))

;;;; Resolution
(define (name z)
  (item-name (z-item z)))

(define (item-name g)
  ((if (graph? g) graph-name child-name)
   g))

;; RESOLVE a PATH where the first element of PATH must match G.
(define (resolve g path)
  (let ((z (if (graph-zipper? g) g (zip-graph traverse g)))
        (p (path->list path)))
    (cond ((null? path) z)
          ((eq? (name z) (car p)) (resolve-in z (cdr p)))
          (else (does-not-exist 'resolve g path)))))

;; RESOLVE a PATH where the first element of PATH must be a child of
;; G.
(define (resolve-in g path)
  (let ((z (if (graph-zipper? g) g (zip-graph traverse g)))
        (p (path->list path)))
    (cond ((resolve-through (down z) p) => up)
          (else (does-not-exist 'resolve-in g path)))))

;; RESOLVE path P from children to children.
(define (resolve-through z p)
  (cond ((null? p) z)                
        ((find-child (name-pred (car p)) z)
         => (lambda (c) (resolve-through (down (down c)) (cdr p))))
        (else #f)))

;; FIND a child matching a PRED?
(define (find-child pred? z)
  (cond ((z-graph? z) (find-child pred? (down z)))
        ((z-end? z) #f)
        ((pred? z) z)        
        (else (find-child pred? (really-next z)))))

(define (name-pred n)
  (lambda (z) (eq? (name z) n)))

(define (does-not-exist proc-name g path)
  (path-error 'path-does-not-exist proc-name `(graph: ,g) `(path: ,path)))

(define (z-graph? z)
  (graph? (z-item z)))

(define (z-child? z)
  (child? (z-item z)))

(define (z-end? z)
  (end-of-children? (z-item z)))

(define (z-leaf? z)
  (leaf? (z-item z)))

;;;; Tests
(begin

  (define-type :foo () (a) (b))

  (let* ((show-graph (cut map->list graph-name <>))
         (perform (lambda (z op) (zip-all-the-way-up (move 'up (op (z-item z)) z))))
         (insert (lambda (g child) (replace-children g (add-child child (graph-children g))))))    
    
    (assert (path->list "/foo/bar/baz") => '(#f foo bar baz))
    (assert (path->list "foo/bar/baz") => '(foo bar baz))
    (assert (path->list '(foo bar)) => '(foo bar))
    (assert (absolute? "/foo"))
    (assert (absolute? '(#f foo)))
    (assert (relative? "foo"))
    (assert (relative? '(foo)))

    (let ((top (root (node :foo
                           (plist (a 1) (b 2))
                           (children ('c1 (node :foo (plist (a 3) (b 4))))
                                     ('c2 (node :foo (plist (a 5) (b 6)))))))))

      (assert (show-graph top) => '(#f (c1) (c2)))      

      (let ((mod (perform (resolve top "/c1")
                          (cut insert <>
                               (edge 'foo (node :foo (plist (a 21) (b 22))))))))
        (assert (show-graph mod) => '(#f (c1 (foo)) (c2)))

        (let ((mod2 (perform (resolve mod "/c1/foo")
                             (cut insert <>
                                  (edge 'bar (node :foo (plist (a 31) (b 32))))))))
          (assert (show-graph mod2) => '(#f (c1 (foo (bar))) (c2)))

          )))))





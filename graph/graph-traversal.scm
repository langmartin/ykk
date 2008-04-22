;;;; Graph Traversal
(define (traverse proc foo)
  (cond ((graph? foo)
         (traverse-graph proc foo))
        ((or (children? foo) (end-of-children? foo))
         (share-children foo (cut traverse-children-in-order proc <>)))
        (else
         (graph-error 'wrong-argument-type 'traverse foo "expecting graph or child-list"))))

;; WALK could be useful for inspection
(define (walk proc g)
  (proc g)
  (fold-children (lambda (c acc)
                   (walk proc (child->graph c)))
                 #f
                 (graph-children g)))

;; MAP->LIST could also be useful for inspection
(define (map->list proc g)
  (cons (proc g)
        (fold-children (lambda (c acc)
                         (cons (map->list proc (child->graph c))
                               acc))
                       '()
                       (graph-children g))))

;;;; Zipper
(define-record-type :graph-zipper
  (graph-zipper dir item k)
  graph-zipper?
  (dir z-dir)
  (item z-item)
  (k z-k))

(define (zip-graph enumerator g)
  (reset (enumerator (lambda (dir g)
                       (shift k (graph-zipper dir g k)))
                     g)))

(define (move dir new-g z)
  ((z-k z) dir new-g))

(define (zip-all-the-way-up z)
  (if (graph-zipper? z)
      (zip-all-the-way-up (move 'up (z-item z) z))
      z))

;;;; Example: Operations on Children

;; MAP-FILTER-CHILDREN is an example of how to make a procedure that
;; transforms a set of children and preserves sharing.
(define (map-filter-children proc children)

  (define (replace child)
    (lambda (new)
      (share-child (lambda (this next)
                     (values new (map next)))
                   child)))

  (define (map child)
    (cond ((end-of-children? child) child)
          ((proc child) => (replace child))
          (else (map (next-child child)))))

  (share-children map children))

(define (remove-children pred? children)
  (map-filter-children (lambda (c) (and (not (pred? c)) (child->graph c)))
                       children))

(define (remove-children-named names children)
  (remove-children (if (symbol? names)
                       (lambda (c) (eq? (child-name c) names))
                       (lambda (c) (memq (child-name c) names)))
                   children))

;;;; Example: Higher Order Zipper
(define-record-type rtd/tracker
  (make-tracker z dpath)
  tracker?
  (z cursor)
  (dpath dpath))

(define (track enumerator g)
  (let ((z (zip-graph enumerator g)))
    (make-tracker z (list (z-dir z)))))

(define (t-item t)
  (z-item (cursor t)))

(define (t-k t)
  (z-k (cursor t)))

(define (t-dir t)
  (z-dir (cursor t)))

;; This higher-order zipper uses simpler (and more restrictive)
;; directions.
(define (move-tracker dir new-g tracker)
  (case dir
    ((up)    (move-up new-g tracker))
    ((down)  (move-down new-g tracker))
    ((left)  (move-left new-g tracker))
    ((right) (move-right new-g tracker))
    (else (bad-direction dir new-g "move-tracker: unrecognized direction"))))

(define (t-all-the-way-up t)
  (if (tracker? t)
      (t-all-the-way-up (move-tracker 'up (t-item t) t))
      t))

;; --------------------
;; Internal

(define (really-move-tracker dir new-g t)
  (let ((new-z (move dir new-g (cursor t))))
    (cond ((not (graph-zipper? new-z))
           new-z)
          ((memq (z-dir new-z) '(up prev-in-order))
           (make-tracker new-z (cdr (dpath t))))
          (else
           (make-tracker new-z (cons (z-dir new-z) (dpath t)))))))

(define (move-down g t)
  (and (not (leaf? g))
      (let ((c (really-move-tracker 'down g t)))
        (really-move-tracker 'in-order (t-item c) c))))

(define (move-up g t)
  (let loop ((c (really-move-tracker 'up g t)))
    (cond ((not (tracker? c)) c)
          ((not (child? (t-item c))) #f)
          (else
           (case (car (dpath c))
             ((in-order)
              (really-move-tracker 'up (t-item c) c))
             ((next-in-order)
              (loop (really-move-tracker 'prev-in-order (t-item c) c)))
             (else #f))))))

(define (move-left g t)
  (let ((c (really-move-tracker 'up g t)))
    (cond ((not (tracker? c)) c)
          ((not (eq? (car (dpath c)) 'next-in-order)) #f)
          (else
           (let ((prev (really-move-tracker 'prev-in-order (t-item c) c)))
             (really-move-tracker 'in-order (t-item prev) prev))))))

(define (move-right g t)
  (let ((c (really-move-tracker 'up g t)))
    (if (not (tracker? c))
        c
        (let ((next (really-move-tracker 'next-in-order (t-item c) c)))
          (if (end-of-children? (t-item next))
              #f
              (really-move-tracker 'in-order (t-item next) next))))))

;;;; Internal
(define (traverse-graph proc g)

  ;; MOVE -- can move across :graph or :child
  (define (move next-dir init-dir old-g)
    (let* ((new-dir g (proc init-dir old-g))
           (dir (next-dir new-dir))
           (leaf (leaf? g)))
      (cond ((eq? dir 'up)
             ;; UP is always supported.  When moving up, simply return
             ;; the handled item.
             g)
            ((memq dir '(next down))
             (move (next 'up)
                   'up
                   (update-children g (cut traverse-children-in-order proc <>))))
            (else
             (bad-direction dir g "unsupported movement")))))  

  ;; Start moving by going down and letting next-dir be anything.
  (move identity 'down g))

(define (traverse-children-in-order proc child)

  (define (move next-dir init-dir old-child)
    (let* ((new-dir child (proc init-dir old-child))
           (dir (next-dir new-dir)))
      (if (end-of-children? child)
          child
          (case dir
            ((up prev-in-order)
             child)
            ((next-in-order)
             (move (next 'prev-in-order)
                   'prev-in-order
                   (update-next child (cut move identity 'next-in-order <>))))
            ((in-order next)
             (move (next 'next-in-order)
                   'up
                   (update-this child (cut traverse-graph proc <>))))
            (else
             (bad-direction dir child "unsupported movement"))))))

  (move identity 'in-order child))

;; Close over which direction will be taken to move to the next
;; location using the natural traversal strategy (depth first right
;; now).
(define (next next-dir)
  (lambda (dir) (if (eq? dir 'next) next-dir dir)))

(define (bad-direction dir g message)
  (graph-error 'unknown-traversal-action
               message
               `(dir: ,dir)
               `(graph: ,g)))

(define (update-children g proc)
  (replace-children g (share-children proc (graph-children g))))

(define (update-this child proc)
  (share-child (update-child proc identity) child))

(define (update-next child proc)
  (share-child (update-child identity proc) child))

(define (update-child update-this update-next)
  (lambda (this next)
    (values (update-this this)
            (update-next next))))

;;;; Tests
(begin

  (define-record-type/primitive :foo a b)
  
  (define-record-type/primitive :bar d e)

  (define-record-type/primitive :baz f g)  

  (define *top*
    (root (node :foo
                (open ykk (parent))

                (begin
                  (define a 'a-value)
                  (define b 'b-value)

                  (define offset 1)
                  (define (adjust distance)
                    (+ offset distance)))

                (for-children (compound-interface
                               (parent-interface)
                               (export adjust)))
                (children ('c1 (node :bar (plist (d 1) (e (adjust d)))))
                              ('c2 (node :baz (plist (f 'f-value) (g "g value"))))
                              ('c3 (node :foo (plist (a 'av) (b 'bv))))))))

  (define (show-graph t)
    (map->list graph-name (t-all-the-way-up t)))

  ;; zipper
  (let ((z (zip-graph traverse *top*)))
    (let ((c (move 'down (z-item z) z)))

      ;; remove the first child, assuming representation of children
      ;; as a list
      (assert (show-graph (zip-all-the-way-up
                           (move 'up (z-item (move 'next-in-order (z-item c) c)) c)))
              => '(#f (c2) (c3)))

      (let ((g (move 'in-order (z-item c) c)))
        (assert (graph-name (z-item g)) => 'c1))))

  ;; tracking zipper
  (let ((t (track traverse *top*)))
    (assert (root? (t-item t)))

    (assert (show-graph (replace-children
                         (t-item t)
                         (remove-children-named 'c1 (graph-children (t-item t)))))
            => '(#f (c2) (c3)))

    (let ((c1 (move-tracker 'down (t-item t) t)))
      (assert (graph-name (t-item c1)) => 'c1)

      (let ((c2 (move-tracker 'right (t-item c1) c1)))
        (assert (graph-name (t-item c2)) => 'c2)

        (let ((c3 (move-tracker 'right (t-item c2) c2)))
          ;; no more right siblings
          (assert (move-tracker 'right (t-item c3) c3) => #f)

          ;; can move left again
          (assert (graph-name (t-item (move-tracker 'left (t-item c3) c3)))
                  => 'c2)

          ;; sharing is preserved
          (assert (eq? *top* (t-all-the-way-up c3)))          

          ;; updating works
          (let ((new-top (t-all-the-way-up
                          (move-tracker 'up
                                        (replace-children
                                         (t-item c1)
                                         (children ('c1-1 (node :foo (plist (a 1) (b 2))))
                                                   ('c1-2 (node :bar (plist (d 3) (e 4))))))
                                        
                                        c1))))

            (assert (map->list graph-name new-top)
                    => '(#f (c1 (c1-1) (c1-2)) (c2) (c3)))

            ;; maximum possible sharing is preserved
            (let* ((t (track traverse new-top))
                   (new-c1 (move-tracker 'down (t-item t) t))
                   (new-c2 (move-tracker 'right (t-item new-c1) new-c1)))
              (assert (eq? (t-item c2) (t-item new-c2)))))

          )))))

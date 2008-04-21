;; The process of merging two possibly conflicting
;; trees requires a third tree to establish a basis of
;; conflicts. This process is divided into two parts: diffing and
;; merging.  (NOTE: this method is weak; I would like to implement
;; a full matching/diffing/merging algorithm as seen here:
;; http://citeseer.ist.psu.edu/chawathe96change.html
;;
;; The diffing process:
;;
;; 1. Let a and b be separate trees. Flatten
;;      a and b (best way to cover all
;;      cases of movement).  We should optimize
;;      this to check for eq? nodes at the top
;;      of the trees and only flatten the general
;;      areas we are interested in.
;; 2. Run an LCS algorithm on a' and b' to generate
;;      a list of matches, deletions, and insertions,
;;      using a match? procedure provided by the user.
;; 3. Return a set of matches (possible content changes,
;;      deletions, and insertions.
;; 
;; The merging process:
;;
;; 1. Let x and y be the two trees, and z be the original
;;      tree.  Run the diffing algorithm on z and x, and
;;      z and y, to produce the change sets z_x and z_y.
;; 2. Run through a list conflict handlers, which are lambdas
;;      that receive z_x and z_y.  Each conflict handler
;;      should return a new change set, which modifies z_x
;;      to indicate conflicts.  Conflicts are indicated
;;      by special conflict nodes, having two children
;;      which are the changes that conflict.
;;      We provide a few built-in conflict handlers, but an
;;      application can provide its own as well.
;; 3. Run through both match sets and apply the changes
;;      to the original tree.  The order matters here.
;;

;;; Matching & Diffing ----------------

;; This lcs algorithm was pulled from Gauche Scheme and
;; tweaked slightly to give us back what we need.
;;
;; The base algorithm.   This code implements
;; Eugene Myers, "An O(ND) Difference Algorithm and Its Variations",
;; Algorithmica Vol. 1 No. 2, 1986, pp. 251-266.
;; It takes O((M+N)D) time and O((M+N)L) space, where
;; N = (length a), M = (length b), D is the length of the smallest edit
;; sequence (SES), and L is the length of the longest common subsequence (LCS).
;; In most applications the difference is small, so it is much better than
;; DP algorithm that is generally O(MN) time and space complextiy.
;; The worst case where a and b totally differ is O((M+N)^2).
;; The Myers's paper gives refinement of the algorithm
;; that improves worst case behavior, but I don't implement it yet. --[SK]

(define (lcs-with-positions a-ls b-ls . opt-eq)
  (let* ((eq (safe-car opt-eq equal?))
         (A  (list->vector a-ls))
         (B  (list->vector b-ls))
         (N  (vector-length A))
         (M  (vector-length B))
         (M+N (+ N M))
         (V_d (make-vector (+ (* 2 M+N) 1) 0))
         (V_r (make-vector (+ (* 2 M+N) 1) '()))
         (V_l (make-vector (+ (* 2 M+N) 1) 0)))

    (let-syntax ((vd
                  (syntax-rules ()
                    ((vd i) (vector-ref V_d (+ i M+N)))
                    ((vd i x) (vector-set! V_d (+ i M+N) x))))
                 (vr
                  (syntax-rules ()
                    ((vr i) (vector-ref V_r (+ i M+N)))
                    ((vr i x) (vector-set! V_r (+ i M+N) x))))
                 (vl
                  (syntax-rules ()
                    ((vl i) (vector-ref V_l (+ i M+N)))
                    ((vl i x) (vector-set! V_l (+ i M+N) x)))))

      (define (finish)
        (let loop ((i (- M+N)) (maxl 0) (r '()))
          (cond ((> i M+N) (list maxl (reverse r)))
                ((> (vl i) maxl)
                 (loop (+ i 1) (vl i) (vr i)))
                (else
                 (loop (+ i 1) maxl r)))))

      (if (zero? M+N)
        '(0 ())
        (let d-loop ((d 0))
          (if (> d M+N)
            (error "lcs-with-positions; something's wrong (implementation error?)")
            (let k-loop ((k (- d)))
              (if (> k d)
                (d-loop (+ d 1))
                (receive (x l r)
                    (if (or (= k (- d))
                            (and (not (= k d))
                                 (< (vd (- k 1)) (vd (+ k 1)))))
                      (values (vd (+ k 1)) (vl (+ k 1)) (vr (+ k 1)))
                      (values (+ (vd (- k 1)) 1) (vl (- k 1)) (vr (- k 1))))
                  (receive (x y l r)
                      (let xy-loop ((x x) (y (- x k)) (l l) (r r))
                        (cond ((>= x N) (values x y l r))
                              ((>= y M) (values x y l r))
                              ((eq (vector-ref A x) (vector-ref B y))
                               (xy-loop (+ x 1) (+ y 1) (+ l 1)
                                        (cons (list (vector-ref A x) x y)
                                              r)))
                              (else (values x y l r))))
                    (vd k x)
                    (vr k r)
                    (vl k l)
                    (if (and (>= x N) (>= y M))
                      (finish)
                      (k-loop (+ k 2)))))))))))))
   
(define (lcs a b . opt-eq)
  (map car (cadr (lcs-with-positions a b (safe-car opt-eq equal?)))))

(define (lcs-fold a-only b-only both seed a b . opt-eq)
  (let ((common (cadr (lcs-with-positions a b (safe-car opt-eq equal?)))))
    (let loop ((common common) (seed seed)
               (a a) (a-pos 0) (b b) (b-pos 0))
      (if (null? common)
        (fold b-only (fold a-only seed a) b)
        (let* ((elt   (car common))
               (a-off (cadr elt))
               (a-skip (- a-off a-pos))
               (b-off (caddr elt))
               (b-skip (- b-off b-pos)))
          (receive (a-head a-tail) (split-at a a-skip)
            (receive (b-head b-tail) (split-at b b-skip)
              (loop (cdr common)
                    (both (car a-tail) (car b-tail)
                          (fold b-only (fold a-only seed a-head) b-head))
                    (cdr a-tail) (+ a-off 1) (cdr b-tail) (+ b-off 1)))))))))

(define (lcs-fold-matches both seed a b . opt-eq)
  (lcs-fold
   (lambda (n acc) acc)
   (lambda (n acc) acc)
   both seed a b opt-eq))


;; Comparisons

(define (quick-ratio a b)
  (if (and (not a) (not b))
      1
      (let* ((b-count (let ((counts (make-char-table)))
                        (string-foreach b
                          (lambda (c)
                            (table-set! counts c (+ (or (table-ref counts c) 0) 1))))
                        counts))
             (char-info (make-char-table))
             (matches (string-fold
                       (lambda (c acc)
                         (let ((num-left (or (table-ref char-info c)
                                             (or (table-ref b-count c) 0))))
                           (table-set! char-info c num-left)
                           (if (> num-left 0)
                               (+ acc 1)
                               acc)))
                       0 a)))
        (* 2 (/ matches (+ (string-length a) (string-length b)))))))


;; Matches

;; This is basically half of a zipper.
;; Interesting, should I be working
;; fully with zippers?
(define-record-type hole
  (make-hole node path)
  hole?
  (node hole-node)
  (path hole-path))

(define-record-type insertion
  (make-insertion hole)
  insertion?
  (hole insertion-hole))

(define-record-type deletion
  (make-deletion hole)
  deletion?
  (hole deletion-hole))

(define-record-type match
  (make-match hole-1 hole-2)
  match?
  (hole-1 match-hole-1)
  (hole-2 match-hole-2))

(define-record-discloser insertion
  (lambda (ins)
    `(ins ,(hole-path (insertion-hole ins)))))

(define-record-discloser deletion
  (lambda (del)
    `(del ,(hole-path (deletion-hole del)))))

(define-record-discloser match
  (lambda (m)
    `(match ,(hole-path (match-hole-1 m)) ,(hole-path (match-hole-2 m)))))

;; Change sets

;; Eventually, change-sets with
;; deletions and insertions won't exist.
;; Generation of the edit script is
;; a bit more trickier than this,
;; where deletions and insertions
;; have to be ordered properly.  This
;; will do for now.
(define-record-type change-set
  (make-change-set matches deletions insertions)
  change-set?
  (matches cs-matches cs-matches-set!)
  (deletions cs-deletions cs-deletions-set!)
  (insertions cs-insertions cs-insertions-set!))

(define-record-discloser change-set
  (lambda (cs)
    `(cs ,(cs-matches cs) ,(cs-deletions cs) ,(cs-insertions cs))))
  
(define (empty-change-set)
  (make-change-set '() '() '()))

(define (lcs->change-set+ cs a b match?)
  (lcs-fold
   (lambda (n cs)
     (cs-deletions-set! cs (cons (make-insertion n)
                                 (cs-deletions cs)))
     cs)
   (lambda (n cs)
     (cs-insertions-set! cs (cons (make-deletion n)
                                  (cs-insertions cs)))
     cs)
   (lambda (n1 n2 cs)
     (cs-matches-set! cs (cons (make-match n1 n2)
                               (cs-matches cs)))
     cs)
   cs a b match?))

(define (lcs->change-set a b match?)
  (lcs->change-set+ (empty-change-set) a b match?))


;; Higher-order zipper

(define-record-type bulky-zipper
  (make-bulky-zipper curr-node k path)
  bulky-zipper?
  (curr-node bz-node)
  (k bz-k)
  (path bz-path))

(define (bulky-zip-next z el)
  ((bz-k z) el))

(define (bulky-zip-fold kons seed zippa)
  (let loop ((z zippa)
             (acc seed))
    (if (bulky-zipper? z)
        (loop (bulky-zip-next z #f) (kons z acc))
        acc)))

(define (bulky-zipper->hole z)
  (make-hole (bz-node z) (bz-path z)))

(define (zip-bulkily tree traverse)
  (reset (traverse (lambda (tree path) (shift k (make-bulky-zipper tree k path))) tree)))

(define (bulky-zip-to-top zipper)
  (if (not (bulky-zipper? zipper))
      zipper
      (bulky-zip-to-top ((bz-k zipper) (bz-node zipper)))))

(define (tree->list tree traverse . skip?)
  (let ((filter (safe-car skip? identity)))
	(bulky-zip-fold
	 (lambda (z acc)
	   (if (filter (bz-node z))
		   acc
		   (cons (bulky-zipper->hole z) acc)))
	 '() (zip-bulkily tree traverse))))

(define (trees->change-set tree1 tree2 traverse root? match?)
  (let ((tree->list-no-root (lambda (tree)
                              (tree->list tree traverse root?)))
        (root-matching (make-match
                        (make-hole tree1 '())
                        (make-hole tree2 '()))))
    (lcs->change-set+ (make-change-set root-matching '() '())
                      (tree->list-no-root tree1)
                      (tree->list-no-root tree2)
                      (lambda (h1 h2)
                        (match? (hole-node h1) (hole-node h2))))))

;; Conflicts

(define-record-type conflict-node
  (make-conflict-node left-action right-action)
  conflict-node?
  (left-action conflict-left-action)
  (right-action conflict-right-action))

(define (remove-match cs m)
  (make-change-set
   (filter (lambda (n) (not (eq? n m))) (cs-matches cs))
   (cs-deletions cs)
   (cs-insertions cs)))

(define (%delete-modify-conflict working-change-set change-set1 change-set2 match?)
  (lcs-fold-matches
   (lambda (n1 n2 cs)
	 (make-change-set
	  (remove-match cs n2)
	  (cs-deletions cs)
	  (cons (make-conflict-node n1 n2) (cs-insertions cs))))
   (cs-deletions change-set1) (cs-matches change-set2) working-change-set
   (lambda (del m)
	 (match? (hole-node (deletion-hole del))
			 (hole-node (match-hole-1 m))))))

(define (conflict/delete-modify working-change-set change-set1 change-set2 match?)
  (%delete-modify-conflict working-change-set change-set1 change-set2 match?))

(define (conflict/modify-delete working-change-set change-set1 change-set2 match?)
  (%delete-modify-conflict working-change-set change-set2 change-set1 match?))

;; TODO: Merging data (binary tree merge, fuzzy merging, etc.)

;; Applying

;; TODO: need to wait for directional zippers
(define (apply-modifications cs tree query)
  (begin))

(define (apply-insertions cs tree query)
  (begin))

(define (apply-deletions cs tree query)
  (fold (lambda (el acc)
		  (let* ((path (drop-right (hole-path el) 1))
				 (node (query path)))
			(begin)))
		tree (cs-deletions cs)))

(define (apply-change-set cs tree query)
  (let ((del (cut apply-deletions <> <> query))
		(ins (cut apply-insertions <> <> query))
		(mod (cut apply-modifications <> <> query)))
	(mod (ins (del cs tree)))))


;;; Use case ----------------


(define (identity x) x)

;; Data structure
(define-record-type node
  (really-make-node tag data nodes)
  node?
  (tag node-tag)
  (data node-data)
  (nodes node-children))

(define-record-discloser node
  (lambda (node)
    `(n ,(node-tag node) ,(node-data node) ,(node-children node))))

(define *gensym-counter* -1)

(define (bad-gensym)
  (set! *gensym-counter* (+ *gensym-counter* 1))
  (string->symbol (string-append "g" (number->string *gensym-counter*))))

(define (make-root)
  (really-make-node 'root -1 '()))

(define (root? node)
  (eq? (node-tag node) 'root))

(define (make-node data children)
  (really-make-node (bad-gensym) data children))

(define (replace-node node data children)
  (really-make-node (node-tag node) data children))

(define (add-child node child)
  (really-make-node (node-tag node) (node-data node) (cons child (node-children node))))
      
(define (match? n1 n2)
  (eq? (node-tag n1) (node-tag n2)))

(define (path-element node)
  (node-tag node))

;; Make a tree

(define (string-repeat str n)
  (let loop ((i 0))
    (if (< i n)
        (begin
          (display str)
          (string-repeat str (- n 1))))))

(define (pp-tree tree depth)
  (string-repeat " " depth)
  (display (node-tag tree))
  (display " ")
  (display (node-data tree))
  (if (not (null? (node-children tree)))
      (begin
        (display "   \\")
        (newline)
        (for-each (lambda (n) (pp-tree n (+ depth 1))) (node-children tree)))
      (begin
        (display "   |")
        (newline))))

(define (pp-root tree)
  (pp-tree tree 0))

(define (make-test-tree size)
  (let ((random (make-random 19)))
    (let loop ((i 1)
               (tree (make-root)))
      (let ((rand (modulo (random) i)))
        (if (<= i size)
            (let* ((zippa (locate-nth-node tree rand))
                   (node (bz-node zippa))
                   (new-node (make-node (modulo (random) 5000) '())))
              (loop (+ i 1) (bulky-zip-to-top ((bz-k zippa) (add-child node new-node)))))
            tree)))))

(define (diff-test-trees t1 t2)
  (trees->change-set t1 t2 depth-first* root? match?))

(define (locate-nth-node tree n)
  (let ((cursor (zip-bulkily tree depth-first*)))
    (let loop ((cursor cursor)
               (i 0))
      (if (bulky-zipper? cursor)
          (if (= i n)
              cursor
              (loop ((bz-k cursor) #f) (+ i 1)))
          (error "too few nodes")))))

(define (map* f l)
  (if (null? l) l
    (let ((h (car l)) (t (cdr l)))
      (let ((h1 (f h)) (t1 (map* f t)))
        (if (and (eq? h1 h) (eq? t1 t)) l
            (cons h1 t1))))))

;: TODO: refactor this to use directional zippers
;; so we don't have to derive the travseral proc
(define (depth-first* handle tree . ppath)
  (let ((cur-path (safe-car ppath '())))
    (cond ((handle tree cur-path) => identity)
          ((null? (node-children tree)) tree)
          (else
           (let ((mapped (map* (lambda (kid)
                                 (depth-first* handle kid (append cur-path (list (node-tag kid)))))
                               (node-children tree))))
             (if (eq? mapped (node-children tree))
                 tree
                 (replace-node tree (node-data tree) mapped)))))))


;; Util ----------------

(define (make-char-table)
  (make-table))

(define (string-foreach str f)
  (let ((l (string-length str)))
    (let loop ((i 0))
      (if (< i l)
          (begin
            (f (string-ref str i))
            (loop (+ i 1)))))))

(define-syntax define-condition
  (syntax-rules ()
    ((_ name super)
     (begin
       (define-condition-type 'name 'super)
       (define (name . stuff)
         (apply signal (cons 'name stuff)))))))

(define-syntax force-condition
  (syntax-rules ()
    ((_ type stuff test)
     (if (not test)
         (type stuff)))))

(define-syntax force-conditions
  (syntax-rules ()
    ((_ type prefix)
     (if #f #f))
    ((_ type prefix (test expr ...) next-test ...)
     (if (not test)
         (type (string-append prefix expr ...))
         (force-conditions type prefix next-test ...)))))

(define-syntax receive
  (syntax-rules ()
    ((_ vars producer expr ...)
     (call-with-values
       (lambda ()
         producer)
       (lambda vars
         expr ...)))))

(define (safe-car lst . def)
  (if (and lst (not (null? lst)))
	  (car lst)
      (if def (car def)
          #f)))

;;;; Scanned Graph
;;;;
;;;; A scanned graph is a "shadow" over the persisted graph.  It scans
;;;; the source code, sets up interfaces and structures, and can be
;;;; used to enforce access control.  This graph format is useful for
;;;; a system-level "plist-style" editor.
;;;;
;;;; It is called "scanned" because the code in each node is not
;;;; evaluated, so this tree format is still in a partially passive
;;;; state.  See persisted-graph.

(define-condition
  scan-error (syntax-error)
  scan-error?)

(define graph-error source:graph-error)
(define graph-error? source:graph-error?)

(define (scan g)
  (cond ((source:graph? g) (scan-edge g))
        ((source:children? g) (scan-children g))
        ((source:child? g) (scan-child g))
        (else g)))

;;;; Node

;;; A source node is scanned in this way:
;;;   * A shallow scan is passed over its code to return a
;;;     "plist-style" mapping of top-level definitions to their forms.
;;;   * The code is evaluated into a package and structures are
;;;     produced corresponding to the interfaces implemented by the
;;;     node.

(define-syntax define-forcers
  (syntax-rules ()
    ((_ (name accessor) ...)
     (begin
       (define (name obj) (force (accessor obj)))
       ...))))

;; --------------------
;; constructors

(define-record-type :node
  (make-scanned-node source type forms children structures)
  node?
  (source source-node)
  (type scanned-type-promise)
  (forms scanned-forms-promise)
  (children scanned-children-promise)
  (structures scanned-structures-promise))

(define-record-discloser :node
  (lambda (node)
    `(node)))

(define-forcers
  (scanned-type scanned-type-promise)
  (scanned-forms scanned-forms-promise)
  (scanned-node-children scanned-children-promise)
  (scanned-structures scanned-structures-promise))

(define (scan-node node)
  (if (node? node)
      node
      (let ((implements code opens for-children children (unrecord node source::node implements code opens for-children children)))
        (make-scanned-node node
                           (delay (environment-ref (interaction-environment) (car implements)))
                           (delay (let-condition scan-error (shallow-scan code)))
                           ;; FIXME: probably set up dynamic evaluation environment here
                           (delay (scan-children children))
                           (delay (source->structures implements for-children opens code))))))

(define (source->structures interfaces child-interface open form)
  'TODO)

;; --------------------
;; updaters

(define (replace-node-children scanned-node children)
  (if (eq? (scanned-node-children scanned-node) children)
      scanned-node
      (let ((children (scan-children children)))
        (make-scanned-node (source:replace-node-children (source-node scanned-node)
                                                         (source-children children))
                           (scanned-type-promise scanned-node)
                           (scanned-forms-promise scanned-node)
                           (delay children)
                           (scanned-structures-promise scanned-node)))))

;;;; Edge

;;; An edge is scanned this way:
;;;   * The TO slot is made into a (delay (scan-node to))
;;;   * The ACCESS slot is compiled into a predicate procedure
;;;
;;; If the "graph" ultimately remains a tree, rtd/scanned-edge and
;;; rtd/scanned-node could be combined (probably if the source edge
;;; and node types were also combined).

;; --------------------
;; Constructors

(define-record-type :edge
  (make-scanned-edge source name to access)
  edge?
  (source source-edge)
  (name scanned-name)
  (to scanned-to-promise)
  (access access-predicate))

(define-record-discloser :edge
  (lambda (edge)
    `(edge ,(scanned-name edge))))

(define-forcers
  (scanned-to scanned-to-promise))

(define (scan-edge edge)
  (if (edge? edge)
      edge
      (let ((name to access (unrecord edge source::edge name to access)))
        (new-edge edge name to access))))

(define (new-edge source name node access)
  (make-scanned-edge source
                     name
                     (delay (scan-node node))
                     (compile-access access)))

;; --------------------
;; Accessors

(define (scanned-edge-children edge)
  (scanned-node-children (scanned-to edge)))

;; --------------------
;; Access Control

(define (compile-access access)
  ;; TODO
  (lambda x #t))

(define (has-access? scanned-edge . rest)
  (apply (access-predicate scanned-edge) rest))

;; --------------------
;; Mutators

(define (rename edge new-name)
  (if (eq? new-name (scanned-name edge))
      edge
      (make-scanned-edge (source:rename (source-edge edge) new-name)
                         new-name
                         (scanned-to-promise edge)
                         (access-predicate edge))))

(define (replace-node edge node)
  (if (eq? node (scanned-to edge))
      edge
      (let ((node (scan-node node)))
        (make-scanned-edge (source:replace-node (source-edge edge)
                                                (source-node node))
                           (scanned-name edge)
                           (delay node)
                           (access-predicate edge)))))

;;;; Child List

;; --------------------
;; Constructors

(define-record-type :child
  (make-child name source this next)
  child?
  (name child-name)
  (source source-child)
  (this child->graph)
  (next next-child-promise))

(define *null-child*
  (make-child #f '() #f '()))

(define-forcers
  (next-child next-child-promise))

(define-record-type :children
  (make-children source scanned)
  children?
  (source source-children)
  (scanned scanned-children))

(define (null-children? children)
  (end-of-children? (scanned-children children)))

(define (child-list . children)
  (scan-children (map-in-order source-child children)))

(define (scan-children source-children)
  (if (children? source-children)
      source-children
      (make-children source-children
                     (scan-child (first-child source-children)))))

(define (first-child source-children)
  (call-with-current-continuation
   (lambda (return)
     (source:share-children return source-children))))

(define (scan-child source)
  (cond ((child? source) source)
        ((or (source:end-of-children? source)
             (end-of-children? source))
         *null-child*)        
        (else
         (make-child (source:child-name source)
                     source
                     (scan-edge (source:child->graph source))
                     (delay (scan-child (source:next-child source)))))))

(define (add-child g children)
  (let ((g (scan-edge g))
        (children (scan-children children)))
    (let ((source (source:add-child (source-edge g) (source-children children))))
      (make-children source
                     (make-child (scanned-name g)
                                 (first-child source)
                                 g
                                 (delay (scanned-children children)))))))

(define (end-of-children? foo)
  (eq? foo *null-child*))

(define (fold-children proc seed children)
  (let loop ((c (scanned-children children)))
    (if (end-of-children? c)
        seed
        (proc c (loop (next-child c))))))

(define (share-children proc children)
  (share children
         scanned-children
         proc
         (lambda (c)
           (let ((c (scan-child c)))
             (make-children
              (source:share-children (lambda (ignore) (source-child c))
                                     (source-children children))
              c)))))

(define (share-child proc child)
  (share child
         un-child
         proc
         (new-child-from child)))

(define (un-child c)
  (values (child->graph c) (next-child c)))

(define (new-child-from child)
  (lambda (this next)
    (let ((this (scan-edge this))
          (next (scan-child next)))
      (make-child (scanned-name this)
                  (source:share-child (lambda (s-this s-next)
                                        (values (source-edge this)
                                                (source-child next)))
                                      (source-child child))
                  this
                  (delay next)))))

;;;; Graph

(define-syntax root
  (syntax-rules ()
    ((_ args ...)
     (scan-edge (source:root args ...)))))

(define-syntax node
  (syntax-rules ()
    ((_ args ...) (source:node args ...))))

(define-syntax edge
  (syntax-rules ()
    ((_ args ...) (source:edge args ...))))

(define-syntax children
  (syntax-rules ()
    ((_ args ...) (source:children args ...))))

(define graph? edge?)
(define-simple-type :graph (:edge) graph?)

(define (root? foo)
  (and (graph? foo)
       (not (graph-name foo))))

(define (leaf? foo)
  (and (graph? foo)
       (null-children? (graph-children foo))))

(define graph-name scanned-name)

(define graph-edge identity)

(define graph-node scanned-to)

(define (graph-type g)
  (scanned-type (graph-node g)))

(define (graph-children (g :graph))
  (scanned-node-children (scanned-to g)))

(define (graph-forms g)
  (scanned-forms (scanned-to g)))

(define (graph-structures g)
  (scanned-structures (scanned-to g)))

(define (replace-children g children)
  (replace-node g (replace-node-children (graph-node g) children)))

;;;; Tests
(begin
  (define-record-type/primitive :foo a b)

  (let* ((tree (root (source:node :foo
                                  (plist (a 1) (b 2))
                                  (children ('c1 (source:node :foo (plist (a 3) (b 4))))
                                            ('c2 (source:node :foo (plist (a 5) (b 6)))))))))

    (assert (root? tree))
    (assert (source:graph? (source-edge tree)))

    (let ((node (scanned-to tree)))
      (assert (node? node))
      (assert (scanned-forms node) => '((a . 1) (b . 2)))
      

      (let ((kids (scanned-node-children node)))
        (share-children
         (lambda (c)
           (assert (child? c))
           (assert (edge? (child->graph c)))
           (assert (child? (next-child c)))
           (assert (end-of-children? (next-child (next-child c))))
           c)
         kids)

        ;; sharing is preserved
        (assert (eq? (share-children identity kids) kids))
        (share-children
         (lambda (c)
           (assert (eq? (share-child (lambda (t n) (values t n)) c) c))
           c)
         kids)

        ;; maximal sharing is preserved
        (let ((mod (add-child (edge 'c0 (source:node :foo (plist (a 1) (b 2))))
                              kids)))
          (share-children
           (lambda (c)
             (share-children
              (lambda (orig)
                (assert (eq? orig (next-child c)))
                (assert (eq? (source-child orig)
                             (source-child (next-child c))))
                orig)
              kids)
             c)
           mod))))

    (assert (leaf? (replace-children tree (children))))
    (assert (eq? tree (replace-children tree (graph-children tree))))
    ))
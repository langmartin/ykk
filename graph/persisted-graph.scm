;;;; Persisted Graph
;;;;
;;;; This graph is essentially an annotated source tree.  It's really
;;;; only useful when it's been scanned or compiled.  See
;;;; scanned-graph and compiled-graph.

(define-condition
  graph-error (error)
  graph-error?)


;;;; Child List

;; define this here so :CHILDREN is available
(syntax/eval
 (define-tagged-list-data :children
   really-cons-child
   child-list
   children?
   null-children?
   children->list
   list->children))

;;;; Node

;; --------------------
;; Constructors

(define-record-type/primitive :node
  nongenerative: uE444623E-520A-4E26-9855-F1FA44BE9963
  (implements   (type :sexpr))
  (code         (type :code-block))
  (opens        (type :maybe-sexpr))
  (for-children (type :maybe-sexpr))
  (children     (type :children)))

(define-syntax unnode
  (syntax-rules ()
    ((_ n . names)
     (unrecord n :node . names))))

(define-syntax new-node
  (syntax-rules ()
    ((_ (implements ...) code opens for-children children)
     (make-node '(implements ...) 'code 'opens 'for-children children))
    ((_ implements code opens for-children children)
     (new-node (implements) code opens for-children children))))

;; --------------------
;; Mutators

(define (replace-node-children node children)
  (sharing-record-update node :node (children children)))

;;;; Edge

;; --------------------
;; Constructors

(define-record-type/primitive :edge
  nongenerative: u8630EBA8-F6E3-441C-95DA-769466AAD692
  (name (type :symbol))
  (to   (type :node))
  ;; FIXME: the entire concept of access-control is undefined
  (access))

(define-syntax unedge
  (syntax-rules ()
    ((_ e . names)
     (unrecord e :edge . names))))

;; --------------------
;; Accessors

(define graph-node edge-to)

(define edge-node edge-to)

(define (graph-type g)
  (car (node-implements (edge-node g))))

;; --------------------
;; Mutators

(define (rename edge new-name)
  (sharing-record-update edge :edge (name new-name)))

(define (replace-node edge new-node)
  (sharing-record-update edge :edge (to new-node)))

;;;; Public Interface

;; --------------------
;; Nodes / Edges

(define-syntax root
  (syntax-rules ()
    ((_ node-def)
     (edge #f node-def))))

(define-syntax edge
  (syntax-rules ()
    ((_ name to)
     (edge name to #f))
    ((_ name to access)
     (make-edge name to access))))

(define-syntax node
  (syntax-rules (open)
    ((_ implements (open arg ...) code rest ...)
     (node-def-keyword (implements code (arg ...) #f (child-list)) (rest ...)))
    ((_ implements code rest ...)
     (node-def-keyword (implements code #f #f (child-list)) (rest ...)))))

(define graph? edge?)

(define-simple-type :graph (:edge) graph?)

(define (root? g)
  (and (graph? g) (not (graph-name g))))

(define (leaf? g)
  (and (graph? g) (null-children? (graph-children g))))

(define graph-edge identity)

(define graph-name edge-name)

(define (graph-children (g :graph))
  (node-children (graph-node g)))

;; --------------------
;; Children

(define-syntax children
  (syntax-rules ()
    ((_ (name node-def) ...)
     (child-list (edge name node-def) ...))))

(define child identity)

(define child? pair?)

(define-simple-type :child (:pair) child?)

(define child->graph car)

(define next-child cdr)

(define (child-name c)
  (graph-name (child->graph c)))

(define (add-child (c :graph) (children :children))
  (really-cons-child c children))

(define (replace-children (g :graph) (children :children))
  (replace-node g (replace-node-children (graph-node g) children)))

;;;; Traversal
(define (share-children proc children)
  (share children
         children->list
         proc
         list->children))

(define end-of-children? null?)

(define new-child cons)

(define un-child uncons)

(define (share-child proc child)
  (share child
         un-child
         proc
         new-child))

(define (fold-children proc seed children)
  (pair-fold-right proc seed (children->list children)))

;;;; Internal
(define (child-name-exists? g name)
  (any (child-name? name)
       (graph-children->list g)))

(define (child-name? name)
  (lambda (c) (eq? (child-name c) name)))

(define (graph-children->list g)
  (children->list (graph-children g)))

(define-syntax node-def-keyword
  (syntax-rules (open for-children)
    ((_ (args ...) ())
     (new-node args ...))

    ((_ (?implements ?code ?opens ?for-children ?children)
        ((open arg ...) etc ...))
     (node-def-keyword (?implements ?code (arg ...) ?for-children ?children)
                      (etc ...)))

    ((_ (?implements ?code ?opens ?for-children ?children)
        ((for-children expr) etc ...))
     (node-def-keyword (?implements ?code ?opens expr ?children)
                      (etc ...)))

    ((_ (?implements ?code ?opens ?for-children ?children)
        (make-children))
     (node-def-keyword (?implements ?code ?opens ?for-children make-children)
                       ()))))

;;;; Tests
(begin

  (define-record-type/primitive :foo
    (a (type :symbol))
    (b (type :symbol)))

  (define-record-type/primitive :bar
    (d (type :number))
    (e (type :number)))

  (define-record-type/primitive :baz
    (f (type :symbol))
    (g (type :value)))

  ;; --------------------
  ;; construction

  (let ((n (new-node :foo (plist (a 1) (b 2)) (scheme) (export a) (children))))    
    (assert (node? n))
    (assert (edge? (edge 'some-name n))))

  ;; --------------------
  ;; graph language
  (let ((n (node :foo (open ykk (parent)) (begin foo bar))))    
    (assert (node? n))
    (assert (root? (root n)))
    (let ((implements code opens for-children children (unrecord n :node implements code opens for-children children)))
      (assert implements => '(:foo))
      (assert code => '(begin foo bar))
      (assert opens => '(ykk (parent)))
      (assert for-children => #f)
      (assert (children->list children) => '())))

  ;; A tiny, example tree with two children
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

  (assert (root? *top*))

  (let ((name to (unrecord *top* :edge name to)))    
    (assert name => #f)
    (assert (node? to))

    (let ((children for-children (unrecord to :node children for-children)))
      (let ((c1 (car (children->list children))))      
        (assert (edge? c1))
        (let ((name to access (unrecord c1 :edge name to access)))                    
          (assert name => 'c1)
          (assert (node? to))

          (let ((opens code children (unrecord to :node opens code children)))                        
            (assert opens => #f)
            (assert code => '(plist (d 1) (e (adjust d))))
            (assert (null? (children->list children))))))))  
  
  (let ((n (replace-node-children (node :foo (begin a b)) (child-list 'alpha 'beta))))
    (let ((code children (unrecord n :node code children)))            
      (assert code => '(begin a b))
      (assert children => '(child-list alpha beta)))

    (let ((r2 (replace-node *top* n)))
      (assert (edge-to r2) => n))))
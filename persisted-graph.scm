;;;; Persisted Graph
;;;;
;;;; This graph is essentially an annotated source tree.  It's really
;;;; only useful when it's been scanned or compiled.  See
;;;; scanned-graph and compiled-graph.

(define-condition
  graph-error (error)
  graph-error?)


;;;; Simple types needed for compound type definitions below
(define (symbol-or-false? obj)
  (or (symbol? obj)
      (boolean? obj)))
(define-simple-type :name (:symbol :boolean) symbol-or-false?)

(define (pair-or-null? foo)
  (or (null? foo) (pair? foo)))

(define (maybe-undefined-sexpr? foo)
  (or (not foo) (sexpr? foo)))
(define-simple-type :maybe-undefined-sexpr () maybe-undefined-sexpr?)

(define sexpr? pair-or-null?)
(define-simple-type :sexpr (:maybe-undefined-sexpr) sexpr?)

(define (code-block? code)
  (and (sexpr? code)
       (memq (car code) '(begin plist))))
(define-simple-type :code-block (:sexpr) code-block?)

;;;; Child List

;; define this here so :CHILDREN is available

(define-tagged-list-data :children
  really-cons-child
  child-list
  children?
  null-children?
  children->list
  list->children)

;;;; Node

;; --------------------
;; Constructors

(define-type :node ()
  node?
  (implements :sexpr)
  (code :code-block)
  (opens :maybe-undefined-sexpr)
  (for-children :maybe-undefined-sexpr)
  (children :children))

(define-syntax new-node
  (syntax-rules ()
    ((_ (implements ...) code opens for-children children)
     (new :node '(implements ...) 'code 'opens 'for-children children))
    ((_ implements code opens for-children children)
     (new-node (implements) code opens for-children children))))

;; --------------------
;; Accessors

(define (graph-node g)
  (unstructure g to))

;; --------------------
;; Mutators

(define-updater (replace-node-children node children)
  (node :node)
  (children children))

;;;; Edge

;; --------------------
;; Constructors

(define-type :edge ()
  edge?
  (name :name)
  (to :node)
  ;; FIXME: the entire concept of access-control is undefined
  (access))

;; --------------------
;; Accessors

(define (edge-name e)
  (unstructure e name))

(define (edge-node e)
  (unstructure e to))

;; --------------------
;; Mutators

(define-updater (rename edge new-name)
  (edge :edge) (name new-name))

(define-updater (replace-node edge new-node)
  (edge :edge) (to new-node))

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
     (new :edge name to access))))

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

(define (graph-name (g :graph))
  (unstructure g name))

(define (graph-children (g :graph))
  (unstructure g (to children)))

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

  (define-type :foo ()
    (a :symbol)
    (b :symbol))

  (define-type :bar ()
    (d :number)
    (e :number))

  (define-type :baz ()
    (f :symbol)
    (g :value))

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
    (let ((implements code opens for-children children (unstructure n)))          
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

  (let ((name to (unstructure *top* name to)))    
    (assert name => #f)
    (assert (node? to))

    (let ((children for-children (unstructure to children for-children)))
      (let ((c1 (car (children->list children))))      
        (assert (edge? c1))
        (let ((name to access (unstructure c1)))                    
          (assert name => 'c1)
          (assert (node? to))

          (let ((opens code children (unstructure to opens code children)))                        
            (assert opens => #f)
            (assert code => '(plist (d 1) (e (adjust d))))
            (assert (null? (children->list children))))))))

  (let ((n (replace-node-children (node :foo (begin a b)) (child-list 'alpha 'beta))))
    (let ((code children (unstructure n code children)))            
      (assert code => '(begin a b))
      (assert children => '(child-list alpha beta)))

    (let ((r2 (replace-node *top* n)))
      (assert (unstructure r2 to) => n))))
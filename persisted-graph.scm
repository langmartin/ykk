;;;; Persisted Graph
;;;;
;;;; This graph is essentially an annotated source tree.  It's really
;;;; only useful when it's been scanned or compiled.  See
;;;; scanned-graph and compiled-graph.

;;;; graph language (sugar)
(define-syntax define-root
  (syntax-rules ()
    ((_ name node-def)
     (define name (root node-def)))))

(define-syntax root
  (syntax-rules ()
    ((_ node-def)
     (new-edge #f node-def))))

(define (root? tree)
  (with-destructured
   ((tree name))
   (not name)))

(define-syntax node
  (syntax-rules (open)
    ((_ implements (open arg ...) code rest ...)
     (node-def-keyword (implements code (arg ...) #f '()) (rest ...)))
    ((_ implements code rest ...)
     (node-def-keyword (implements code #f #f '()) (rest ...)))))

;; --------------------
;; Internal

(define-syntax node-def-keyword
  (syntax-rules (open for-children children)
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
        ((children c ...) etc ...))
     (node-def-keyword (?implements ?code ?opens ?for-children (expand-children c ...))
                      (etc ...)))))

(define-syntax expand-children
  (syntax-rules ()
    ((_ "make" (name node-def))
     (new-edge name node-def))
    ((_ c ...)
     (edge-list (expand-children "make" c) ...))))

;;;; some simple types

(define (pair-or-null? foo)
  (or (null? foo) (pair? foo)))
(define edge-list? pair-or-null?)
(define-simple-type :edge-list () edge-list?)

(define (maybe-undefined-sexpr? foo)
  (or (not foo) (sexpr? foo)))
(define-simple-type :maybe-undefined-sexpr () maybe-undefined-sexpr?)

(define sexpr? pair-or-null?)
(define-simple-type :sexpr (:maybe-undefined-sexpr) sexpr?)

(define (code-block? code)
  (and (sexpr? code)
       (memq (car code) '(begin plist))))
(define-simple-type :code-block (:sexpr) code-block?)

;;;; Edge List
(define edge-list list)

(define (fold-edge-list proc seed list)
  (fold-right (lambda (edge acc)
                (with-destructured
                 ((edge name))
                 (proc name edge acc)))
              seed
              list))

;;;; Node

;; --------------------
;; constructors

(define-type :node ()
  node?
  (implements :sexpr)
  (code :code-block)
  (opens :maybe-undefined-sexpr)
  (for-children :maybe-undefined-sexpr)
  (children :edge-list))

(define-syntax new-node
  (syntax-rules ()
    ((_ (implements ...) code opens for-children children)
     (new :node '(implements ...) 'code 'opens 'for-children children))
    ((_ implements code opens for-children children)
     (new-node (implements) code opens for-children children))))

;; --------------------
;; updaters

(define-updater (replace-children node children)
  (node :node)
  (children children))


;;;; Edge

;; --------------------
;; Source constructors

(define (symbol-or-false? obj)
  (or (symbol? obj)
      (boolean? obj)))

(define-simple-type :name (:symbol :boolean) symbol-or-false?)

(define-type :edge ()
  edge?
  (name :name)
  (to :node)
  ;; FIXME: the entire concept of access-control is undefined
  (access))

(define-syntax new-edge
  (syntax-rules ()
    ((_ name to)
     (new-edge name to #f))
    ((_ name to access)
     (new :edge 'name to access))))

;; --------------------
;; Source updaters

(define-updater (replace-node edge new-node)
  (edge :edge) (to new-node))

;;;; tests

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

  (let ((n (new-node :foo (plist (a 1) (b 2)) (scheme) (export a) '())))
    (assert (node? n))
    (assert (edge? (new-edge some-name n))))

  ;; --------------------
  ;; graph language

  (let ((n (node :foo (open ykk (parent)) (begin foo bar))))
    (assert (node? n))
    (assert (root? (root n)))
    (with-destructured ((n implements code opens for-children children))
     (assert implements => '(:foo))
     (assert code => '(begin foo bar))
     (assert opens => '(ykk (parent)))
     (assert for-children => #f)
     (assert children => '())))

  ;; A tiny, example tree with two children
  (define-root *top*
    (node :foo
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
          (children (c1 (node :bar (plist (d 1) (e (adjust d)))))
                    (c2 (node :baz (plist (f 'f-value) (g "g value")))))))

  (assert (root? *top*))

  (with-destructured ((*top* name to))
   (assert name => #f)
   (assert (node? to))

   (with-destructured ((to children for-children))
    (assert (edge-list? children))
    (assert for-children => '(compound-interface (parent-interface) (export adjust)))

    (let ((c1 (car children)))
      (assert (edge? c1))
      (with-destructured ((c1 name to))
       (assert name => 'c1)
       (assert (node? to))

       (with-destructured
        ((to opens code children))
        (assert opens => #f)
        (assert code => '(plist (d 1) (e (adjust d))))
        (assert (null? children)))))))

  (let ((n (replace-children (node :foo (begin a b)) '(alpha beta))))
    (with-destructured
     ((n code children))
     (assert code => '(begin a b))
     (assert children => '(alpha beta)))

    (let ((r2 (replace-node *top* n)))
      (with-destructured
       ((r2 to))
       (assert to => n)))))
(define (leaf? tree)
  (not (and tree            
            (or (left tree)
                (right tree)))))

(define (red/black-tree tree= tree<)
  (make-tree tree= tree< (make-lookup tree= tree<) #f))

(define (empty-tree? tree)
  (not (root tree)))

(define (color node)
  (and node (if (red? node) red black)))

(define (make-lookup tree= tree<)
  (lambda (node target-value terminate found go-left go-right)      
    (let descend ((node node))    
      (cond ((not node)
             (terminate))
            ((tree= target-value (value node))
             (found node))              
            ((tree< target-value (value node))
             (go-left node descend #t))              
            (else
             (go-right node descend #f))))))

(define-syntax define-tree-lookup
  (syntax-rules ()
    ((_ (descend tree node-name target)
        terminate found go-left go-right)
     (define descend
       (let ((tree= (tree= tree))
             (tree< (tree< tree)))
         (lambda (node-name)             
           (cond ((not node-name)
                  terminate)
                 ((tree= target (value node-name))
                  found)
                 ((tree< target (value node-name))
                  go-left)
                 (else
                  go-right))))))))

; --------------------
; Pre-cooked types

(define (number-tree)
  (make-tree eq? < #f))

(define (string-tree)
  (make-tree string=? string<? #f))

(define (symbol-tree)
  (make-tree eq? symbol< #f))

(define (symbol< a b)
  (string<? (symbol->string a)
            (symbol->string b)))

; --------------------
; Helpful definitions

(define (leaf color value)
  (color #f #f value))

(define (change-left node color left)
  (color left (right node) (value node)))

(define (change-right node color right)
  (color (left node) right (value node)))

(define (change-value node value)
  ((color node) (left node) (right node) value))

(define (change-color new-color node)
  (if (eq? (color node) new-color)
      node
      (new-color (left node)
                 (right node)
                 (value node))))

(define (graft node left right)
  ((color node) left right (value node)))

(define (graft/recolor node color left right)
  (color left right (value node)))

(define (new-root tree root)
  (make-tree (tree= tree)
             (tree< tree)
             root))

; --------------------
; search

(define (tree-ref tree target-value . default)
  
  (define-tree-lookup (descend tree node target-value)
    (and (not (null? default)) (car default))
    (value node)
    (descend (left node))
    (descend (right node)))

  (descend (root tree)))

; --------------------
; insertion

(define (insert tree value)  
  (new-root tree
            (if (empty-tree? tree)
                (leaf black value)
                (change-color black
                              (insert-one tree value change-value)))))

(define (maybe-replace tree new-value maybe?)
  (call-with-current-continuation
   (lambda (exit)
     (new-root tree
               (if (empty-tree? tree)
                   (leaf black new-value)
                   (change-color black
                                 (insert-one tree
                                             new-value                                             
                                             (lambda (node new-value)
                                               (if (maybe? (value node))
                                                   (change-value node new-value)
                                                   (exit tree))))))))))

(define (insert-one tree new-value change-value)
  
  (define-tree-lookup (descend tree node new-value)
    (leaf red new-value)
    (change-value node new-value)
    (insert-transform node (descend (left node)) (right node))
    (insert-transform node (left node) (descend (right node))))
  
  (descend (root tree)))

(define (bal x y z a b c d)
  (red (black a b (value x))
       (black c d (value z))
       (value y)))

(define (insert-transform node lc rc)
  (let ((red-lc (red? lc)))    
    (if (not (and (black? node)
                  (or red-lc (red? rc))))      
        (graft node lc rc)
        ; proceeding clockwise around Figure 1 in Okasaki's "Functional
        ; Pearls: Red-Black Trees in a Functional Setting"
        (let ((red-rc (red? rc))
              (l left)
              (r right))
          (cond ((and red-lc (red? (r lc)))
                 (bal lc (r lc) node (l lc) (l (r lc)) (r (r lc)) rc))
                ((and red-rc (red? (r rc)))
                 (bal node rc (r rc) lc (l rc) (l (r rc)) (r (r rc))))
                ((and red-rc (red? (l rc)))
                 (bal node (l rc) rc lc (l (l rc)) (r (l rc)) (r rc)))
                ((and red-lc (red? (l lc)))
                 (bal (l lc) lc node (l (l lc)) (r (l lc)) (r lc) rc))
                (else
                 (graft node lc rc)))))))

; --------------------
; deletion
;
; Modeled on the documentation for libavl deletion.

;; first, some syntax

;; continue to transform if necessary
(define-syntax apply-delete-transform
  (syntax-rules ()
    ((_ transform? changed out-dir)
     (if transform?
      (delete-transform changed out-dir)
      (values changed transform?)))))

;; at each branch on the way down the tree, graft in the new subtree
;; and possibly run a transformation.
(define-syntax transform-delete-branch
  (syntax-rules ()
    ((_ descend node out-dir)
     (receive (new-child transform?)
         (descend (out-dir node))      
       (let* ((change (if (eq? out-dir left) change-left change-right))
              (changed (change node (color node) new-child)))
         (apply-delete-transform transform? changed out-dir))))))

;; only initiate a transformation if the removed node was red
(define-syntax rebalance?
  (syntax-rules ()
    ((_ removed-color) 
     (not (eq? removed-color red)))))

;; possibly initiate transformation for DELETE-THIS cases 1 and 2.
(define-syntax maybe-transform
  (syntax-rules ()
    ((_ removed-color replacement out-dir)
     (if (not (rebalance? removed-color))
         ;; special case: when a red node is removed, the black height does
         ;; not change and no rebalancing needs to be done          
         (values replacement #f)
         (delete-transform replacement out-dir)))))

;; For case 3 in DELETE-THIS: remove in-order successor and
;; rebalance the right subtree
(define-syntax splice-out-successor
  (syntax-rules ()
    ((_ node)
     (letrec ((successor #f)
              (find (lambda (node)
                      (if (not (left node))
                          (begin                            
                            (set! successor node)
                            (values (right node)
                                    (rebalance? (color successor))))
                          (receive (new-subtree transform?)
                              (find (left node))
                            (apply-delete-transform
                             transform?
                             (change-left node
                                          (color node)
                                          new-subtree)
                             left))))))
    
       ;; first, find the in-order successor, possibly rebalancing the
       ;; right subtree on the way back up
       (receive (new-subtree transform?)
           (find (right node))
      
         ;; finally, splice out NODE, replacing it with SUCCESSOR and
         ;; recoloring
         (apply-delete-transform
          transform?
          (graft/recolor successor
                         (color node)
                         (left node)
                         new-subtree)
          right))))))

;; DELETE-THIS -- the target has been found, remove it
;;
;; three cases from
;; http://www.stanford.edu/~blp/avl/libavl.html/Deleting-an-RB-Node-Step-2-_002d-Delete.html
(define-syntax delete-this
  (syntax-rules ()
    ((_ node)     
     (let ((r (right node)))      
       (cond ((not r)
              (maybe-transform (color node) (left node) #f))        
             ((not (left r))
              (maybe-transform (color r)
                               (graft/recolor r (color node) (left node) (right r))
                               right))          
             (else
              (splice-out-successor node)))))))

(define g/r-left graft/recolor)
(define (g/r-right node color r l)
  (graft/recolor node color l r))

(define-syntax sibling
  (syntax-rules ()
    ((_ node dir)
     (and node ((if (eq? dir left) right left) node)))))

;; DELETE

(define (delete tree value)
  (if (empty-tree? tree)
      tree
      (call-with-current-continuation
       (lambda (k)
         (new-root tree
                   (change-color
                    black
                    (delete-one
                     tree value (lambda () (k tree)))))))))

(define rb-delete delete)

(define (delete-one tree target-value not-found)

  (define-tree-lookup (descend tree node target-value)
    (not-found)
    (delete-this node)
    (transform-delete-branch descend node left)
    (transform-delete-branch descend node right))    
  
  (receive (new-node transform?)
      (descend (root tree))
    new-node))
  
;; DELETE-TRANSFORM
;;
;; returns two values: NEW-SUBTREE and TRANSFORM?
(define (delete-transform node out-dir . testing)  
  (cond ((not (sibling node out-dir))
         (values node #t))
        ((eq? out-dir left)
         ;; left transform
         (transform node out-dir change-left change-right left right g/r-left (not (null? testing))))
        (else
         ;; right transform
         (transform node out-dir change-right change-left right left g/r-right (not (null? testing))))))

;; rebalancing cases from libavl:
;; http://www.stanford.edu/~blp/avl/libavl.html/Deleting-an-RB-Node-Step-3-_002d-Rebalance.html    
(define (transform node out-dir change-left change-right left right graft/recolor testing)
  (let ((w (sibling node out-dir))
        (x (out-dir node)))      
    (cond ((and testing
                (or (symbol? (left w)) (symbol? (right w))))
           (values node #t))
          ((red? x)
           ;; case: beginning of for-loop.  If the node is red,
           ;; change it to black and we're done balancing
           (values (change-left node (color node) (change-color black x))
                   #f))            
          ((red? w)             
           ;; case reduction: ensure w is black
           (receive (new-subtree transform?)
               (transform (change-right node red (left w)) left
                          change-left change-right left right graft/recolor testing)
             (let ((new-node (change-left w black new-subtree)))
               (if transform?
                   (transform new-node left
                              change-left change-right left right graft/recolor testing)
                   (values new-node #f)))))            
          ((not (or (red? (left w)) (red? (right w))))             
           ;; case 1: w has no red children
           (values (change-right node
                                 (color node)
                                 (change-color red w))
                   #t))
          ((red? (right w))
           ;; case 2: w's right child is red
           (values (graft/recolor w
                                  (color node)
                                  (change-right node
                                                black
                                                (left w))                                    
                                  (change-color black (right w)))
                   #f))
          ((red? (left w))
           ;; case 3: w's left child is red, transform into case 2
           ;; by rotating at w
           (values (graft/recolor (left w)
                                  (color node)
                                  (change-right node
                                                black
                                                (left (left w)))
                                  (change-left w
                                               (color w)
                                               (right (left w))))
                   #f))
          (else
           (values node #t)))))

; --------------------
; Other useful procedures

;; NODE->LIST (postorder)
(define (node->list node)
  (if (not (or (red? node) (black? node)))
      node      
      (list (if (red? node) 'red 'black)
            (node->list (left node))            
            (node->list (right node))
            (value node))))

(define (tree->node-list tree)
  (node->list (root tree)))

(define (lfold-tree-in-order proc seed tree)
  (let loop ((acc seed) (node (root tree)))
    (if (not node)
        acc
        (loop (proc node
                    (loop acc (left node)))
              (right node)))))

(define (tree/in-order->list tree)
  (reverse (lfold-tree-in-order (lambda (item acc)
                                  (cons (value item) acc))
                                '()                                
                                tree)))

(define (insert-set tree lst)
  (if (null? lst)
      tree
      (insert-set (insert tree (car lst))
                  (cdr lst))))

(define (delete-set tree lst)
  (if (null? lst)
      tree
      (delete-set (delete tree (car lst))
                  (cdr lst))))

(define r/b-make-tree make-tree)
(define r/b-number-tree number-tree)
(define r/b-symbol-tree symbol-tree)
(define r/b-string-tree string-tree)
(define r/b-tree? red/black-tree?)
(define r/b-empty? empty-tree?)
(define r/b-ref tree-ref)
(define r/b-insert insert)
(define r/b-maybe-replace maybe-replace)
(define r/b-insert-set insert-set)
(define r/b-delete delete)
(define r/b-delete-set delete-set)
(define r/b-tree->node-list tree->node-list)
(define r/b-tree/in-order->list tree/in-order->list)

(define (r/b-lfold kons knil tree)
  (lfold-tree-in-order (lambda (item acc)
                         (kons (value item) acc))
                       knil
                       tree))
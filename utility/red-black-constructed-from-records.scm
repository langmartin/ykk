; --------------------
; tree and node definitions

(define-record-type rtd/rb-tree
  (make-tree tree= tree< root)
  red/black-tree?
  (tree= tree=)
  (tree< tree<)
  (root root))

(define-record-type rtd/red
  (red left right value)
  red?  
  (left red-left)
  (right red-right)
  (value red-value))

(define-record-type rtd/black
  (black left right value)
  black?
  (left black-left)
  (right black-right)    
  (value black-value))

(define (disclose-node obj)
  `(,(if (red? obj) 'red 'black)    
    ,(if (left obj) #t #f)
    ,(if (right obj) #t #f)
    ,(value obj)))

(define-record-discloser rtd/red disclose-node)
(define-record-discloser rtd/black disclose-node)

(define-syntax define-tree-ref
  (syntax-rules ()
    ((_ name index)
     (define (name tree)
       (record-ref tree index)))))

(define-tree-ref left  1)
(define-tree-ref right 2)
(define-tree-ref value 3)

; --------------------
; Tests

(define (recursive-record-pred? rec1 rec2 pred? ignore-type?)
  (and (record? rec1)
       (record? rec2)
       (= (record-length rec1) (record-length rec2))
       (or ignore-type? (eq? (record-ref rec1 0) (record-ref rec2 0)))       
       (let loop ((index (- (record-length rec1) 1)))
         (if (= index 0)
             #t
             (let ((ref1 (record-ref rec1 index))
                   (ref2 (record-ref rec2 index)))
               (and (if (record? ref1)
                        (recursive-record-pred? ref1 ref2 pred? ignore-type?)
                        (pred? ref1 ref2))
                    (loop (- index 1))))))))
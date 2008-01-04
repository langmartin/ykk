
;;;; List data-types
(define-syntax define-checked
  (syntax-rules (:tree :node)    
    ((_ (name (thing :tree)) body ...)
     (checked->define red/black-tree?
                      "First argument should be a tree"
                      (name thing) body ...))    
    ((_ (name (thing :node)) body ...)
     (checked->define red/black-node?
                      "First argument should be a node"
                      (name thing) body ...))))

(define-syntax checked->define
  (syntax-rules ()
    ((_ pred? error-message (name thing) body ...)
     (define (name thing)
       (if (pred? thing)
           (begin body ...)
           (error error-message 'name))))))

(define-syntax define-list-pred
  (syntax-rules ()
    ((_ name sym)
     (define (name foo)
       (and (pair? foo)
            (eq? (car foo) sym))))))

;;;; Implementation
;; Tree Constructor
(define (make-tree tree= tree< root)
  (list 'red/black-tree tree= tree< root))

(define-list-pred red/black-tree? 'red/black-tree)
(define-checked (tree= (tree :tree)) (cadr tree))
(define-checked (tree< (tree :tree)) (caddr tree))
(define-checked (root (tree :tree)) (cadddr tree))

;; Node Constructor
(define (red left right value)
  (list 'red left right value))

(define-list-pred red? 'red)

(define (black left right value)
  (list 'black left right value))

(define-list-pred black? 'black)

(define (red/black-node? foo)
  (or (red? foo)
      (black? foo)))

;; Node Accessor
(define-checked (left (node :node)) (cadr node))
(define-checked (right (node :node)) (caddr node))
(define-checked (value (node :node)) (cadddr node))

; --------------------
; Tests

(define (recursive-record-pred? rec1 rec2 pred? ignore-type?)
  (and (list? rec1)
       (list? rec2)
       (= (length rec1) (length rec2))
       (or ignore-type? (eq? (car rec1) (car rec2)))
       (let loop ((r1 rec1) (r2 rec2))
         (cond ((and (null? r1) (null? r2))
                #t)
               ((or (red/black-tree? (car r1))
                    (red/black-node? (car r1)))
                (recursive-record-pred? (car r1) (car r2) pred? ignore-type?))
               (else
                (and (pred? (car r1) (car r2))
                     (loop (cdr r2) (cdr r2))))))))


;;;; Red/Black Trees
(define-interface red/black-interface
  (export r/b-make-tree
          r/b-number-tree
          r/b-symbol-tree
          r/b-string-tree

          r/b-ref
          
          r/b-insert
          r/b-insert-set

          r/b-delete
          r/b-delete-set))

(define-interface red/black-inspection-interface
  (export r/b-tree->node-list
          r/b-tree/in-order->list))
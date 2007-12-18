;;;; Primitive
(define-interface extended-fluids-interface
  (export make-fluid
          (define-fluid :syntax)
	  let-fluid
	  let-fluids
	  fluid
          
	  fluid-cell-ref
	  fluid-cell-set!
          
	  set-fluid!))

(define-interface ykk/bindings-interface
  (export new-binding
          new-value-binding
          binding?
          binding-type
          binding-value
          cast-binding))

(define-interface ykk/bindings-internal-interface
  (export binding->s48-binding))

(define-interface ykk/names-interface
  (export make-name-table
          define-names
          delete-names
          lookup-name))

(define-interface ykk/names-inspection-interface
  (export list-names))

(define-interface ykk/environments-interface
  (export empty-environment
          extend-environment
          forget-bindings
          cast-bindings
          current-environment
          with-current-environment
          environment-ref
          lookup))

(define-interface ykk/environments-internal-interface
  (export carefully
          %environment-ref))

(define-interface ykk/evaluation-interface
  (export safe-evaluation-environment
          safe-eval
          eval/extend))

(define-interface ykk/evaluation-internal-interface
  (export safe-eval->env
          s48-package->ykk-definitions))

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
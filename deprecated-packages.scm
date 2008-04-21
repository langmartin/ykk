;;;; Primitive (Deprecate for comp)
(define-interface ykk/bindings-interface
  (export new-binding
          new-value-binding
          binding?
          binding-type
          binding-value
          cast-binding))

(define-interface ykk/bindings-internal-interface
  (export binding->s48-binding))

(define-structures ((ykk/bindings ykk/bindings-interface)
                    (ykk/bindings-internal ykk/bindings-internal-interface))
  (open scheme
        meta-types
        locations
        methods              ; FIXME: for :value, :symbol, etc.  Change to ykk/methods later
        srfi-9+
        assert
        (s48 bindings)
        (s48 packages))
  (files (prim binding)))

(define-interface ykk/names-interface
  (export make-name-table
          define-names
          delete-names
          lookup-name
          fold-names
          merge-name-tables))

(define-interface ykk/names-inspection-interface
  (export list-names))

(define-structures ((ykk/names ykk/names-interface)
                   (ykk/names-inspection ykk/names-inspection-interface))
  (open scheme
        set
        assert)
  (files (prim names)))

(define-interface ykk/environments-interface
  (export empty-environment
          extend-environment
          forget-bindings
          cast-bindings
          current-environment
          with-current-environment
          environment-ref
          lookup))

(define-interface environment-manipulation-interface
  (export (subset :syntax)
          (with-prefix :syntax)
          (modify :syntax)
          prefix
          expose
          hide
          alias
          rename))

(define-interface ykk/environments-internal-interface
  (export carefully
          %environment-ref
          extend-with-native-package
          native-package->ykk-definitions))

(define-structures ((ykk/environments ykk/environments-interface)
                    (ykk/environments-internal ykk/environments-internal-interface)
                    (environment-manipulation environment-manipulation-interface))
  (open scheme
        uuidgen
        srfi-1
        srfi-9+
        fluids+
        conditions
        primitives
        big-util                ; for IDENTITY (is this worth the open?)
        methods                 ; FIXME: for :symbol, change to ykk/methods later
        (subset packages-internal (for-each-definition))
        ykk/names-inspection    ; LIST-NAMES for record discloser (FIXME: remove when done with initial development)
        ykk/names
        ykk/bindings
        assert)
  (files (prim environments)))

(define-interface ykk/evaluation-interface
  (export safe-evaluation-environment
          safe-eval
          safe-eval->env
          safe-eval/extend))

(define-structure ykk/evaluation ykk/evaluation-interface
  (open scheme
        compiler-envs
        package-commands-internal
        srfi-8
        srfi-9+
        primitives
        methods              ; for :symbol, FIXME: remove later
        (s48 bindings)
        (s48 evaluation)
        (s48 environments)
        (s48 packages)
        (s48 packages-internal)
        (s48 names)
        ykk/bindings
        ykk/bindings-internal
        ykk/environments
        ykk/environments-internal
        assert)
  (files (prim evaluation)))

(define-interface ykk/methods-interface
  (interface-of methods))

(define-structure ykk/methods ykk/methods-interface
  (open scheme
	define-record-types
	records record-types records-internal
	bitwise util primitives
	simple-signals
        assert
        ykk-ports)
  (files (prim ykk-methods))
  (optimize auto-integrate))

(define-interface ykk/types-interface
  (export (type-definition :syntax)
          (define-type :syntax)
          (define-updater :syntax)
          new
          undefined
          undefined?

          (unstructure :syntax)
          instance-ref

          :ykk
          :ykk-type))

(define-interface ykk/type-reflection-interface
  (export ykk-type?

          type-slots
          type-slot-ref
          slot-names
          slot-types

          slot-name
          slot-type
          slot-initform

          instance-type
          instance-values

          has-init-form?))

(define-structures ((ykk/types ykk/types-interface)
                    (type-reflection ykk/type-reflection-interface))  

  ;; for destructuring
  (for-syntax (open scheme srfi-1))
  (open extra-scheme
        (modify sharing (rename (shared:share share)) (prefix shared:))
        methods meta-methods
        list srfi-8 srfi-9+
        proc-def
        exceptions
        primitives
        (subset record-types (record-type?)))
  (files types))

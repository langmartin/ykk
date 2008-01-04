;;;; Bindings look exactly like scheme48 bindings, except the type is
;;;; not the static type.  Instead it's a reference to something in
;;;; the type tree.

;;;; Currently, the implementation is a shallow wrapper around
;;;; Scheme48 bindings.  A real Scheme48 binding is needed in the
;;;; evaluator hook.  From the standpoint of the rest of the system, a
;;;; binding is a tuple <TYPE,VALUE>.  The location abstraction is
;;;; conflated with the binding abstraction (that is, a location is
;;;; not first-class in YKK whereas in scheme48 both locations and
;;;; bindings are first-class).

;;; Bindings
(define-record-type rtd/binding
  (make-binding type s48)
  binding?
  (type binding-type)
  (s48 binding->s48-binding))

(define (cast-binding binding type)
  (make-binding type (binding->s48-binding binding)))

(define (binding-value binding)
  (contents (s48:binding-place (binding->s48-binding binding))))

(define (guess-type value)
  ;; FIXME: add type-guessing logic for ykk types once they're implemented?
  :value)

(define new-location
  (let ((ykk-locations (s48:make-simple-package '() #f #f 'ykk-locations)))
    (lambda (name value)
      (let ((loc (s48:make-new-location ykk-locations name)))
        (set-contents! loc value)
        loc))))

(define (new-binding name type value)
  (let ((place (if (s48:binding? value)
                 value
                 (s48:make-binding usual-variable-type (new-location name value) #f))))
    (make-binding type place)))

(define (new-value-binding name value)
  (new-binding name (guess-type value) value))

;;;; Tests
(let ((foo (new-binding 'foo :symbol 'bar)))
  (assert (binding? foo))
  (assert (s48:binding? (binding->s48-binding foo)))
  (assert (location? (s48:binding-place (binding->s48-binding foo))))
  (assert (binding-type foo) => :symbol)
  (assert (binding-value foo) => 'bar))
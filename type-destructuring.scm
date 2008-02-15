;; (destructure STRUCTURE) => list of values that are bound in STRUCTURE
;;
;; STRUCTURE              ::= (INSTANCE BINDING ...)
;; INSTANCE               ::= IDENTIFIER | (IDENTIFIER => ALIAS)
;; BINDING                ::= STRUCTURE | INSTANCE
;; ALIAS                  ::= IDENTIFIER | UNINTERNED-IDENTIFIER
;; UNINTERNED-IDENTIFIER  ::= _
;;
;; An "uninterned identifier" can be used to force an intermediate
;; type instance into the result list.
;;
;; Example:
;; (destructure (some-order (bill-to street) (ship-to street)))
;; => (list "bill-to street" "ship-to street")
;;
;; (destructure (some-order ((bill-to => _) street) (ship-to street)))
;; => (list bill-to-instance "bill-to street" "ship-to street")
;;
(define-syntax destructure
  (lambda (form rename compare)
    (let* ((%make-let (rename 'make-let))
           (%list (rename 'list))
           (instance (caadr form))
           (structure (alias-everything (cadr form))))
      (call-with-values
          (lambda () (structure->grouped-bindings rename structure))
        (lambda (names descended-through only-referenced)
          `(,%make-let ,descended-through
                       ,only-referenced
                       (,%list ,@names)))))))

;; (with-destructured (structure ...) body ..)
;;
;; Wraps BODY in a LET-block of bindings given by STRUCTURE ...
(define-syntax with-destructured
  (lambda (form rename compare)
    (let ((formals (cadr form))
          (%body `(,(rename 'begin) ,@(cddr form)))
          (%make-let (rename 'make-let)))

      (define (make-let structure body)
        (call-with-values
            (lambda () (structure->grouped-bindings rename structure))
          (lambda (names descended-through only-referenced)
            `(,%make-let ,descended-through
                         ,only-referenced
                         ,body))))

      (fold make-let %body (reverse formals)))))

;;; Utility
;; MAKE-LET -- given a list of type instances and instance references,
;;   make a let block
;;
;; NESTED-INSTANCES     ::= (BINDING ...)
;; REFERENCES           ::= (BINDING ...)
;; BINDING             ::= SCHEME-BINDING | TYPE-BINDING
;; SCHEME-BINDING      ::= (LET-IDENTIFIER SCHEME-IDENTIFIER #f CONTEXT NESTED?)
;; TYPE-BINDING        ::= (LET-IDENTIFIER SLOT-NAME TYPE-INSTANCE CONTEXT NESTED?)
;;
;; The bindings in NESTED-INSTANCES are from outermost to innermost.
;; Each binding refers to the previous one.  The bindings are created
;; in a LET* block.
;;
;; The bindings in REFERENCES are all terminal.  That is, each refers
;; to some binding in NESTED-INSTANCES, but no other binding refers to
;; it.  These bindings are created in a LET block nested inside the
;; LET* block.
(define-syntax make-let
  (syntax-rules ()
    ((_ "inst" ((name slot-name instance path #t) ...) body)
     (let* ((name (type-instance->destructurable
                   (slot-ref instance slot-name path)))
            ...)
       body))
    ((_ "ref" ((name slot-name instance path #f) ...) body)
     (let ((name (slot-ref instance slot-name path))
           ...)
       body))
    ;; public interface
    ((_ nested-instances references body)
     (make-let "inst" nested-instances
               (make-let "ref" references body)))))

;; FIXME: when types are fully implemented, change this to do
;; syntax-time lookup of accessor procedures
(define-syntax slot-ref
  (syntax-rules ()
    ((_ #f scheme-binding path)
     scheme-binding)
    ((_ instance slot-name path)
     (really-slot-ref instance 'slot-name 'path))))

(define (really-slot-ref env name path)
  (cond ((assq name env) => cadr)
        (else
         (type-error "unrecognized slot"
                          `(slot ,name in ,(pretty-path path))))))

(define (type-instance->destructurable instance)
  (zip (slot-names instance)
       (instance-values instance)))
;;; tests
(begin

   (define-type :address ()
     (street :string)
     (city :string)
     (state)
     (zip))

   (define-type :intl-address (:address)
     (country :string))

   (define-type :order ()
     (id :number)
     (bill-to :address)
     (ship-to :intl-address))

   (let* ((coptix (new :address "4009 Tennessee Ave" "Chattanooga" 'TN '37409))
          (apt (new :intl-address "507 E 5th St, Apt #7" "Chattanooga" 'TN '37403 "US"))
          (some-order (new :order 100 coptix apt)))



     ;; simple: just get a few slots from the instance
     (assert (destructure (apt street zip))
             => '("507 E 5th St, Apt #7" 37403))

     ;; more complex: get some nested slot values and include an
     ;;  intermediate instance in the final list
     (assert (destructure (some-order ((bill-to => _) street) (ship-to street)))
             => `(,coptix "4009 Tennessee Ave" "507 E 5th St, Apt #7"))

     ;; slots may be repeated
     (assert (destructure (apt zip zip zip))
             => '(37403 37403 37403))

     ;; with-destructured allows multiple instances to be destructured
     ;; into the same scope.  Notice that coptix->zip shadows
     ;; apt->zip.
     (with-destructured
      ((apt (street => apt-street) state zip)
       (coptix zip))
      (assert (list apt-street state zip)
              => '("507 E 5th St, Apt #7" TN 37409)))
     ))

(type-slots :address)

(define-generic foo &foo)
(define-method &foo (thing)
  "default")
(define-method &foo ((thing :address))
  "address")

(let* ((coptix (new :address "4009 Tennessee Ave" "Chattanooga" 'TN '37409))
       (apt (new :intl-address "507 E 5th St, Apt #7" "Chattanooga" 'TN '37403 "US"))
       (some-order (new :order 100 coptix apt)))

  (foo apt))

(begin

   (define-type :address ()
     (street :string)
     (city :string)
     (state)
     (zip))

   (define-type :intl-address (:address)
     (country :string))

   (define-type :order ()
     (id :number)
     (bill-to :address)
     (ship-to :intl-address))

   (type-slots :address))

(strict-subtype? :intl-address :address)

(similar:ykk? :intl-address :ykk)
(similar:simple? :number :real)

(ancestors :intl-address)
(ancestors<= :address :intl-address)
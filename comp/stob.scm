(define-condition
  stob-error (error)
  stob-error?)

;;;; Stored Object

(define *minimal-stob-info* '((identifier) (type)))
(define *stob-slots* (length *minimal-stob-info*))

(define *minimal-type-info* '((name) (self) (self-size) (instance) (instance-size)))
(define *type-slots* (length *minimal-type-info*))

;; --------------------
;; STOB data-dependent representation dependent procedures

(define %stob? vector?)
(define stob-ref vector-ref)
(define stob-set! vector-set!)
(define stob-size vector-length)

(define (stob-initialize-header! stob identifier type)
  (stob-set! stob 0 identifier)  
  (stob-set! stob 1 (or type stob)))

(define (args->stob stob offset args)
  (if (null? args)
      stob
      (begin
        (stob-set! stob offset (car args))
        (args->stob stob (+ offset 1) (cdr args)))))

;; --------------------
;; A few primitive STOB operations

(define-syntax define-stob-index-ref
  (syntax-rules ()
    ((_ (name index) ...)
     (begin
       (define (name stob)
         (vector-ref stob index))
       ...))))

(define-stob-index-ref
  (%stob-identifier 0)
  (%stob-type 1)
  (%stob-type-name 2)
  (%stob-type-self 3)
  (%stob-type-self-size 4)
  (%stob-type-slots 5)
  (%stob-type-slots-size 6))

;; A STOB has a STOB-TYPE in its STOB-TYPE slot.
(define (stob? foo)
  (and (%stob? foo)       
       (stob-type? (%stob-type foo))))

;; A STOB-TYPE is self-referencing in its STOB-TYPE slot.
(define (stob-type? foo)
  (and (%stob? foo)
       (eq? foo (%stob-type foo))))

;; --------------------
;; Construction

(define (really-make-stob static-identifier type size initialize verify)
  (let ((address stob commit (allocate (+ *stob-slots* size) static-identifier)))
    (cond ((not stob)
           (stob-error 'make-stob "could not allocate space"))
          ((and static-identifier (not commit))
           (verify stob))
          (else
           (stob-initialize-header! stob static-identifier type)
           (commit
            (initialize
             (initializer 'really-make-stob size
                          (cut args->stob stob *stob-slots* <>))))))))

(define (initializer name size k)
  (lambda args
    (k (assert-size name size args))))

(define (partial-initializer final . partial)  
  (lambda (initializer)
    (final
     (lambda args
       (apply initializer (append partial args))))))

(define (assert-size name size args)
  (if (and size (not (= (length args) size)))
      (stob-error 'wrong-number-of-arguments
                  name
                  `(got: ,(length args) expecting: ,size)
                  `(args: ,@args))
      args))

;; --------------------
;; STOB Types

(define (make-stob-type static-identifier name self instance initialize verify)
  (let* ((self (merge-type-descriptions *minimal-type-info* self))
         (self-size (length self)))    
    (really-make-stob static-identifier
                      #f
                      self-size
                      (partial-initializer initialize name self self-size instance (length instance))
                      (compose verify (cut consistent-static-type <> name self instance)))))

(define (consistent-static-type stob name self slots)
  (let ((orig-name (%stob-type-name stob))
        (orig-self (%stob-type-self stob))
        (orig-slots (%stob-type-slots stob)))     
    (if (and (eq? name orig-name)
             (equal? self orig-self)
             (equal? slots orig-slots))
        stob
        (stob-error 'make-stob-type
                    "inconsistent static definition"
                    `(orig: ,orig-name ,orig-slots)
                    `(new: ,name ,slots)))))

(define merge-type-descriptions update-force-alist)

(define (type-description-index describe type name)
  (+ *stob-slots*
     (or (alist-key-index eq? name (describe type))
         (stob-error 'type-description-index
                     "could not find slot"
                     `(type: ,type)
                     `(description: ,describe)
                     `(slot-name: ,name)))))

(define (stob-type-slot-index type name)
  (type-description-index %stob-type-slots type name))

(define (stob-type-self-index type name)
  (type-description-index %stob-type-self type name))

;; --------------------
;; STOB Construction

(define (make-stob static-identifier type initialize verify)
  (really-make-stob static-identifier
                    type
                    (%stob-type-slots-size type)
                    initialize
                    verify))

;; --------------------
;; STOB Definition Helpers

(define (make-monomorphic-stob-predicate type)
  (lambda (foo)
    (and (not (eq? foo type))
         (%stob? foo)
         (eq? (%stob-type foo) type))))

;;;; Basic Accessors
(define-syntax define-accessors
  (syntax-rules ()
    ((_ (name pred? access) ...)
     (begin
       (define (name (obj pred?))
         (access obj))
       ...))))

(define-accessors
  (stob-identifier      stob?      %stob-identifier)
  (stob-type            stob?      %stob-type)
  (stob-type-name       stob-type? %stob-type-name)
  (stob-type-self       stob-type? %stob-type-self)
  (stob-type-self-size  stob-type? %stob-type-self-size)
  (stob-type-slots      stob-type? %stob-type-slots)
  (stob-type-slots-size stob-type? %stob-type-slots-size)
  )

;;;; Scheme 48
(define-simple-type :stob (:vector) stob?)
(define-simple-type :stob-type (:stob) stob-type?)

(define-method &disclose ((obj :stob))
  `(stob ,(%stob-identifier obj)))

(define-method &disclose ((obj :stob-type))
  `(stob-type ,(%stob-type-name obj) ,(%stob-identifier obj)))

;;;; Tests

(begin
  (assert ((initializer 'foo 2 identity) 'a 'b) => '(a b))
  (assert ((cut <> 'a 'b) (initializer 'foo 2 identity)) => '(a b))  
  (assert ((partial-initializer (cut <> 'b) 'a)
           (initializer 'foo 2 identity))
          => '(a b))
  
  (assert ((partial-initializer (cut <> 'e) 'c 'd)
           (lambda x (append '(a b) x)))
          => '(a b c d e))

  (let ((t (make-stob-type "7D70A6F2-D74E-4F34-8AB5-84FAD1B701D3"
                           'foo-type
                           '()                     
                           '((a))
                           (cut <>)
                           identity)))    
    (assert (stob-type? t))
    (assert (stob-type t) => t)
    (assert (stob-type-name t) => 'foo-type)
    (assert (stob-type-self t) => *minimal-type-info*)
    (assert (stob-type-self-size t) => (length *minimal-type-info*))
    (assert (stob-type-slots t) => '((a)))    
    (assert (stob-type-slots-size t) => 1)    
    (assert (stob-type-slot-index t 'a) => *stob-slots*)

    (let ((stob (make-stob "9C9D1690-42F7-48B9-8D27-CD31A1A163E4"
                           t                           
                           (cut <> 'a-value)
                           identity)))
      (assert (stob? stob))
      (assert (not (stob-type? stob)))
      (assert (stob-type stob) => t)
      (assert ((make-monomorphic-stob-predicate t) stob)))))
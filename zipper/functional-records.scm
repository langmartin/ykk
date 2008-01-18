(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor)
       ...)
     (begin
       (define type
         (make-record-type 'type '(field-tag ...)))
       (define constructor
         (record-constructor type '(constructor-tag ...)))
       (define predicate
         (record-predicate type))
       (define accessor
         (record-accessor type 'field-tag))
       ...))))

;;;; record-type
(define record-marker (list 'record-marker))

(define real-vector? vector?)

(define (record-marker? x)
  (and (< 0 (vector-length x))
       (eq? (vector-ref x 0)
            record-marker)))

(define (vector? x)
  (and (real-vector? x)
       (not (record-marker? x))))

(define (record? x)
  (and (real-vector? x)
       (record-marker? x)))

(define (make-record . args)
  (apply vector (cons record-marker args)))

(define (record-ref record index)
  (vector-ref record (+ index 1)))

;;; Record types are themselves records, so we first define the type for
;;; them.  Except for problems with circularities, this could be defined as:
;;;  (define-record-type :record-type
;;;    (make-record-type name field-tags)
;;;    record-type?
;;;    (name record-type-name)
;;;    (field-tags record-type-field-tags))
;;; As it is, we need to define everything by hand.

(define :record-type
  (make-record
   #f
   ':record-type
   '(name field-tags)))

(define (make-record-type name field-tags)
  (make-record
   :record-type
   name
   field-tags))

(define (record-type record)
  (record-ref record 0))

(define (record-type-name record-type)
  (record-ref record-type 1))

(define (record-type-field-tags record-type)
  (record-ref record-type 2))

;;;; procedures used by the macro expansion
(define (field-index type tag)
  (let loop ((i 1) (tags (record-type-field-tags type)))
    (cond ((null? tags)
           (error "record type has no such field" type tag))
          ((eq? tag (car tags))
           i)
          (else
           (loop (+ i 1) (cdr tags))))))

(define (record-constructor type tags)
  (let ((arg-count (length tags)))
    (lambda args
      (if (= (length args)
             arg-count)
          (apply make-record type args)
          (error "wrong number of arguments to constructor" type args)))))

(define (record-predicate type)
  (lambda (thing)
    (and (record? thing)
         (eq? (record-type thing)
              type))))

(define (record-accessor type tag)
  (let ((index (field-index type tag)))
    (lambda (thing)
      (if (and (record? thing)
               (eq? (record-type thing)
                    type))
          (record-ref thing index)
          (error "accessor applied to bad value" type tag thing)))))

;;;; testing
(define-record-type foo
  (make-foo one two)
  foo?
  (one foo-one)
  (two foo-two))

(define bar (make-foo 1 2))

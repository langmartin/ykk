(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor . more) ...)
     (begin
       (define type
         (make-record-type 'type '(field-tag ...)))
       (define constructor
         (record-constructor type '(constructor-tag ...)))
       (define predicate
         (record-predicate type))
       (define-record-field type field-tag accessor . more)
       ...))))

(define-syntax define-record-field
  (syntax-rules ()
    ((define-record-field type field-tag accessor)
     (define accessor (record-accessor type 'field-tag)))
    ((define-record-field type field-tag accessor modifier)
     (begin
       (define accessor (record-accessor type 'field-tag))
       (define modifier (record-modifier type 'field-tag))))))

;;; Record types are implemented using vector-like records.  The first
;;; slot of each record contains the record's type, which is itself a
;;; record.

(define (record-type record)
  (record-ref record 0))

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
   :record-type
   ':record-type
   '(name field-tags)))

;;; Now that :record-type exists we can define a procedure for making
;;; more record types.

(define (make-record-type name field-tags)
  (make-record
   :record-type
   name
   field-tags))

(define (record-type-name record-type)
  (record-ref record-type 1))

(define (record-type-field-tags record-type)
  (record-ref record-type 2))

;;; A utility for getting the offset of a field within a record.

(define (field-index type tag)
  (let loop ((i 1) (tags (record-type-field-tags type)))
    (cond ((null? tags)
           (error "record type has no such field" type tag))
          ((eq? tag (car tags))
           i)
          (else
           (loop (+ i 1) (cdr tags))))))

;;; Now we are ready to define RECORD-CONSTRUCTOR and the rest of the
;;; procedures used by the macro expansion of DEFINE-RECORD-TYPE.

(define (record-constructor type tags)
  (let ((size (length (record-type-field-tags type)))
        (arg-count (length tags))
        (indexes (map (lambda (tag)
                        (field-index type tag))
                      tags)))
    (lambda args
      (if (= (length args)
             arg-count)
          (let ((new (make-record (+ size 1))))
            (record-set! new 0 type)
            (for-each (lambda (arg i)
			(record-set! new i arg))
                      args
                      indexes)
            new)
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

(define (record-modifier type tag)
  (let ((index (field-index type tag)))
    (lambda (thing value)
      (if (and (record? thing)
               (eq? (record-type thing)
                    type))
          (record-set! thing index value)
          (error "modifier applied to bad value" type tag thing)))))

;;;; record type itself
(define record-marker (list 'record-marker))

(define real-vector? vector?)

(define (vector? x)
  (and (real-vector? x)
       (or (= 0 (vector-length x))
	   (not (eq? (vector-ref x 0)
		record-marker)))))

(define (record? x)
  (and (real-vector? x)
       (< 0 (vector-length x))
       (eq? (vector-ref x 0)
            record-marker)))

(define (make-record . args)
  (apply vector (cons record-marker args)))

(define (record-ref record index)
  (vector-ref record (+ index 1)))

(define (record-set record index value)
  (vector-set record (+ index 1) value))

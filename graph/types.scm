
;;;; Self-describing Descriptors

;;;; Basic Operations on Type Descriptors
(define (make-predicate type)
  (let ((i (initial-index type)))    
    (lambda (foo)
      (and (stob? foo)
           (> (stob-size foo) i)
           (eq? type (record-ref stob i))))))

(define (initial-index type)
  (cond ((self-described? type) 0)
        (else 0)))

;;;; Type Descriptor
(define :record-type-descriptor
  (make-self-described
   "A7034818-6968-4427-B1DF-E8420026737D"
   (list->description '(type identifier name slots))
   (lambda (description construct)
     (construct description
                (new-identifier)
                'record-type-descriptor                  
                (list->description '(type identifier name parents annotation schemes slots defined-schemes defined-slots priority))))))

(define record-type-descriptor?
  (make-predicate :record-type-descriptor))

(define (really-make-type-descriptor static-identifier type initialize verify)
  (allocate/verify
   static-identifier
   (lambda ()
     (initialize
      (lambda args
        (apply record (append (list type (new-identifier)) args)))))   
   verify))

(define (make-type-scheme ))

(define (really-make-type-descriptor type static-identifier name parent annotation schemes slots)
  (allocate/verify
   static-identifier
   (lambda ()
     (assert-not-scheme! parent)
     (assert-all-schemes! schemes)
     (let ((actual-slots (inherit-slots parent (normalize-slot-description slots)))
           (actual-schemes (inherit-schemes parent schemes)))       
       (assert-slots-consistent/schemes! actual-slots actual-schemes)
       (record (polymorphic-type :record-type-descriptor type)
               (new-identifier)
               name
               (if parent (list parent) '())
               actual-schemes
               actual-slots
               schemes
               slots               
               (compute-priority parent))))
   (cut verify-consistent-descriptor <> name parent schemes slots)))

(define-stob-accessors
  record-type-descriptor?
  (descriptor-name             2)
  (descriptor-parents          3)
  (descriptor-schemes          4)
  (descriptor-slots            5)
  (descriptor-defined-schemes  6)
  (descriptor-defined-slots    7)
  (descriptor-priority         8))

(define (inherit-slots parent slot-descriptor)
  (if (not parent)
      slot-descriptor
      (combine-slot-descriptors
       (descriptor-slots parent)
       slot-descriptor)))

(define (inherit-schemes parent schemes)
  (cond ((not parent) schemes)
        ((null? schemes) (descriptor-schemes parent))
        (else
         (delete-duplicates
          (append (descriptor-schemes parent)
                  schemes)
          eq?))))

(define (combine-slot-descriptors d1 d2)
  (combine-descriptors-by-name
   `((type ,(cut choose-more-specific-type d1 <> d2 <>)))
   proj-1
   d1
   d2))

(define (choose-more-specific-type d-a a d-b b)
  (if (%same-type? a b)
      a
      (let ((less more (if (more-specific-type? b a) (values b a) (values a b))))
        (if (strict-subtype? less more)
            more
            (error 'incompatible-slot-descriptions
                   `(,more is-not-a-subtype-of ,less)
                   `(original-spec: ,(description-specification d-a))
                   `(extended-by: ,(description-specification d-b)))))))

;; ====> from methods.scm <====
(define *increment* 10)

(define (compute-priority super)
  (if super
      (+ *increment* (%type-priority super))
      0))

(define (more-specific-type? t1 t2)
  (> (%type-priority t1) (%type-priority t2)))
;; ====> end from methods.scm <====

(define (assert-slots-consistent/schemes! d schemes)
  (for-each (lambda (s)
              (if (not (descriptions-consistent?
                        (descriptor-description s)
                        d))
                  (error 'inconsistent-type-descriptions
                         "description of type is inconsistent with a scheme it implements"
                         `(type-description: ,d)
                         `(scheme-description: ,(descriptor-description s)))))                        
            schemes))

;; D2 is consistent with D1 if it:
;;  1. Has all of the names specified in D1
;;  2. The type of each name in D2 is a STRICT-SUBTYPE? of the
;;     corresponding name in D1.
(define (descriptions-consistent? d1 d2)
  (let ((spec2 (description-specification d2)))
    (let loop ((spec1 (description-specification d1)))
      (cond ((null? spec1)
             #t)
            ((assq (slot-spec-name spec1) spec2)
             => (lambda (spec2-entry)
                  (and (strict-subtype? (slot-spec-type (car spec1))
                                        (slot-spec-type (spec2-entry)))
                       (loop (cdr spec1)))))
            (else #f)))))

(define slot-spec-name car)
(define (slot-spec-type spec)
  (cond ((assq 'type (cdr spec)) => cdr)
        (else :value)))

(define (verify-consistent-descriptor d name parent schemes slots)
  (assert-consistent-descriptor!
   d
   name
   parent
   schemes
   (normalize-slot-description slots))
  d)

(define (assert-consistent-descriptor! d name parent schemes slots)
  (if (not (and (eq? name (descriptor-name d))
                (descriptor-parent-is? d parent)
                (descriptor-defines-schemes? d schemes)
                (equal? slots (descriptor-defined-slots d))))
      (error 'inconsistent-descriptor
             d
             `(name: ,name)
             `(parent: ,parent)
             `(schemes: ,schemes)
             `(slots: ,slots))))

(define (descriptor-parent-is? d parent)
  (let ((p (descriptor-parents d)))
    (or (and (not parent) (null? p))
        (and parent (not (null? p)) (eq? parent (car p))))))

(define (descriptor-implements-schemes? d schemes)
  (lset= eq?
         (descriptor-schemes d)
         schemes))

;;;; Operations on Type Lattice
(define (%type-priority t))

(define (%same-type? t1 t2))

(define (strict-subtype? t1 t2))
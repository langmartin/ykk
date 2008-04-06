;;;; Operations on Types
(define (strict-subtype? t maybe-supertype)
  (or (%same-type? t maybe-supertype)
      (direct-descendant? t maybe-supertype)))

(define (%same-type? t1 t2)
  (or (identical-type? t1 t2)
      (same-type? t1 t2)))

(define identical-type? eq?)

(define (direct-descendant? t maybe-ancestor)
  (find (cut %same-type? maybe-ancestor <>) (ancestors t)))

(define (ancestors t)
  (delete-duplicates (accumulate-superiors t) %same-type?))

(define (ancestors-and-self t)
  (cons t (ancestors t)))

(define (accumulate-superiors t)
  (expanding-fold cons type-superiors '() (type-superiors t)))

;; ====> from methods.scm <====
(define *increment* 10)

(define (compute-priority supers)
  (if (null? supers)
      0
      (+ (apply max (map-in-order type-priority supers))
	 *increment*)))

(define (more-specific-type? t maybe-less-specific)
  (> (type-priority t) (type-priority maybe-less-specific)))
;; ====> end from methods.scm <====

;;;; Utility
;; depth-first expansion
(define (expanding-fold kons expand seed lst)

  (define (recur item acc)
    ;; minor optimization: check for a non-null expansion before
    ;; making the recursive call.
    (let ((expanded (expand item)))
      (if (null? expanded)
          acc
          (expanding-fold kons expand acc expanded))))

  (fold (lambda (item acc) (kons item (recur item acc)))
        seed
        lst))

;;;; Tests
(begin
  (assert (identical-type? :zero :zero))
  (assert (not (identical-type? :zero :null)))
  (assert (more-specific-type? :integer :number))
  (assert (lset= eq?
                 (ancestors :integer)
                 (list :rational :real :complex :number :value :values)))
  (assert (direct-descendant? :integer :number))
  (assert (strict-subtype? :integer :number))
  (assert (not (strict-subtype? :char :symbol))))
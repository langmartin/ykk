;;;; MOP Extensions

;; --------------------
;; Type Name

(define-generic type-name &type-name (type))

(define-method &type-name ((type :simple-type))
  (record-ref type 4))

(define-method &type-name ((type :record-type))
  (record-type-name type))

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

;;;; If we ever implement type-inferencing...
(define (scheme-form-conforms-to-type? type form)
  (cond ((not type)
         #t)
        ((eq? type :symbol)
         (and (quotation? type) (symbol? (cadr form))))
        ((self-evaluating-type? type)
         ((type-predicate type) form))
        (else
         (error 'wrong-type-argument
                "scheme-data-conforms-to-type?: unhandled type"
                type))))

(define self-evaluating-type?
  (let ((self-evaluating (list :zero :number :boolean :char :null :string)))
    (lambda (t)
      (lset<= (ancestors-and-self t) self-evaluating))))

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
  (assert (type-name :integer) => ':integer)
  (assert (type-name :record-type) => 'record-type)
  
  (assert (identical-type? :zero :zero))
  (assert (not (identical-type? :zero :null)))
  (assert (more-specific-type? :integer :number))
  (assert (lset= eq?
                 (ancestors :integer)
                 (list :rational :real :complex :number :value :values)))
  (assert (direct-descendant? :integer :number))
  (assert (strict-subtype? :integer :number))
  (assert (not (strict-subtype? :char :symbol)))

  (assert (scheme-form-conforms-to-type? #f #f))
  (assert (scheme-form-conforms-to-type? :number 1))
  (assert (scheme-form-conforms-to-type? :string "foo"))
  (assert (scheme-form-conforms-to-type? :boolean #f))
  (assert (scheme-form-conforms-to-type? :null '()))
  )
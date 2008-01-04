;;;; Sets
;;;;
;;;; An implementation of sets in using srfi-1

(define-record-type rtd/list-set
  (really-make-set set= set< elements)
  set?
  (set= set=)
  (set< set<)
  (elements set->list))

(define (make-set set= set< elements)
  (really-make-set set= set<
                   (or elements '())))

(define (empty-set set= set<)
  (make-set set= set< #f))

(define (empty? set)
  (null? (set->list set)))

(define (in-set? set item)
  (any (element-pred? set item)
       (set->list set)))

(define (set-ref set item . default)
  (cond
   ((find-tail (element-pred? set item) (set->list set)) => car)
   (else
    (and (not (null? default))
         (car default)))))

(define (set=? . sets)
  (apply-nary-lset-op lset= sets))

(define (set<=? . sets)
  (apply-nary-lset-op lset<= sets))

(define (adjoin set . elements)
  (adjoin-list set elements))

(define (adjoin-list set elements)
  (update set
          (apply lset-adjoin (set= set) (set->list set) elements)))

(define (remove set . elements)
  (remove-list set elements))

(define (remove-list set elements)
  (update set
          (lset-difference (set= set) (set->list set) elements)))

(define (union . sets) (lset-nary-op lset-union sets))
(define (difference . sets) (lset-nary-op lset-difference sets))
(define (intersection . sets) (lset-nary-op lset-intersection sets))

;;;; Internal
(define (update set elements)
  (make-set (set= set)
            (set< set)
            elements))

(define (lset-nary-op op sets)
  (update (car sets)
          (apply-nary-lset-op op sets)))

(define (apply-nary-lset-op op sets)
  (apply op
         (cons (set= (car sets))
               (map set->list sets))))

(define (element-pred? set item)
  (lambda (element)
    ((set= set) element item)))
;;;; Sets
;;;;
;;;; A simple implementation of sets in using red/black trees.

;;;;
(define (make-set set= set< elements)
  (let ((set (r/b-make-tree set= set< #f)))
    (cond ((set? elements) (union set elements))
          ((list? elements) (adjoin-list set elements))
          (else set))))

(define (empty-set set= set<)
  (make-set set= set< #f))

(define set? r/b-tree?)
(define empty? r/b-empty?)

(define (in-set? set item)
  (and (r/b-ref set item)
       #t))

(define set-ref r/b-ref)

(define (set<=? . sets)
  (boolean-set-fold subset? sets))

(define (set=? . sets)
  (boolean-set-fold set-equal sets))

(define (set->list set)
  (reverse (r/b-lfold cons '() set)))

(define (adjoin set . elements)
  (adjoin-list set elements))

(define (adjoin-list set elements)
  (fold adjoin-element set elements))

(define (remove set . elements)
  (remove-list set elements))

(define remove-list r/b-delete-set)

(define (union . sets)
  (fold (lambda (set acc)
          (if (or (not acc) (r/b-empty? acc))
              set
              (r/b-lfold adjoin-element acc set)))
        #f
        sets))

(define (difference . sets)
  (fold (lambda (set acc)
          (r/b-lfold remove-element acc set))
        (car sets)
        (cdr sets)))

(define (intersection . sets)
  (fold (lambda (set seed)
          (cond ((r/b-empty? set) set)
                ((r/b-empty? seed) seed)
                (else
                 (r/b-lfold (lambda (element acc)
                              (if (r/b-ref set element)
                                  acc
                                  (remove-element element acc)))
                            seed
                            seed))))
        (car sets)
        (cdr sets)))

;;;; Internal
(define (never? . x) #f)

(define (adjoin-element element set)
  (r/b-maybe-replace set element never?))

(define (remove-element element set)
  (r/b-delete set element))

(define (subset? set1 set2)
  (or (eq? set1 set2)
      (call-with-current-continuation
       (lambda (exit)
         (r/b-lfold (lambda (item acc)
                      (if (not (in-set? set2 item))
                          (exit #f)
                          acc))
                    #t
                    set1)))))

(define (set-equal set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

(define (boolean-set-fold pred? sets)
  (call-with-current-continuation
   (lambda (exit)
     (and (fold (lambda (set2 set1)
                  (if (not (pred? set1 set2))
                      (exit #f)
                      set2))
                (car sets)
                (cdr sets))
          #t))))

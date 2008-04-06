;;;; Store

;; Represented here as a table, a `Store' must support the creation of a
;; location, the dereferencing of such a location, and changing the value
;; of the location.

;; A location is represented as an identifier.

;; STORE-REF will return #f when a non-existent location has been
;; dereferenced.  When a location does exist, a stored-object is
;; returned.

(define make-stob cons)
(define stob-address car)
(define stob-data cdr)
(define stob? pair?)

(define *store* (make-symbol-table))

(define (store-ref loc)
  (table-ref *store* loc))

(define (store-set! loc stob)
  (table-set! *store* loc stob))

(define (store loc obj commit)
  (let ((stob (commit (make-stob loc obj))))
    (store-set! loc stob)
    stob))

(define (clear-store!)
  (set! *store* (make-symbol-table)))

(define address identifier)
(define new-address new-identifier)

(define store-static store)
(define static-address identifier)
(define static-ref store-ref)

(define (allocate static-identifier thunk commit)
  (let ((address (if static-identifier (static-address static-identifier) (new-address))))
    (cond ((and static-identifier (static-ref address))
           => (cut values <> #f))
          (static-identifier
           (values (store-static address (thunk) commit) #t))
          (else
           (values (store address (thunk) commit) #t)))))

(begin
  (let ((v1 alloc1? (allocate #f (lambda () (make-vector 1)) identity))
        (v2 alloc2? (allocate #f (lambda () (make-vector 1)) identity)))
    (let ((addr1 (stob-address v1))
          (addr2 (stob-address v2)))

      (assert alloc1?)
      (assert alloc2?)
      (assert (neq? v1 v2))
      (assert (eq? v1 (store-ref addr1)))
      (assert (eq? v2 (store-ref addr2)))

      ;; clean up
      (store-set! addr1 #f)
      (store-set! addr2 #f)))

  (let* ((v1 alloc1? (allocate 'foo (lambda () (make-vector 1)) identity))
         (addr1 (stob-address v1)))
    (assert alloc1?)
    (assert (eq? v1 (store-ref addr1)))

    (let* ((v2 alloc2? (allocate 'foo (lambda () (make-vector 1)) identity))
           (addr2 (stob-address v2)))
      (assert (not alloc2?))
      (assert (eq? v1 v2))

      ;; clean up
      (store-set! addr1 #f)
      )))
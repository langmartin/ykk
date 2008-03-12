;;;; Store

;; Represented here as a table, a `Store' must support the creation of a
;; location, the dereferencing of such a location, and changing the value
;; of the location.

;; A location is represented as an identifier.

;; STORE-REF will return #f when a non-existent location has been
;; dereferenced.  When a location does exist, a stored-object is
;; returned.

(define *store* (make-symbol-table))

(define (store-ref loc)
  (table-ref *store* loc))

(define (store-set! loc stob)
  (table-set! *store* loc stob))

(define (store loc stob)
  (store-set! loc stob)
  stob)

(define (clear-store!)
  (set! *store* (make-symbol-table)))

(define address identifier)
(define new-address new-identifier)

(define store-static store)
(define static-address identifier)
(define static-ref store-ref)

(define (allocate size static-identifier)
  (let ((address (if static-identifier (static-address static-identifier) (new-address))))    
    (cond ((and static-identifier (static-ref address))
           => (cut values address <> #f))
          (static-identifier
           (values address (make-vector size) (cut store-static address <>)))          
          (else
           (values address (make-vector size) (cut store address <>))))))

(begin
  (let ((addr1 v1 commit1 (allocate 1 #f))
        (addr2 v2 commit2 (allocate 1 #f)))
    (assert commit1)
    (assert commit2)
    (assert (neq? v1 v2))
    (assert (eq? (commit1 v1) (store-ref addr1)))
    (assert (eq? (commit2 v2) (store-ref addr2)))

    ;; clean up
    (store-set! addr1 #f)
    (store-set! addr2 #f))  

  (let ((addr1 v1 commit1 (allocate 1 'foo)))    
    (assert commit1)
    (assert (eq? (commit1 v1) (store-ref addr1)))

    (let ((addr2 v2 commit2 (allocate 1 'foo)))      
      (assert (not commit2))
      (assert (eq? v1 v2))

      ;; clean up
      (store-set! addr1 #f)
      )))
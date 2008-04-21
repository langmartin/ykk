;; update part of a structure
(define (share foo un-foo proc new-foo)  
  (let* (((values . orig) (un-foo foo))
         ((values . new) (apply proc orig)))    
    (if (every eq? orig new)
        foo
        (apply new-foo new))))

;; generic functional data constructor that keeps ORIG when possible.
(define (shared-kons kons orig old-car old-cdr new-car new-cdr)
  (let ((new-cdr (maybe-force new-cdr)))    
    (if (and (eq? new-car old-car)
             (eq? new-cdr old-cdr))
        orig
        (kons new-car new-cdr))))

(define shared-cons
  (cut shared-kons cons <> <> <> <> <>))

;;;; Iteration

;; shared iteration primitive
(define (transform knull? kar kdr kons end sequence)
  (let loop ((seq sequence))
    (if (knull? seq)
        (end seq)
        (let* ((old-car (kar seq))
               (old-cdr (kdr seq)))          
          (maybe-force
           (kons seq old-car old-cdr (delay (loop old-cdr))))))))

(define transform-list
  (cut transform null? car cdr <> identity <>))

;; --------------------
;; TRANSFORM specializations

(define (map transform shared-cons proc children)  
  (map-matching transform shared-cons always? proc children))

(define (map-matching transform shared-cons pred? proc children)

  (define (kons seq old-car old-cdr new-cdr)
    (if (pred? old-car)
        (shared-cons seq old-car old-cdr (proc old-car) new-cdr)
        (shared-cons seq old-car old-cdr old-car new-cdr)))

  (transform kons children))

(define (map-once transform shared-cons pred? proc children)

  (define (kons seq old-car old-cdr new-cdr)
    (if (pred? old-car)
        (shared-cons seq old-car old-cdr (proc old-car) old-cdr)
        (shared-cons seq old-car old-cdr old-car new-cdr)))

  (transform kons children))

(define (map-while transform shared-cons pred? proc children)

  (define (kons seq old-car old-cdr new-cdr)
    (if (pred? old-car)
        (shared-cons seq old-car old-cdr (proc old-car) new-cdr)
        (shared-cons seq old-car old-cdr old-car old-cdr)))

  (transform kons children))

(define (remove transform shared-cons pred? children)

  (define (kons seq old-car old-cdr new-cdr)
    (if (not (pred? old-car))
        (shared-cons seq old-car old-cdr old-car new-cdr)
        new-cdr))

  (transform kons children))

;; --------------------
;; List-only

(define (map2 proc a b)
  (if (or (null? a) (null? b))
      '()
      (let ((old-car (car a))
            (old-cdr (cdr a)))        
        (shared-cons a old-car old-cdr
                     (proc old-car (car b))
                     (map2 proc old-cdr (cdr b))))))

;;;; Tests
(let ((foo '(1 2 3))
      (map (cut map transform-list shared-cons <> <>))
      (map-matching (cut map-matching transform-list shared-cons <> <> <>))
      (map-once (cut map-once transform-list shared-cons <> <> <>))
      (map-while (cut map-while transform-list shared-cons <> <> <>))
      (remove (cut remove transform-list shared-cons <> <>)))
  
  (assert (eq? (share foo identity identity identity) foo))
  (assert (not (eq? (share foo identity (lambda (x) #t) identity) foo)))
  
  (assert (eq? (map2 (lambda (a b) a) foo '(a b c)) foo))
  (assert (not (eq? (map2 (lambda (a b) (+ a b)) foo foo) foo)))

  (assert (eq? (map identity foo) foo))
  (assert (neq? (map never? foo) foo))

  (let ((mod (map-matching even? never? foo)))
    (assert mod => '(1 #f 3))
    (assert (neq? mod foo))
    (assert (eq? (cddr mod) (cddr foo))))

  (let ((mod (map-once always? never? foo)))
    (assert mod => '(#f 2 3))
    (assert (neq? mod foo))
    (assert (eq? (cdr foo) (cdr mod))))

  (let ((mod (map-while (cut < <> 3) never? foo)))
    (assert mod => '(#f #f 3))
    (assert (neq? mod foo))
    (assert (eq? (cddr foo) (cddr mod))))

  (let ((mod (remove even? foo)))
    (assert mod => '(1 3))
    (assert (neq? mod foo))
    (assert (eq? (cdr mod) (cddr foo)))))
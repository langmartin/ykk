(define &cons (make-table))

(define (make-dictionary label . args)
  (apply (lookup label &cons) args))

(define (dict-ref key obj . missing)
  (let-optionals* missing ((missing #f))
    (dict-ref* key obj missing)))

(define-generic dict-ref* &ref)

(define-generic dict-update &update)

(define-generic dict-match &match)

(define-method &ref (key (alist :alist) missing)
  (cond ((assoc key alist) => cdr)
        (else missing)))

(define-method &ref (key (tree :redblack) missing)
  (red/black-ref key tree missing))

(define-method &ref (key (table :table) missing)
  (cond ((table-ref key table) => identity)
        (else missing)))

(define-method &update ((obj alist?) . values)
  ())

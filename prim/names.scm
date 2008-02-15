;;; An implementation of name-lookup tables.
;;;
;;; At this time, names must be symbols.

;;; Public Interface
(define (make-name-table)
  (make-set car-eq? car-symbol<? #f))

(define (define-name table name binding)
  (adjoin table (cons name binding)))
(define define-names adjoin-list)
(define delete-name remove)
(define delete-names remove-list)

(define (lookup-name table name)
  (cond ((and (symbol? name)
              (set-ref table name)) => cdr)
        (else #f)))

(define (fold-names kons knil table)
  (lfold-set (lambda (pair acc)
               (kons (car pair) (cdr pair) acc))
             knil
             table))

(define merge-name-tables union)

;;; Internal Interface
(define (car-eq? a b)
  (eq? (maybe-car a) (maybe-car b)))

(define (car-symbol<? a b)
  (symbol<? (maybe-car a) (maybe-car b)))

(define (maybe-car foo)
  (if (pair? foo) (car foo) foo))

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (list-names table)
  (map car (set->list table)))

;;; Tests
(let ((table (define-names (make-name-table) '((a . a-value) (b . b-value)))))
  (assert (list-names table) => '(a b))
  (assert (lookup-name table 'b) => 'b-value))

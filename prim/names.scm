;;; An implementation of name-lookup tables, currently using red/black trees.
;;;
;;; At this time, names must be symbols.

;;; Public Interface
(define (make-name-table)
  (r/b-make-tree car-eq? car-symbol<? #f))

(define define-names r/b-insert-set)
(define delete-names r/b-delete-set)

(define (lookup-name table name)
  (cond ((and (symbol? name)
              (r/b-ref table name)) => cdr)
        (else #f)))

;;; Internal Interface

(define (car-eq? a b)
  (eq? (if (pair? a) (car a) a)
       (if (pair? b) (car b) b)))

(define (car-symbol<? a b)
  (symbol<? (if (pair? a) (car a) a)
            (if (pair? b) (car b) b)))

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(define (list-names table)
  (map car (r/b-tree/in-order->list table)))

;;; Tests
(let ((table (define-names (make-name-table) '((a . a-value) (b . b-value)))))
  (eq? (lookup-name table 'b) 'b-value)
  (list-names table))

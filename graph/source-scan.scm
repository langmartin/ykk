;;;; Scanning
(define (shallow-scan form)  
  (reverse (shallow-scan-form form (empty-top-level))))

;; Allowed top-level forms so far:
;;
;;  BEGIN: descend and scan recursively
;;  PLIST: like a LET without a body (associate names with forms)
;;  DEFINE: if there are formals transform to a lambda and associate
;;   with name.  Otherwise, just associate the form with the name.
;;  DEFINE-SYNTAX: associate name with SYNTAX-RULES or LAMBDA
;;
(define (shallow-scan-form form top-level)
  (case (car form)
    ((begin)
     (fold shallow-scan-form top-level (cdr form)))
    ((plist)
     (fold (lambda (item acc)
             (add-top-level (plist-item-name item) (plist-item-body item)
                            acc))
           top-level
           (plist-items form)))
    ((define define-syntax)
     (cond ((malformed-define? form)
            (raise-condition 'shallow-scan "malformed define" form))
           ((procedure-definition? form)
            (add-top-level (procedure-name form)
                           `(lambda ,(procedure-formals form)
                              ,@(procedure-body form))
                           top-level))
           (else
            ;; some other form
            (add-top-level (define-name form) (define-body form)
                           top-level))))
    (else
     (raise-condition 'shallow-scan "unrecognized top-level form" form))))

(define (empty-top-level) '())
(define add-top-level alist-cons)

(define plist-items cdr)
(define plist-item-name car)
(define plist-item-body cadr)

(define (procedure-definition? form)
  (and (pair? (cdr form))
       (pair? (cadr form))
       (pair? (cddr form))))
(define procedure-name caadr)
(define procedure-formals cdadr)
(define procedure-body cddr)

;; A malformed define looks like
;;  (define)
;;  (define foo)
;;  (define (foo bar baz))
(define (malformed-define? form)
  (or (not (pair? (cdr form)))
      (not (pair? (cddr form)))))

(define define-name cadr)
(define define-body caddr)

;;;; Test
(begin
  (assert (shallow-scan '(begin (define foo 1)
                                (plist (bar (lambda (a) (list a)))
                                       (baz (lambda (b) (* b 2))))
                                (define quux 'beta)))
          => '((foo . 1) (bar lambda (a) (list a)) (baz lambda (b) (* b 2)) (quux quote beta))))
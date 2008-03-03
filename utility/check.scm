;;;; Define Checked
(define-syntax define-checked
  (syntax-rules ()
    ((_ (name formals ...) body ...)
     (expand-checked (name) (formals ...) () (body ...)))
    ((_ otherwise ...)
     (define otherwise ...))))

(define-syntax expand-checked
  (syntax-rules ()
    ;; final step in the recursion, expand into a define
    ((_ ((name assert-return) formals ...) () (assertion ...) (body ...))
     (define (name formals ...)
       (check-return assert-return
                     (begin assertion ... body ...)
                     'name)))
    ((_ formals () (assertion ...) (body ...))
     (define formals assertion ... body ...))
    
    ;; fold formals into names and checks
    ((_ (name p ...) ((parameter assertion) formals ...) (check ...) body)
     (expand-checked (name p ... parameter)
                     (formals ...)
                     (check ... (check-formal assertion parameter 'name 'parameter))
                     body))
    ((_ (name ...) (parameter formals ...) check body)
     (expand-checked (name ... parameter)
                     (formals ...)
                     check
                     body))))

(define-syntax check-formal
  (syntax-rules ()
    ((_ assertion value procedure-name parameter-name)
     (check assertion
            value
            (error 'wrong-type-argument
                   `(procedure: procedure-name)
                   `(parameter: parameter-name)
                   `(assertion: <assertion>)
                   `(bad-value: ,<value>))))))

(define-syntax check-return
  (syntax-rules ()
    ((_ assertion form procedure-name)
     (check assertion
            form
            (error 'bad-return-value
                   `(procedure: procedure-name)
                   `(assertion: <assertion>)
                   `(bad-value: ,<value>))))))

(define-syntax check
  (syntax-rules ()
    ((_ assertion form error)
     (let ((value form))
       (if (check-assertion assertion value)
           value
           (wildcard #f ((<assertion> assertion) (<form> 'form) (<value> value)) error))))))

;; DANGER: this breaks hygene!
(define-syntax wildcard
  (lambda (form rename compare)    
    (let ((apply-to (cadr form))          
          (replace (map (lambda (pair) (cons (desyntaxify (car pair)) (cadr pair)))
                        (caddr form)))
          (source (cadddr form)))
      
      (define (scan form)
        (cond ((null? form) form)
              ((pair? form) (map-in-order scan form))
              ((and (generated? form) (assq (desyntaxify form) replace)) => cdr)
              ((assq form replace) => cdr)
              (else form)))      

      (if apply-to
          `(,@apply-to ,@(scan source))
          (scan source)))))

(define (check-assertion assertion value)
  (if (procedure? assertion)
      (assertion value)
      ((type-predicate assertion) value)))

;;;; Tests
(begin
  (assert (wildcard (list 'foo) ((_ 'sub) (? 'sub2)) ('a 1 `(a _) `((b ?) (c 2)))) =>
          '(foo a 1 (a 'sub) ((b 'sub2) (c 2)))))

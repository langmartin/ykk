;;;; Syntax Continuations
(define-syntax syntax-k
  (syntax-rules ()
    ((_ value (k . etc))
     (k value . etc))))

(define-syntax syntax-k/values
  (syntax-rules ()
    ((_ (value ...) (k . etc))
     (k value ... . etc))))

(define-syntax syntax-k-into
  (syntax-rules ()
    ((_ value (k ...))
     (k ... value))))

(define-syntax syntax-k-into/values
  (syntax-rules ()
    ((_ values (k ...))
     (k ... . values))))

;;;; Expansion-time Definition
(define-syntax define/expansion
  (syntax-rules ()
    ((_ (name . formals) body ...)
     (define/expansion name (lambda formals body ...)))
    ((_ name expr)
     (define-during-syntax-expansion! name expr))))

(define-syntax define/force-up
  (syntax-rules ()
    ((_ (name . formals) body ...)
     (define/force-up name (lambda formals body ...)))
    ((_ name expr)
     (begin
       (define-during-syntax-expansion! name expr)
       (force-binding-up-for-syntax! name name)))))

(define-syntax syntax/eval
  (lambda (form rename compare)
    (let (((values . result) (eval `(,(rename 'begin) ,@(cdr form)) (interaction-environment))))
      (if (null? result)
          (list (rename 'begin))
          (apply values result)))))

(define-syntax define-during-syntax-expansion!
  (lambda (form rename compare)
    (let* ((name (cadr form))
           (expr (caddr form)))
      (define-now! name expr)      
      `(,(rename 'begin)))))

(define-syntax force-binding-up-for-syntax!
  (lambda (form rename compare)
    (let* ((for-syntax-name (cadr form))
           (binding-name (caddr form)))
      (force-up! for-syntax-name binding-name)      
      `(,(rename 'begin)))))

;;;; Applicative-order Macro Transformation
(define-syntax define-syntax/applicative-order
  (syntax-rules ()
    ((_ name body)
     (define-syntax name       
       (let ((transformer body))         
         (lambda (form rename compare)
           (apply-macro-transformer transformer form rename compare)))))
    ((_ name er-lambda free)
     (define-syntax/applicative-order er-lambda))))

;;;; SRFI-89 Macros
(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (name . formals) . body)     
     (srfi-89/define name formals (begin . body) srfi-89/terminate))
    ((_ (name . formals) body)
     (srfi-89/define name formals body srfi-89/terminate))))

(define-syntax srfi-89/define
  (lambda (form rename compare)
    
    (define %A (rename (uuidgen)))
    (define %define-syntax (rename 'define-syntax))
    (define %rules (rename 'syntax-rules))
    (define %syntax-k (rename 'syntax-k))
    
    (let ((name (cadr form))
          (formals (caddr form))
          (body (cadddr form))
          (terminate (cadr (cdddr form)))
          (allowed `((rest: ,(rename 'srfi-89/rest))
                     (no-rest: ,(rename 'srfi-89/no-rest))
                     (optional: ,(rename 'srfi-89/optional-parameters))
                     (positional: ,(rename 'srfi-89/required-parameters))
                     (named: ,(rename 'srfi-89/named-parameters)))))

      (let* ((T stack (srfi-89:parse-formals (cons 'quote name) formals))
             (k-expand `(,terminate ,T (,name ("expanded"))))
             (k (srfi-89:stack->k allowed k-expand stack)))        
        `(,%define-syntax ,name
          (,%rules ()
                   ((_ ("expanded") ,@T) ,body)
                   ((_ . ,%A)
                    (,%syntax-k (',name ,%A ()) ,k))))))))

(define-syntax srfi-89/required-parameters
  (lambda (form rename compare)
    (let ((name A V (unlist (cadr form)))
          (n (caddr form))
          (k (cadddr form)))
      (let ((A V (srfi-89:require-positionals name A V n)))
        (continue (list name A V) k)))))

(define-syntax srfi-89/optional-parameters
  (lambda (form rename compare)
    (let ((name A V (unlist (cadr form)))
          (options (caddr form))
          (k (cadddr form)))
      (let ((A V (srfi-89:optional-positionals name A V options)))        
        (continue (list name A V) k)))))

(define-syntax srfi-89/named-parameters
  (lambda (form rename compare)
    (let ((name A V (unlist (cadr form)))
          (named (map-car desyntaxify (caddr form)))
          (k (cadddr form)))
      (let ((A V (srfi-89:named-parameters name A V named)))
        (continue (list name A V) k)))))

(define-syntax srfi-89/rest
  (lambda (form rename compare)
    (let ((name A V (unlist (cadr form)))
          (k (caddr form)))
      (continue (append V (list A)) k))))

(define-syntax srfi-89/no-rest
  (lambda (form rename compare)
    (let ((name A V (unlist (cadr form)))
          (k (caddr form)))
      (if (not (null? A))
          (error/syntax 'unexpected-rest-arguments name A)          
          (continue V k)))))

;; NOTE: beta-substitution is possibly wrong here since it could lead
;; to duplicate evaluation.
(define-syntax srfi-89/terminate
  (lambda (form rename compare)
    (let ((V (cadr form))
          (T (caddr form))
          (k (cadddr form)))

      (define (lookup value E)
        (find (lambda (pair) (compare (car pair) value))
              E))      

      ; FIXME: at the very least, this is definitely the wrong thing
      ; to do unless forms passed in as arguments are not
      ; beta-substituted this way.

      ; (continue-into/values (beta-substitute lookup T V) k)
      
      (continue-into/values V k)
      )))

;; --------------------
;; Internal

(define-syntax quote-list
  (syntax-rules ()
    ((_ . A)
     'A)))

;;;; Misc.
(define-syntax syntax/quote-non-literal
  (lambda (form rename compare)
    (let ((foo (cadr form)))
      (quote-non-literal foo))))

;;;; Tests
(begin

  ;; ------------------------------------------------------------
  ;; Syntax Continuation

  (assert (syntax-k '(1 2) (list 3)) => '((1 2) 3))
  (assert (syntax-k/values (1 2 3) (list 4)) => '(1 2 3 4))
  (assert (syntax-k-into '(1 2 3) (list 0)) => '(0 (1 2 3)))
  (assert (syntax-k-into/values (1 2 3) (list 0)) => '(0 1 2 3))  

  ;; ------------------------------------------------------------
  ;; Expansion-time Definition

  (syntax/eval
   (define (make-foo . args)
     args))

  ;; MAKE-FOO is now bound to a procedure in the environment
  
  (define-syntax do-something-with-foo
    (lambda (form rename compare)
      (let* ((foo-name (cadr form))
             (foo (definition-value foo-name)))
        ;; The `run-time' value of FOO can be operated on.
        `(,(rename 'list) ,@(map (lambda (item) (+ 1 item)) foo)))))  

  (define-syntax expand-foo
    (syntax-rules ()
      ((_ name thing stuff ...)
       (begin
         ;; This will bind NAME to the result of evaluating (MAKE-FOO stuff ...)
         (define/expansion name (make-foo stuff ...))
         ;; DO-SOMETHING-WITH-FOO is passed NAME; it can dereference
         ;; NAME in the environment to access value.
         (define thing (do-something-with-foo name))))))  

  (expand-foo bar baz (+ 1 2) (+ 3 4))
  (assert bar => '(3 7))
  (assert baz => '(4 8))  

  ;; ------------------------------------------------------------
  ;; Applicative-order macros
  
  (define-syntax/applicative-order quux
    (syntax-rules ()
      ((_ a) 'a)))
  
  (define-syntax frob
    (syntax-rules ()
      ((_ a) 'a)))  

  (define-syntax mumble
    (syntax-rules ()
      ((_ a) a)))

  ;; QUUX and FROB have the same form, but QUUX is applicative order.
  ;; Note the difference in the results.
  (assert (quux (mumble 1)) => 1)
  (assert (frob (mumble 1)) => '(mumble 1))

  (assert
   (srfi-89/required-parameters (foo (1 2 3) ()) 2 (quote-list))
   => '((foo (3) (1 2))))

  (assert
   (srfi-89/optional-parameters (foo (1 2 3) ()) (a b c d e) (quote-list))
   => '((foo () (1 2 3 d e))))

  (assert
   (srfi-89/named-parameters (foo (c: 2 mumble quux) ()) ((a 'a-value) (c) (d 'd-value)) (quote-list))
   => '((foo (mumble quux) ('a-value 2 'd-value))))

  (assert
   (srfi-89/rest (foo (mumble quux) (a b c)) (quote-list))
   => '((a b c (mumble quux))))

  (assert
   (srfi-89/no-rest (foo () (a b c)) (quote-list))
   => '((a b c)))

;;   (assert
;;    (srfi-89/terminate
;;     (1 (+ a 1))
;;     (a b)
;;     (quote-list))
;;    => '(1 (+ 1 1)))  

;;   (define-syntax* (waffle a (b 1) (c: c (+ b 1)))
;;     (list a b c))
  
;;   (assert (waffle 'flub) => '(flub 1 2))
  )
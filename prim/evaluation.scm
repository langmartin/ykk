;;;; Integration with the Scheme48 evaluator

;;;; This code is concerned with adding an evaluation hook for a ykk
;;;; environment to be referenced and providing a "safe" overall
;;;; environment for evaluation.

;;;; The scheme48 evaluator expects the following arguments: (FORM
;;;; PACKAGE).  FORM is evaluated in the environment PACKAGE and the
;;;; result of the evaluation is returned.

;;;; The evaluation hook is added by modifying the default "evaluation
;;;; environment" exported by a scheme48 package (see
;;;; bcomp/package.scm and bcomp/cenv.scm).  The export procedure
;;;; (PACKAGE->ENVIRONMENT), which is actually just a record slot
;;;; accessor, is called by COMPILE-AND-RUN in rts/eval.scm.

;;;; A safe environment, at this time, is defined to be R5RS.  The
;;;; process of safe evaluation is, essentially, to provide an empty
;;;; R5RS environment that is "closed over" a ykk environment.  The
;;;; empty R5 environment is modified destructively during evaluation
;;;; to contain the top-level bindings in FORM.

;;; Public
(define (safe-evaluation-environment ykk-env)
  (let ((new (s48-make-simple-package (list *safe-scheme-structure*)
                                      #t
                                      *tower*)))
    (set-package->environment! new (really-package->environment ykk-env new *tower*))
    new))

;; Evaluate FORM in YKK-ENV.
(define (safe-eval form ykk-env)
  (call-with-values
      (lambda () (%safe-eval form ykk-env))
    (lambda (result env)
      result)))

(define (eval/extend form ykk-env export?)
  (extend-environment ykk-env
                      (s48-package->ykk-definitions (safe-eval->env form ykk-env)
                                                    export?)))

;;; Internal
(define (safe-eval->env form ykk-env)
  (call-with-values
      (lambda () (%safe-eval form ykk-env))
    (lambda (result env)
      env)))


(define (s48-package->ykk-definitions env export?)
  (let ((defs '()))    
    (s48-for-each-definition (lambda (name binding)
                               (if (export? name binding)                                   
                                   (set! defs (cons (list name binding)
                                                    defs))))
                             env)
    defs))

;;; Package-internal

;; FIXME: make a custom scheme environment based on r5rs that removes
;; I/O, dangerous operations, etc.
(define *safe-scheme* (scheme-report-environment 5))
(define *safe-scheme-structure* (car (s48-package-opens *safe-scheme*)))
(define *tower* (s48-make-reflective-tower s48-eval (list *safe-scheme-structure*) 'ykk))

;; Here, we have a bit of kludge to replace the normal
;; package->environment with one that will first check for bindings in
;; a given YKK environment.

(define (set-package->environment! package proc)
  (record-set! package 10 proc))

;; verbatim from bcomp/package.scm except for the lookup procedure (first lambda) and s48 prefixes
(define (really-package->environment ykk-env package tower)
  (make-compiler-env (lambda (name)
                       ;; Lookup is in this order:
                       ;;   1. First check only in PACKAGE
                       ;;   2. Check in YKK-ENV
                       ;;   3. (search-opens NAME PACKAGE)
                       (really-lookup ykk-env package name (s48-package-integrate? package)))
		     (lambda (name type . maybe-static)
		       (s48-package-define! package
                                            name
                                            type
                                            #f
                                            (if (null? maybe-static)
					    #f
					    (car maybe-static))))
		     tower
		     package))	; interim hack

(define (really-lookup ykk-env package name integrate?)
  (let ((probe (s48-package-definition package name)))
    (cond (probe
           (if integrate?
               probe
               (s48-forget-integration probe)))
          ((s48-generated? name)
           ; Access path is (generated-parent-name name)
           (s48-generic-lookup (s48-generated-env name)
                               (s48-generated-name name)))
          (else
           (carefully ykk-env name
                      binding->s48-binding
                      (lambda () (search-opens (s48-package-opens-really package)
                                               name
                                               integrate?)))))))

;; verbatim from bcomp/package.scm except for s48 prefixes
(define (search-opens opens name integrate?)
  (let loop ((opens opens))
    (if (null? opens)
	#f
	(or (s48-structure-lookup (car opens) name integrate?)
	    (loop (cdr opens))))))

;; The resulting environment can be inspected.  It will contain the
;; top-level definitions in FORM.  One could use
;; S48-FOR-EACH-DEFINITION to iterate over the bindings in ENV.
(define (%safe-eval form ykk-env)
  (let ((env (safe-evaluation-environment ykk-env)))
    (values (s48-eval form env) env)))

;;; Tests

(define *test-env* (extend-environment (empty-environment)
                                       `((a ,:symbol av) (b bv))))

;; closures work
((safe-eval
   '(begin
      (define-syntax foo
        (syntax-rules ()
          ((_ a)
           '(foo a))))
      (define thing (foo bar))
      (define b 'something-else)
      (lambda ()
        (list thing a b)))
   *test-env*))

(define *evaled-package*
  (safe-eval->env
   '(begin
      (define-syntax foo
        (syntax-rules ()
          ((_ a)
           '(foo a))))
      (define thing (foo bar))
      (define b 'something-else)
      thing)
   *test-env*))

;; what's inside?
(s48-package->ykk-definitions *evaled-package* (lambda x #t))

;; equivalent of eval/extend
(extend-environment
 *test-env*
 (s48-package->ykk-definitions
  *evaled-package*
  (lambda x #t)))


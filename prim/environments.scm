;;;; An implementation of first-class, functionally updatable
;;;; environments.
;;;;
;;;; Environments are a mapping of names (see names.scm) to bindings
;;;; (see bindings.scm).  Operations on environments include: name
;;;; lookup, adding names, removing names, and casting bindings.

;;; Public

;; Environments are tracked through the dynamic environment

(define-fluid ($current-environment #f)
  current-environment
  with-current-environment)

(define (lookup name)
  (environment-ref (current-environment) name))

;; Some basic operations

(define (environment-ref env name)
  (binding-value (%environment-ref env name)))

(define (extend-environment env definitions)
  (map->evolved (lambda (def)
                  ;; def looks like: (NAME VALUE) or (NAME TYPE VALUE)
                  (cond ((not (pair? def))
                         (error "extend-environment: malformed definition ~S" def))
                        ((not (null? (cddr def)))                         
                         (cons (car def)
                               (apply new-binding def)))
                        (else
                         (cons (car def)
                               (apply new-value-binding def)))))
                env definitions))

(define (forget-bindings env names)
  (evolve env (delete-names (package-definitions env)
                            names)))

(define (cast-bindings env name/type-pairs)
  (map->evolved (lambda (pair)
                  (cons (car pair)
                        (cast-binding (%environment-ref env (car pair))
                                      (cdr pair))))                
                env name/type-pairs))

;;; Internal
(define (%environment-ref env name)
  (carefully env name identity (lambda () (error "undefined variable ~S in ~S" name env))))

;; FIXME: this should be as careful as CAREFULLY in rts/env.scm
(define (carefully env name found not-found)
  (let ((probe (lookup-name (package-definitions env) name)))
    (cond ((not probe)
           (not-found))
          ((not (binding? probe))
           (error "environment-ref: strange binding ~S in ~S" probe env))
          (else
           (found probe)))))

;;; Package-internal

;; An environment is actually a package
(define-record-type rtd/package :ykk/package
  (really-make-package uid definitions all)
  package?
  (uid package-uid)
  (definitions package-definitions)
  (all all-packages set-all-packages!))

(define-record-discloser :ykk/package
  (lambda (rec)
    `(env ,(package-uid rec) ,(list-names (package-definitions rec)))))

;; A package knows about itself and any previous packages.  In the S48
;; implementation, this part is actually a global table.  We keep a
;; functionally updatable record inside each package in this
;; implementation.

(define remember-package cons)
(define (empty-package-set) '())

(define (make-package definitions all-packages)
  (let* ((uuid (uuidgen))
         (package (really-make-package uuid definitions #f)))
    (set-all-packages! package
                       (remember-package package all-packages))
    (make-immutable! package)))

(define (empty-environment)
  (make-package (make-name-table) (all-packages (current-environment))))

(define (ground-level-package)
  (make-package (make-name-table) (empty-package-set)))

(define (evolve old definitions)
  (make-package definitions (all-packages old)))

(define (map->evolved proc env lst)
  (evolve env (define-names (package-definitions env)
                (map proc lst))))

(set-fluid! $current-environment (ground-level-package))


;;; Tests
(empty-environment)
(define *test-env* (extend-environment (empty-environment)
                                       `((a ,:symbol av) (b bv))))

(with-current-environment
 *test-env*
 (lambda ()
   (lookup 'a)))

;; (with-current-environment
;;  (forget-bindings *test-env* '(a))
;;  (lambda ()
;;    (lookup 'a)))

(with-current-environment
 (cast-bindings *test-env* `((a . ,:symbol)))
 (lambda ()
   (lookup 'a)
   (binding-type (%environment-ref (current-environment) 'a))))

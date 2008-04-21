;; from $Id: new-shift.scm,v 1.2 2006/04/07 07:08:17 oleg Exp $

(define-syntax reset
  (syntax-rules ()
    ((_ ?e) (*reset (lambda () ?e)))))

(define-syntax shift
  (syntax-rules ()
    ((_ ?k ?e) (*shift (lambda (?k) ?e)))))

; see the following for details
; scheme48-1.3.tgz!scheme48-1.3/scheme/rts/fluid.scm
; as well as wind.scm in the same directory

(define (get-dynamic-env)
  (record-ref (current-thread) 1))

(define (set-dynamic-env! env)
  (record-set! (current-thread) 1 env))

(define (get-dynamic-point)
  (record-ref (current-thread) 2))

(define (set-dynamic-point! point)
  (record-set! (current-thread) 2 point))

;;;; we should also deal with proposals -- but we skip those for now

; Our meta-continuation is the triple:
;  a function to call with the value (which restores the prev meta-cont)
;  winding continuation of the closest reset
;  and the dynamic env of that reset
(define *meta-continuation*
  (vector
   (lambda (v)
      (error "You forgot the top-level reset...")) ;0
    'no-reset				; 1 full-k
    '()                                 ; 2 dynamic-env
))

; reset the meta-continuation stack
(define (*abort-pure v)
  ((vector-ref *meta-continuation* 0) v))

; execute the thunk in the dynamic env of the closest reset
(define (*abort-env thunk)
  ((vector-ref *meta-continuation* 1) thunk))

(define (*reset thunk)
  (let ((mc *meta-continuation*)
	(env (get-dynamic-env))
	(pt  (get-dynamic-point)))
    (set-dynamic-point! #f)
    (primitive-cwcc
      (lambda (k-pure)
	(*abort-pure
	  ((call-with-current-continuation
	     (lambda (k)
	       (set! *meta-continuation*
		 (vector
                  (lambda (v)
		     (set! *meta-continuation* mc)
		     (set-dynamic-env! env)
		     (set-dynamic-point! pt)
		     (with-continuation k-pure (lambda () v)))
		   k
		   env))               
	       thunk))))))))

; compute the list difference between l and lsuffix
(define (unwind-env l lsuffix)
  (if (eq? l lsuffix) '()
    (cons (car l) (unwind-env (cdr l) lsuffix))))
    
(define (*shift f)
  (let* ((parent-env (vector-ref *meta-continuation* 2))
	 (curr-env   (get-dynamic-env))
	 (diff-env   (unwind-env curr-env parent-env)))

    (call-with-values
        (lambda ()
          (call-with-current-continuation
           (lambda (k)
             (*abort-env (lambda ()
                           (f (lambda v
                                (reset (apply k v)))))))))
      (lambda v
        (set-dynamic-env! (append diff-env (vector-ref *meta-continuation* 2)))
        (apply values v)))))

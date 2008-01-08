(define process-list (top-ref 'process-list))

(define (running-list)
  (car (top-ref 'process-list)))

(define (blocked-list)
  (cdr (top-ref 'process-list)))

(define (update running blocked)
  (top-set
   'process-list
   (cons running
         blocked)))

(define (proc-insert process lst)
  (r/b-insert
   lst
   process))

(define (proc-delete process lst)
  (r/b-delete
   lst
   process))

(define-record-type process
  (make-process resource auth)
  process?
  (resource process-resource)
  (auth process-auth))

(define (process-start resource auth)
  (update
   (proc-insert (make-process resource auth) (running-list))
   (blocked-list)))

(define (process-block proc)
  (update
   (proc-delete proc (running-list))
   (proc-insert proc (blocked-list))))

(define (process-free proc)
  (update
   (proc-insert proc (running-list))
   (proc-delete proc (blocked-list))))

(define (fold-procs cons nil lst)
  (if (null? lst)
      nil
      (let ((lst1
             (fold-procs cons nil (cdr lst))))
        (if (eq? lst1 lst) lst lst1))))

(define (fold-running cons nil)
  (fold-procs cons nil (running-list)))

(define (fold-blocked cons nil)
  (fold-procs cons nil (blocked-list)))

;; (define (depth-first-procs cons lst)
;;   (depth-first cons lst))

;; (define (depth-first-running cons)
;;   (depth-first-procs cons (running-list)))

;; (define (depth-first-blocked cons)
;;   (depth-first-procs cons (blocked-list)))


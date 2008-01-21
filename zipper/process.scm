(define (process-list) top:processes)

(define (running-list)
  (car (process-list)))

(define (blocked-list)
  (cdr (process-list)))

(define (update running blocked)
  (persistent-symbol-set!
   top:processes
   (cons running
         blocked)))

(define proc-insert cons)

(define (proc-delete process lst)
  (filter (lambda (x)
            (not (eq? x process)))
          lst))

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

;;;; processes themselves

(define (process-list) ykk:processes)

(define (running-list)
  (car (process-list)))

(define (blocked-list)
  (cdr (process-list)))

(define (update running blocked)
  (ykk:persistent-symbol-set!
   ykk:processes
   (cons running
         blocked)))

(define proc-insert cons)

(define (proc-delete process lst)
  (filter (lambda (x)
            (not (eq? x process)))
          lst))

(def-record rtd/process
  (make-proc
   proc?
   proc-update
   proc-apply)
  proc-zipper
  proc-auth)

(def-discloser ((proc :rtd/process))
  `(proc ,(proc-zipper proc)))

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

;; (define (fold-procs cons nil lst)
;;   (if (null? lst)
;;       nil
;;       (let ((lst1
;;              (fold-procs cons nil (cdr lst))))
;;         (if (eq? lst1 lst) lst lst1))))

;; (define (fold-running cons nil)
;;   (fold-procs cons nil (running-list)))

;; (define (fold-blocked cons nil)
;;   (fold-procs cons nil (blocked-list)))

(define running car)
(define blocked cadr)

(define (block proc acc)
  (cons (cons proc (blocked acc))
        acc))

(define (keep proc acc)
  (cons (cons proc (running acc))
        acc))

(define (process-fold)
  (let ((new 
         (fold-append (lambda (proc acc)
                        (let ((new (advance proc)))
                          (if (blocked? proc)
                              (block proc acc)
                              (keep proc acc))))
                      '()
                      (running-list)
                      (blocked-list))))
    (update (running new)
            (blocked new))))

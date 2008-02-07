(define-fluid (*current-process* #f)
  current-process
  with-process)

(define (syscall thunk)
  (let* ((proc (current-process))
         (thread
          (spawn
           (lambda ()
             (unblock-process
              proc
              (thunk))))))
    (block-process
     proc
     thread)))

(define (bury! running blocked)
  (ykk:persistent-symbol-set!
   ykk:processes
   (cons running
         blocked)))

(define (block-process proc thread)
 (let ((tag (uuidgen)))
   (values (dict-set (blocked) tag (cons proc thread))
           tag)))

(define (unblock-process tag value)
  (let ((proc (dict-ref (blocked) tag)))
    (dict-del (blocked) tag)
    ))

(define (advance-queue)
  (let fold ((queue '()) (trees '())
             (tail (running-process-queue)))
    (if (null? tail)
        (begin
          (update-data-tree (merge-all-trees trees))
          queue)
        (let ((next (advance-proc proc)))
          (if (cursor? next)
              (fold (cons next queue) trees (cdr tail))
              (fold queue (cons next trees) (cdr tail)))))))

(define (advance-proc proc)
  #f)

(define (cursor? obj)
  (and (process? obj)
       (zipper? (proc-zipper obj))))

(define (send/suspend proc)
  (call-with-current-continuation
   (lambda (k)
     (block-process (current-process) k))))

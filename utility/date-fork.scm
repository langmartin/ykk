;;;; This is a hacky implementation that forks date, and assumes GNU date, moreover.

(define (date-format format time)
  (fork-date "-d" (concat #\@ (time-seconds time))
             (concat #\+ format)))

(define (date-parse string)
  (make-time (fork-date "-d" string "+%s")))

(define (fork-date . args)
  (backtick
   "date"
   (or (and-let* ((tz (current-timezone)))
         (list (concat "TZ=" tz)))
       '())
   args))

(define (sh->stdout program env args)
  (receive
   (input output)
   (pipe)
   (let ((pid (fork)))
     (if pid
         (begin
           (wait-for-child-process pid)
           (values (process-id-exit-status pid)
                   input))
         (let-current-output-port
                 output
               (if (pair? env)
                   (exec-with-environment program env args)
                   (exec program args)))
         ))
   
   (let ((pid
          (fork
           (lambda ()
             ))))
     )))

(define (backtick program env args)
  (receive
   (status port)
   (sh->stdout program env args)
   (if (not (zero? status))
       (e-non-zero status))
   (port-slurp port)))

(define (*log*) (current-error-port))

(define (initialize-log file-name)
  (set! *log*
        (lambda ()
          (force (delay (open-output-file file-name))))))

(define (log-cell cell . port)
  (let-optionals* port ((port (*log*)))
    (write (list (rec-id cell)
                 (rec-car cell)
                 (or (rec-cdr-tag cell)
                     (rec-cdr cell)))
           port)
    (newline port)))

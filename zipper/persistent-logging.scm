(define-record-type log rtd/log
  (make-log tag)
  log?
  (tag log-tag)
  (port log-port set-log-port!))

(define-fluid (*log* #f)
  current-log
  with-log)

(define (set-log! log)
  (set-fluid! *log* log))

(define-syntax without-log
  (syntax-rules ()
    ((_ body ...)
     (with-log #f (lambda () body ...)))))

(define (reopen-log-port port)
  (if (current-log)
      (close-output-port (current-log)))
  (set-log! port))

(define (initialize-log tag)
  (let* ((log (make-log tag))
         (path (concat "/tmp/ykk-log-" tag)))
    (if (file-exists? (log-tag log))
        (error "log file exists" log)
        (open-output-file log)
        (set-log! (cons log )))))

(define (rotate-log)
  ())

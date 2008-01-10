(define (curl* args)
  (receive
   (input output)
   (open-pipe)
   (let ((pid (fork)))
     (cond (pid (curl-parent pid input output))
           (else
            (remap-file-descriptors!
             #f
             output
             (current-error-port))
            (apply exec "curl" args))))))

(define (curl-parent pid port child-port)
  (wait-for-child-process pid)
  (if (zero? (process-id-exit-status pid))
      (begin
        (close-port child-port)
        port)
      (error "curl exit " (process-id-exit-status pid))))

(define (curl-ssl->string url . args)
  (let ((port (curl* `("-ksi" "--raw" ,@args ,url))))
    (let-current-input-port
        port
      (char-ready?)
      (let-string-output-port
       (let lp ()
         (let ((ch (read-char)))
           (if (and (char-ready?)
                    (not (eof-object? ch)))
               (begin
                 (display ch)
                 (lp))
               (close-input-port
                (current-input-port)))))))))

(define (curl-data* alist k)
  (let ((data
         (fold (lambda (x acc)
                 `("-d" ,(concat (urlencode-string (car x))
                                 #\=
                                 (urlencode-string (cdr x)))
                   ,@acc))
               '()
               alist)))
    (k data)))

(curl-data*
 '(("user" . "foo bar")
   ("pass" . "coptix"))
 (lambda (args)
   (apply curl-ssl->string "https://login.coptix.com/" args)))

;; (exec "curl" "-k" "https://login.coptix.com/")

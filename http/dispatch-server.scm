(define (http-server-exit)
  (http-server-exec
   (lambda () (exit 0))
   (let-http-response (220 "ok")
     (let-headers ((content-type "text/plain"))
       (let-content-length
        "stopping server...")))))

(define (http-server-restart)
  (http-server-exec
   (lambda ()
     (exit 1))
   (let-http-response (220 "ok")
     (let-headers ((content-type "text/plain"))
       (let-content-length
        "restarting server...")))))

(define (dispatch-handler dispatcher)
  (lambda (version method path port)
    (let* ((url (parse-url path))
           (host (url-host url))
           (path (url-path url)))
      (cond
       ((or (equal? "/stop" path)) (http-server-exit))
       ((equal? "/restart" path) (http-server-restart))
       (else (dispatcher url))))))

(define (dispatch-server dispatcher port)
  (display (string-append "Starting server (port " (number->string port) ") [hit '/stop' to quit, '/restart' to restart]...\n"))
  (http-server 'ip port (dispatch-handler dispatcher)))

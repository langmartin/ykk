(define (with-output-to-string string thunk)
  (call-with-string-output-port
   (lambda (port)
     (call-with-current-output-port
      port
      thunk))))

(define (call-with-output-string string receiver)
  (call-with-string-output-port
   receiver))

(define (with-input-from-string string thunk)
  (call-with-current-input-port
   (make-string-input-port string)
   thunk))

(define (port? obj)
  (or (input-port? obj)
      (output-port? obj)))

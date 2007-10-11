(define-syntax with-output-to-string
  (syntax-rules ()
    ((_ "" thunk)
     (call-with-string-output-port
      (lambda (port)
        (wind-fluid
         current-output-port
         set-current-output-port!
         port
         thunk))))))

(define-syntax call-with-output-string
  (syntax-rules ()
    ((_ "" receiver)
     (call-with-string-output-port
      receiver))))

(define-syntax with-input-from-string
  (syntax-rules ()
    ((_ string thunk)
     (call-with-current-input-port
      (make-string-input-port string)
      thunk))))

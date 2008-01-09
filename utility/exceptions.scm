(define (with-exception-catcher catcher thunk)
  ((call-with-current-continuation
    (lambda (k)
      (lambda ()
        (with-handler
         (lambda (c propagate)
           (k (lambda () (catcher c propagate))))
         thunk))))))

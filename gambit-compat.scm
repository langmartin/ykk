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

(define (call-with-input-string string receiver)
  (receiver (make-string-input-port string)))

(define (read-line . rest)
  (let-optionals* rest ((port (current-input-port))
                        (separator #\newline)
                        (include-separator? #f))
    (if separator
        (next-chunk separator port include-separator?)
        (next-chunk not-eof-object? port))))

(define (read-all . rest)
  (let-optionals* rest ((reader read)
                        (port (current-input-port)))
    (let lp ()
      (let ((current (reader port)))
        (if (eof-object? current)
            '()
            (cons current
                  (lp)))))))

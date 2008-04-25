(define (disp-for-each writer rest)
  (let-port-rest rest (port (current-output-port))
    (for-each (lambda (x)
                (writer x port))
              rest)))

(define (disp . rest)
  (disp-for-each display rest))

(define (writ . rest)
  (disp-for-each write rest))

(define (output-atom writer x)
  (cond ((list? x)
         (output-for-each writer x))
        ((pair? x)
         (output-atom writer (car x))
         (output-atom writer (cdr x)))
        ((procedure? x)
         (x))
        ((boolean? x) #f)
        ((null? x) #f)
        ((eq? x (if #f #f)))
        (else
         (writer x))))

(define (output-for-each writer lst)
  (for-each (lambda (x)
              (output-atom writer x))
            lst))

(define (output . rest)
  (let-port-rest rest (port '())
    (let-maybe-current-output-port
        port
      (output-for-each display rest))))

(define (output->string . stream)
  (let-string-output-port
   (apply output stream)))

(assert
 (call-with-string-output-port
  (lambda (p)
    (output p 1 2 3 '(2 3) 5 6))) => "1232356")

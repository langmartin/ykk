(define (optional rest default)
  (if (null? rest)
      default
      rest))

(define (port? obj)
  (or (input-port? obj)
      (output-port? obj)))

(define (port-dot-rest port/data rest default-port)
  (if (port? port/data)
      (values port/data rest)
      (values default-port (cons port/data rest))))

(define (wind-fluid getter setter value thunk)
  (let ((previous (getter)))
    (dynamic-wind
        (lambda ()
          (setter value))
        thunk
        (lambda ()
          (setter previous)))))

(define (next-chunk delims/proc port)
  (let* ((proc
          (if (procedure? delims/proc)
              delims/proc
              (let ((p (string-or-chars->predicate delims/proc)))
                (lambda (c)
                  (if (eof-object? c)
                      #f
                      (not (p c))))))))
    (call-with-string-output-port
      (lambda (out)
        (let lp ()
          (if (proc (peek-char port))
              (begin
                (display (read-char port) out)
                (lp))))))))

(define (string-or-chars->predicate obj)
   (cond ((PROCEDURE? obj)
          obj)
         ((LIST? obj)
          (lambda (c)
            (memq c obj)))
         ((STRING? obj)
          (string-or-chars->predicate
           (string->list obj)))
         ((CHAR? obj)
          (lambda (c)
            (char=? c obj)))
         (ELSE
          error "can't use " obj)))

(define (crlf? port)
  (define (look ch)
    (and (char-ready? port)
         (char=? ch(peek-char port))
         (read-char port)))
  (and (look #\return)
       (look #\newline)))

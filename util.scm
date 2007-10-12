(define-syntax if-car
  (syntax-rules ()
    ((_ rest default)
     (if (null? rest)
         default
         (car rest)))))

(define-syntax if-cadr
  (syntax-rules ()
    ((_ rest default)
     (if (or (null? rest) (null? (cdr rest)))
         default
         (cadr rest)))))

(define (optional rest default)
  (if (null? rest)
      default
      (car rest)))

(define (port-dot-rest port/data rest default-port)
  (if (port? port/data)
      (values port/data rest)
      (values default-port (cons port/data rest))))

(define (string/port->port obj)
  (if (port? obj)
      obj
      (make-string-input-port obj)))

(define (wind-fluid getter setter value thunk)
  (let ((previous (getter)))
    (dynamic-wind
        (lambda ()
          (setter value))
        thunk
        (lambda ()
          (setter previous)))))

(define-syntax unless
  (syntax-rules ()
    ((_ test body ...)
     (or test
         (begin
           body ...)))))

(define-syntax when
  (syntax-rules ()
    ((_ test body ...)
     (unless (not test)
             body ...))))

#;
(define-syntax and-let*
  (syntax-rules ()
    ((_ ((binding expr)) body ...)
     (let ((binding expr))
       (and binding
            (begin body ...))))
    ((_ (expr) body ...)
     (and expr
          (begin body ...)))
    ((_ (claw1 claw2 ...) body ...)
     (and-let* (claw1)
               (and-let* (claw2 ...)
                         body ...)))))

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

(define-functional-test
  (next-chunk "%+" (make-string-input-port "hello%20there")) "hello")

(define (crlf? port)
  (define (look ch)
    (and (char-ready? port)
         (char=? ch(peek-char port))
         (read-char port)))
  (and (look #\return)
       (look #\newline)))

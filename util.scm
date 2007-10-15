(define-syntax assert
  (syntax-rules (=>)
    ((_ expr => expected)
     (let ((result expr))
       (or (equal? result expected)
           (error (concat "assertion " expected) result))))
    ((_ e1 => r1 e2 ...)
     (begin (assert e1 => r1)
            (assert e2 ...)))
    ((_ expr)
     (let ((result expr))
       (or result (error "assertion" result))))
    ((_ e1 e2 ...)
     (begin (assert e1)
            (assert e2 ...)))))

(define-syntax if-car
  (syntax-rules ()
    ((_ rest default)
     (if (null? rest)
         default
         (car rest)))))

(define-syntax if-cdr
  (syntax-rules ()
    ((_ lst)
     (if (pair? lst)
         (cdr lst)
         '()))))

(define-syntax let-optionals*
  (syntax-rules ()
    ((_ rest ((name val))
        body ...)
     ((lambda (name)
        body ...)
      (if-car rest val)))
    ((_ rest (e1 e2 ...) body ...)
     (begin
       (let-optionals* rest (e1)
                       (let-optionals* (if-cdr rest) (e2 ...)
                                       body ...))))))

(define-syntax let-optionals
  (syntax-rules ()
    ((_ arg ...)
     (let-optionals* arg ...))))

(assert
 (let-optionals '(a b) ((port "a") (string "b")) (list port string)) => '(a b))

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

(assert (next-chunk "%+" (make-string-input-port "hello%20there")) => "hello")

(define (crlf? port)
  (define (look ch)
    (and (char-ready? port)
         (char=? ch(peek-char port))
         (read-char port)))
  (and (look #\return)
       (look #\newline)))

(define (concat . things)
  (call-with-string-output-port
   (lambda (port)
     (for-each (lambda (x)
                 (display x port))
               things))))

(define-syntax assert
  (syntax-rules (=>)
    ((_ expr => expected)
     (let ((result expr))
       (or (equal? result expected)
           (error (concat (concat-write 'expr) " => " expected)
                  result))))
    ((_ e1 => r1 e2 ...)
     (begin (assert e1 => r1)
            (assert e2 ...)))
    ((_ expr)
     (let ((result expr))
       (or result (error (concat-write 'expr) #f))))
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

(define-syntax call/datum-rest
  (syntax-rules ()
    ((_ rest test? default receiver)
     (if (test? (car rest))
         (receiver (car rest) (cdr rest))
         (receiver default rest)))))

(define-syntax call/port-rest
  (syntax-rules ()
    ((_ rest default receiver)
     (call/datum-rest rest port? default receiver))))

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

(define (next-chunk-display delims/proc port output-port . rest)
  (let-optionals* rest ((output-delimiter-too #f))
    (let* ((proc
            (if (procedure? delims/proc)
                delims/proc
                (let ((p (string-or-chars->predicate delims/proc)))
                  (lambda (c)
                    (if (eof-object? c)
                        #f
                        (not (p c))))))))
      (let lp ()
        (if (proc (peek-char port))
            (begin
              (display (read-char port) output-port)
              (lp))
            (if output-delimiter-too
                (display (read-char port) output-port)))))))

(define (next-chunk delims/proc port . rest)
  (call-with-string-output-port
   (lambda (output-port)
     (next-chunk-display delims/proc port output-port (if-car rest)))))

(let ((mp make-string-input-port))
  (assert
   (next-chunk "%+" (mp "hello%20there")) => "hello"
   (next-chunk " " (mp "foo bar") #t) => "foo "
   (next-chunk " " (mp "foobar")) => "foobar"))

(define (not-eof-object? obj)
  (not (eof-object? obj)))

(define (port-slurp port)
  (next-chunk not-eof-object?
              port))

(define (disp-primitive writer rest)
  (call/port-rest rest (current-output-port)
    (lambda (port rest)
      (for-each (lambda (x)
                  (writer x port))
                rest))))

(define (disp . rest)
  (disp-primitive display rest))

(define (writ . rest)
  (disp-primitive write rest))

(define (flattening-output writer lst)
  (for-each (lambda (x)
              (if (pair? x)
                  (flattening-output writer x)
                  (if (procedure? x)
                      (x)
                      (writer x))))))

(define (output-primitive writer rest)
  (call/port-rest rest (current-output-port)
    (lambda (port rest)
      (call-with-current-output-port
       (lambda ()
         (flattening-output writer rest))))))

(define (output . rest)
  (cout-primitive display rest))

(define (crlf? port)
  (define (look ch)
    (and (char-ready? port)
         (char=? ch (peek-char port))
         (read-char port)))
  (and (look #\return)
       (look #\newline)))

(define (concat-primitive writer things)
  (call-with-string-output-port
   (lambda (port)
     (apply writer port things))))

(define (concat . things)
  (concat-primitive disp things))

(define (concat-write . things)
  (concat-primitive writ things))

(define (list->alist lst)
  (let lp ((lst lst))
    (if (null? lst)
        '()
        (cons (cons (car lst)
                    (cadr lst))
              (lp (cddr lst))))))

(assert (list->alist '(1 2 3 4)) => '((1 . 2) (3 . 4)))

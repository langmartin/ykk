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


;;;; String & port things
(define (port? obj)
  (or (input-port? obj)
      (output-port? obj)))

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
  (let-optionals* rest ((keep-delimiter #f))
    (let* ((proc (string-or-chars->predicate delims/proc)))
      (let lp ()
        (let ((current (peek-char port)))
          (or (eof-object? current)
              (if (proc current)
                  (if keep-delimiter
                      (display (read-char port) output-port))
                  (begin
                    (display (read-char port) output-port)
                    (lp)))))))))

(define (next-chunk delims/proc . rest)
  (let-optionals* rest ((port (current-input-port))
                        (keep-delimiter #f))
    (call-with-string-output-port
     (lambda (output-port)
       (next-chunk-display delims/proc port output-port keep-delimiter)))))

(let ((mp make-string-input-port))
  (assert
   (next-chunk "%+" (mp "hello%20there")) => "hello"
   (next-chunk " " (mp "foo bar") #t) => "foo "
   (next-chunk " " (mp "foobar")) => "foobar"
   (next-chunk "q" (mp "")) => ""))

(define (not-eof-object? obj)
  (not (eof-object? obj)))

(define (port-slurp port)
  (next-chunk not-eof-object?
              port))


;;;; This is all the lazy output stuff from the io-out days
(define (disp-for-each writer rest)
  (call/port-rest rest (current-output-port)
    (lambda (port rest)
      (for-each (lambda (x)
                  (writer x port))
                rest))))

(define (disp . rest)
  (disp-for-each display rest))

(define (writ . rest)
  (disp-for-each write rest))

(define (flattening-output writer lst)
  (for-each (lambda (x)
              (if (pair? x)
                  (flattening-output writer x)
                  (if (procedure? x)
                      (x)
                      (writer x))))))

(define (output-for-each writer rest)
  (call/port-rest rest (current-output-port)
    (lambda (port rest)
      (call-with-current-output-port
       port
       (lambda ()
         (flattening-output writer rest))))))

(define (output . rest)
  (output-for-each display rest))

(define (crlf? port)
  (define (look ch)
    (and (char-ready? port)
         (char=? ch (peek-char port))
         (read-char port)))
  (and (look #\return)
       (look #\newline)))

(define (concat-for-each writer things)
  (call-with-string-output-port
   (lambda (port)
     (apply writer port things))))

(define (concat . things)
  (concat-for-each disp things))

(define (concat-write . things)
  (concat-for-each writ things))

;;;; The fancy iterators from Oleg's zipper
(define (map* proc lst)
  (if (null? lst)
      lst
      (let ((head (car lst)) (tail (cdr lst)))
        (let ((head1 (proc head))
              (tail1 (map* proc tail)))
          (if (and (eq? head1 head) (eq? tail1 tail))
              lst
              (cons head1 tail1))))))

(define (depth-first handle tree)
  (cond ((null? tree) tree)
        ((handle tree) =>
         (lambda (new-tree) new-tree))
        ((not (pair? tree)) tree)
        (else
         (let ((mapped (map* (lambda (kid)
                               (depth-first handle kid))
                             tree)))
           (if (eq? mapped tree)
               tree
               mapped)))))


;;;; bits that came up doing ducts. alists, bits for testing
(define (call-with-string-ports input-string thunk)
  (call-with-string-output-port
   (lambda (output)
     (call-with-current-output-port
      output
      (lambda ()
        ((lambda (input)
           (call-with-current-input-port
            input
            thunk))
         (make-string-input-port
          input-string)))))))

(define-syntax with-string-ports
  (syntax-rules ()
    ((_ input body ...)
     (call-with-string-ports
      input
      (lambda ()
        body ...)))))

(define-syntax begin1
  (syntax-rules ()
    ((_ e1 e2 ...)
     (let ((result e1))
       e2 ...
       result))))

(define (list->alist lst)
  (let lp ((lst lst))
    (if (null? lst)
        '()
        (cons (cons (car lst)
                    (cadr lst))
              (lp (cddr lst))))))

(assert (list->alist '(1 2 3 4)) => '((1 . 2) (3 . 4)))

(define (find-first proc lst)
  (let lp ((lst lst))
    (if (null? lst)
        '()
        (or (proc (car lst))
            (lp (cdr lst))))))

(assert (find-first (lambda (x) (and (= x 4) x)) '(1 3 5 4 6)) => 4)

(define (update-alist orig update)
  (map (lambda (old)
         (or (assq (car old) update)
             old))
       orig))

(assert
 (update-alist '((a . 1) (b . 2) (c . 3)) '((b . 42) (d . 3))) =>
 '((a . 1) (b . 42) (c . 3)))

(define (update-force-alist orig update)
  (fold (lambda (x acc)
          (if (assq (car x) acc)
              acc
              (cons x acc)))
        '()
        (append (reverse update)
                (reverse orig))))

(assert
 (update-force-alist
  '((a . 1) (b . 2) (c . 3)) '((b . 42) (d . 3))) =>
  '((a . 1) (c . 3) (b . 42) (d . 3)))

(define (close-port port)
  (and (input-port? port) (close-input-port port))
  (and (output-port? port) (close-output-port port)))

(define (call-while test? thunk)
  (let lp ()
    (if (test? (thunk))
        (lp)
        #f)))

(define-syntax while
  (syntax-rules ()
    ((_ test? expr)
     (call-while test?
                 (lambda ()
                   expr)))))

(define-syntax until
  (syntax-rules ()
    ((_ test? expr)
     (call-while (lambda (x)
                   (not (test? x)))
                 (lambda ()
                   expr)))))

(define-syntax case-equal
  (syntax-rules (else)
    ((_ key (else body ...))
     (begin body ...))
    ((_ key ((datum ...) body ...))
     (and (member key '(datum ...))
          (begin body ...)))
    ((_ key clause1 clause2 ...)
     (or (case-equal key clause1)
         (case-equal key clause2 ...)))))

(define (make-not proc)
  (lambda x (not (apply proc x))))

(define (intersperse obj lst)
  (cons (car lst)
        (fold-right (lambda (x acc)
                      (cons obj
                            (cons x
                                  acc)))
                    '()
                    (cdr lst))))

(assert (intersperse #\a '(1 2 3)) => '(1 #\a 2 #\a 3))

(define-syntax let-port-rest
  (syntax-rules ()
    ((_ rest (port default) body ...)
     (call/datum-rest
      rest
      port?
      default
      (lambda (port rest)
        body ...)))))

(define (string/port->port obj)
  (if (port? obj)
      obj
      (make-string-input-port obj)))

(define (port? obj)
  (or (input-port? obj)
      (output-port? obj)))

(define (close-port port)
  (and (input-port? port) (close-input-port port))
  (and (output-port? port) (close-output-port port)))

(define with-current-output-port call-with-current-output-port)

(define-syntax define-let-variation
  (syntax-rules ()
    ((_ name proc arg ...)
     (define-syntax name
       (syntax-rules ()
         ((_ arg ... . body)
          (proc arg ... (define-let-variation body))))))
    ((_ (body ...))
     (lambda () body ...))))

(define-let-variation let-foo foo thing)

(define-syntax let-current-output-port
  (syntax-rules ()
    ((_ port body ...)
     (with-current-output-port port (lambda () body ...)))))

(define (maybe-current-output-port port thunk)
  (if (null? port)
      (thunk)
      (with-current-output-port
          (if (pair? port) (car port) port)
        thunk)))

(define-syntax let-maybe-current-output-port
  (syntax-rules ()
    ((_ port body ...)
     (maybe-current-output-port port (lambda () body ...)))))

(define with-current-input-port call-with-current-input-port)

(define-syntax let-current-input-port
  (syntax-rules ()
    ((_ port body ...)
     (with-current-input-port port (lambda () body ...)))))

(define (maybe-current-input-port port thunk)
  (if (null? port)
      (thunk)
      (with-current-input-port
          (if (pair? port) (car port) port)
        thunk)))

(define-syntax let-maybe-current-input-port
  (syntax-rules ()
    ((_ port body ...)
     (maybe-current-input-port port (lambda () body ...)))))

(define (with-string-output-port thunk)
  (call-with-string-output-port
   (lambda (port)
     (with-current-output-port
         port
       thunk))))

(define-syntax let-string-output-port
  (syntax-rules ()
    ((_ body ...)
     (with-string-output-port (lambda () body ...)))))

(define (with-string-input-port string thunk)
  (with-current-input-port
   (make-string-input-port string)
   thunk))

(define-syntax let-string-input-port
  (syntax-rules ()
    ((_ string body ...)
     (with-string-input-port string (lambda () body ...)))))

(define (call-with-u8-output-port receiver)
  (let ((port (make-byte-vector-output-port)))
    (receiver port)
    (byte-vector-output-port-output port)))

(define (with-u8-output-port thunk)
  (call-with-u8-output-port
   (lambda (port)
     (with-current-output-port
         port
       thunk))))

(define-syntax let-u8-output-port
  (syntax-rules ()
    ((_ body ...)
     (with-u8-output-port (lambda () body ...)))))

(define (with-string-ports input-string thunk)
  (let-string-output-port
   (with-string-input-port
       input-string
     thunk)))

(define-syntax let-string-ports
  (syntax-rules ()
    ((_ input body ...)
     (with-string-ports input (lambda () body ...)))))

;;;; Parsing utilties
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

(define (next-chunk-primitive peek-char read-char pred? display keep-delimiter)
  (let ((current (peek-char)))
    (or (eof-object? current)
        (if (pred? current)
            (if keep-delimiter
                (display (read-char)))
            (begin
              (display (read-char))
              (next-chunk-primitive peek-char read-char pred? display keep-delimiter))))))

(define (next-chunk-for-each pred? display keep-delimiter)
  (next-chunk-primitive peek-char read-char pred? display keep-delimiter))

(define (next-chunk delims/proc . rest)
  (let-optionals* rest ((port '())
                        (keep-delimiter #f))
    (let ((pred? (string-or-chars->predicate delims/proc)))
      (let-maybe-current-input-port
          port
        (let-string-output-port
         (next-chunk-for-each pred? display keep-delimiter))))))

(define sp make-string-input-port)

(assert
 (next-chunk "%+" (sp "hello%20there")) => "hello"
 (next-chunk " " (sp "foo bar") #t) => "foo "
 (next-chunk " " (sp "foobar")) => "foobar"
 (next-chunk "q" (sp "")) => "")

(define (not-eof-object? obj)
  (not (eof-object? obj)))

(define (port-slurp port)
  (next-chunk not-eof-object?
              port))

;;;; This is all the lazy output stuff from the io-out days
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

(assert
 (call-with-string-output-port
  (lambda (p)
    (output p 1 2 3 '(2 3) 5 6))) => "1232356")

(define (crlf? port)
  (define (look ch)
    (and (char-ready? port)
         (char=? ch (peek-char port))
         (read-char port)))
  (and (look #\return)
       (look #\newline)))

;;;; string parsing and some quick functions
(define to-return-pred (string-or-chars->predicate '(#\return)))

(define (read-crlf-line . port)
  (let-maybe-current-input-port
      port
    (let-string-output-port
     (next-chunk-for-each to-return-pred display #f)
     (read-char)
     (let ((next (peek-char)))
       (if (eof-object? next)
           next
           (if (char=? #\newline next)
               (read-char)
               (begin
                 (display #\return)
                 (display (read-crlf-line)))))))))

(define (string-split string . pred+max)
  (let-optionals* pred+max ((pred whitespace?)
                            (max #f)
                            (consume #t))
    (let ((proc (string-or-chars->predicate pred)))
      (with-string-input-port
          string
        (lambda ()
          (let lp ((max (or max -1)))
            (if (or (eof-object? (peek-char)) (= max 0))
                '()
                (let ((section (next-chunk proc)))
                  (if consume (consume-chars proc))
                  (cons section
                        (lp (- max 1)))))))))))

(define (whitespace? ch)
  (or (char=? ch #\space)
      (and (char>=? ch #\tab)
           (char<=? ch #\return))))

(define (consume-chars pred . port)
  (let-optionals* port ((port (current-input-port)))
    (let ((current (peek-char port)))
      (or (eof-object? current)
          (and (pred current)
               (begin
                 (read-char)
                 (consume-chars pred port)))))))

(assert
 (string-split "foo   bar" whitespace? 3) => '("foo" "bar")
 (string-split "foo bar" whitespace? 1) => '("foo")
 (string-split "foo   bar") => '("foo" "bar"))

;;;; Gambit compatibility
(define (with-output-to-string string thunk)
  (with-string-output-port thunk))

(define (call-with-output-string string receiver)
  (receiver (make-string-output-port)))

(define with-input-from-string with-string-input-port)

(define (call-with-input-string string receiver)
  (receiver (make-string-input-port string)))

(define (read-line . rest)
  (let-optionals* rest ((port (current-input-port))
                        (separator #\newline)
                        (include-separator? #f))
    (if separator
        (next-chunk separator port include-separator?)
        (next-chunk (lambda (c) #f) port))))

(define (read-all . rest)
  (let-optionals* rest ((reader read)
                        (port (current-input-port)))
    (let lp ()
      (let ((current (reader port)))
        (if (eof-object? current)
            '()
            (cons current
                  (lp)))))))

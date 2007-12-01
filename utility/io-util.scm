(define-syntax call/port-rest
  (syntax-rules ()
    ((_ rest default receiver)
     (call/datum-rest rest port? default receiver))))

(define (string/port->port obj)
  (if (port? obj)
      obj
      (make-string-input-port obj)))

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

(define (close-port port)
  (and (input-port? port) (close-input-port port))
  (and (output-port? port) (close-output-port port)))

(define (read-crlf-line port)
  (call-with-string-output-port
   (lambda (output-port)
     (next-chunk-display '(#\return) port output-port)
     (read-char port)
     (if (char=? #\newline (peek-char port))
         (read-char port)
         (begin
           (display #\return output-port)
           (display (read-crlf-line port) output-port))))))

(define (with-input-from-string string thunk)
  (call-with-current-input-port
   (make-string-input-port string)
   thunk))

(define (string-split string . pred+max)
  (let-optionals* pred+max ((pred whitespace?)
                            (max #f)
                            (consume #t))
    (let ((proc (string-or-chars->predicate pred)))
      (with-input-from-string
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

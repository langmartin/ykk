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

(define (crlf? port)
  (define (look ch)
    (and (char=? ch (peek-char port))
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

(define (read-line . rest)
  (let-optionals* rest ((port (current-input-port))
                        (separator #\newline)
                        (include-separator? #f))
    (if separator
        (next-chunk separator port include-separator?)
        (next-chunk (lambda (c) #f) port))))

(define (port-fold cons nil reader . port)
  (let ((current (apply reader port)))
    (if (eof-object? current)
        nil
        (port-fold cons (cons current nil) reader))))

(define (port-fold-right cons nil reader . port)
  (let ((current (apply reader port)))
    (if (eof-object? current)
        nil
        (cons current
              (port-fold-right cons nil reader)))))

(define (read-all . rest)
  (let-optionals* rest ((reader read)
                        (port (current-input-port)))
    (port-fold-right cons '() reader port)))

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

(define (%string-escape pred string)
  (string-for-each (lambda (ch)
                     (if (pred ch)
                         (display ch))
                     (display ch))
                   string))

(define (escape-by-doubling strpred string)
  (let ((pred (string-or-chars->predicate strpred)))
    (let-string-output-port
     (%string-escape pred string))))

(define (escape-and-quote ch string)
  (let-string-output-port
   (display ch)
   (%string-escape (lambda (el)
                     (char=? el ch))
                   string)
   (display ch)))

(assert
 (escape-by-doubling #\' "''") => "''''"
 (escape-and-quote #\/ "hello/there") => "/hello//there/")

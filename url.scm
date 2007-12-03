(define (return x) x)

(define (hex . chars)
  (ascii->char
   (read
    (make-string-input-port
     (list->string
      (cons #\#
            (cons #\x
                  chars)))))))

(define (urldecode producer)
  (let ((buffer #f))
    (define (push ch return)
      (set! buffer ch)
      return)
    (define (pop)
      (if (not buffer)
          #f
          (let ((ch buffer))
            (set! buffer #f)
            ch)))
    (define (read1)
      (let ((ch (producer)))
        (and (not (eof-object? ch)) ch)))
    (define (found-esc esc)
      (let* ((one (read1)) (two (read1)))
        (if (not one)
            esc
            (if (not two)
                (push one esc)
                (hex one two)))))
    (lambda ()
      (cond ((pop) => return)
            (else
             (let ((ch (producer)))
               (or (and (eof-object? ch) ch)
                   (case ch
                     ((#\+) #\space)
                     ((#\%) (found-esc ch))
                     (else ch)))))))))

(define (urldecode-string string)
  (with-string-ports
   string
   (let ((proc (urldecode read-char)))
     (let lp ()
       (if (not (eof-object? (peek-char)))
           (begin
             (display (proc))
             (lp)))))))

(define-record-type url rtd/url
  (make-url proto host port path parameters)
  url?
  (proto url-protocol)
  (host url-host)
  (port url-port)
  (path url-path)
  (parameters url-parameters))

(define-record-discloser rtd/url
  (lambda (url)
    `(url ,(url-protocol url)
          ,(url-host url)
          ,(url-port url)
          ,(url-path url)
          ,(url-parameters url))))

(define (parse-url-port param-proc nil port)
  (call-with-current-input-port
   port
   (lambda ()
    (let* ((pred (string-or-chars->predicate "://"))
           (protocol (string->symbol
                      (string-downcase
                       (next-chunk pred))))
           (_ (consume-chars pred))
           (host (next-chunk pred))
           (port (or (inline-port)
                     (default-port protocol)))
           (path (next-chunk "?"))
           (parameters (maybe-parameters param-proc nil)))
      (make-url
       protocol
       host
       port
       path
       parameters)))))

(define (parse-url-string param-proc nil url-string)
  (parse-url-port param-proc nil (make-string-input-port url-string)))

(define (cons-alist k v tail)
  (cons (cons k v) tail))

(define (parse-url url-string)
  (parse-url-string
   cons-alist
   '()
   url-string))

(define (inline-port)
  (let ((ch (peek-char)))
    (and (char? ch)
         (char=? ch #\:)
         (read-char)
         (string->number
          (next-chunk "/")))))

(define *protocol-ports*
  '((http . 80)))

(define (default-port protocol)
  (cond ((assq protocol *protocol-ports*) => cdr)
        (else
         #f)))

(define (maybe-parameters param-proc nil)
  (let ((ch (peek-char)))
    (if (eof-object? ch)
        nil
        (begin
          (read-char)
          (url-foldr-parameters param-proc
                          nil
                          (current-input-port))))))

(define (empty-string? string)
  (string=? "" string))

(define (url-foldr-parameters param-proc nil port)
  (let ((key (urldecode-string
              (next-chunk "=" port))))
    (read-char)
    (if (empty-string? key)
        nil
        (let ((val (urldecode-string (next-chunk "&;" port))))
          (read-char)
          (param-proc key val
                      (url-foldr-parameters param-proc nil port))))))

(define (two-ary-url=? url1 url0)
  (if (and (and (url? url0) (url? url1))
           (eq? (url-protocol url0) (url-protocol url1))
           (equal? (url-host url0) (url-host url1))
           (eq? (url-port url0) (url-port url1))
           (equal? (url-path url0) (url-path url1))
           (equal? (url-parameters url0) (url-parameters url1)))
      url0
      #f))

(define (url=? . urls)
  (url?
   (fold two-ary-url=?
         (car urls)
         (cdr urls))))

(assert
 (url=? (parse-url "http://coptix.com/foo/page.php")
        (make-url 'http "coptix.com" 80 "/foo/page.php" '())))

(assert
 (url=? (parse-url "http://coptix.com:81/foo/page.php?foo=bar%20baz")
        (make-url 'http "coptix.com" 81 "/foo/page.php" '(("foo" . "bar baz")))))

(define (for-each-display producer)
  (let ((ch (producer)))
    (or (eof-object? ch)
        (begin
          (display ch)
          (for-each-display producer)))))

(assert
 (let-string-ports
     "hello"
   (for-each-display read-char)) => "hello")

(define (return x) x)

(define (hex . chars)
  (integer->char
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
  (let-string-ports
      string
    (for-each-display (urldecode read-char))))

(assert (urldecode-string "foo+bar") => "foo bar") 

(define (alphanumeric? ch)
  (or (and (char>=? ch #\a) (char<=? ch #\z))
      (and (char>=? ch #\A) (char<=? ch #\Z))
      (and (char>=? ch #\0) (char<=? ch #\9))))

;; (define (urladditional? ch)
;;   (case ch
;;     ((#\_ #\/ #\. #\- #\:) ch)
;;     (else #f)))

(define (urlencode producer)
  (define buffer '())
  (define (pop!)
    (and (pair? buffer)
         (let ((c (car buffer)))
           (set! buffer (cdr buffer))
           c)))
  (lambda ()
    (or (pop!)
        (let ((ch (producer)))
          (cond ((eof-object? ch) ch)
                ((alphanumeric? ch) ch)
                ((char=? #\space ch)
                 #\+)
                (else
                 (let ((n (char->scalar-value ch)))
                   (set! buffer
                         (string->list
                          (number->string n 16)))
                   #\%)))))))

(define (urlencode-string string)
  (let-string-ports
      string
    (for-each-display (urlencode read-char))))

(assert (urlencode-string "foo bar baz!") => "foo+bar+baz%21")

(define (urlencode-display string)
  (let-string-input-port
   string
   (for-each-display (urlencode read-char))))

;;;; Data type, Interface
(define-record-type url
  (make-url proto host port path parameters)
  url?
  (proto url-protocol)
  (host url-host)
  (port url-port)
  (path url-path)
  (parameters url-parameters))

(define-record-discloser url
  (lambda (url)
    `(url ,(url-protocol url)
          ,(url-host url)
          ,(url-port url)
          ,(url-path url)
          ,(url-parameters url))))

(define (parse-url-path* param-proc nil)
  (values
   (next-chunk "?")
   (maybe-parameters param-proc nil)))

(define (parse-url-port param-proc nil . port)
  (let-maybe-current-input-port
      port
    (let* ((pred (string-or-chars->predicate "://"))
           (protocol (string->symbol
                      (string-downcase
                       (next-chunk pred))))
           (_ (consume-chars pred))
           (host (next-chunk pred))
           (port (or (inline-port)
                     (default-port protocol))))
      (call-with-values
          (lambda () (parse-url-path* param-proc nil))
        (lambda (path parameters)
         (make-url
          protocol
          host
          port
          path
          parameters))))))

(define (parse-url-string param-proc nil url-string)
  (parse-url-port param-proc nil (make-string-input-port url-string)))

(define (cons-alist k v tail)
  (cons (cons k v) tail))

(define (parse-url url-string)
  ;; (note "parse-url" url-string)
  (let ((url
         (parse-url-string
          cons-alist
          '()
          url-string)))
    (note "parse-url" url-string url)
    url))

(define (parse-url-path url-path-string)
  (let-string-input-port
      url-path-string
   (lambda ()
     (parse-url-path* cons-alist '()))))

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
          (url-foldr-parameters param-proc nil)))))

(define (empty-string? string)
  (string=? "" string))

(define (url-foldr-parameters-lp param-proc nil)
  (let ((lp (lambda () (url-foldr-parameters-lp param-proc nil)))
        (variable (next-chunk "&;")))
    (read-char)
    (if (string-null? variable)
        nil
        (let* ((port (make-string-input-port variable))
               (key (urldecode-string (next-chunk #\= port)))
               (_ (read-char port))
               (val (urldecode-string (next-chunk (lambda (x) #f) port))))
          (param-proc key val (lp))))))

(define (url-foldr-parameters proc nil . port)
  (let-maybe-current-input-port
      port
    (url-foldr-parameters-lp proc nil)))

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
        (make-url 'http "coptix.com" 80 "/foo/page.php" '()))
 (url=? (parse-url "http://coptix.com:81/foo/page.php?foo=bar%20baz")
        (make-url 'http "coptix.com" 81 "/foo/page.php" '(("foo" . "bar baz"))))
 (url=? (parse-url "http://coptix.com:81/foo/page.php?foo&bar")
        (make-url 'http "coptix.com" 81 "/foo/page.php" '(("foo" . "") ("bar" . "")))))

(assert
 (url=?
  (parse-url "http://coptix.com:81/foo/page.php?foo=bar%20baz&thing=5;bz=blip")
  (make-url 'http "coptix.com" 81 "/foo/page.php"
            '(("foo" . "bar baz") ("thing" . "5") ("bz" . "blip")))))

(assert
 (url=? (parse-url "http://coptix.com:3140/"))
 (make-url 'http "coptix.com" 3140 "/" '()))

(define (url-parameter-string url)
  (define (show key val)
    (urlencode-display key)
    (display #\=)
    (urlencode-display val))
  (let ((param (url-parameters url)))
    (let-string-output-port
     (if (not (null? param))
         (for-each (lambda (x)
                     (if (pair? x)
                         (show (car x) (cdr x))
                         (display x)))
                   (intersperse #\& param))))))

(define (url-parameters? url)
  (not (null? (url-parameters url))))

(assert
 (url-parameter-string
  (make-url 1 2 3 4 '(("foo" . "bar!") ("baz" . "quux, biatch!"))))
 => "foo=bar%21&baz=quux%2c+biatch%21")

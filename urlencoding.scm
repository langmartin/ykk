(define (hex . chars)
  (read
   (make-string-input-port
    (list->string
     (cons #\#
           (cons #\x
                 chars))))))

(define (urldecode s/p)
  (let ((port (string/port->port s/p)))
    (define (read1)
      (let ((ch (read-char port)))
        (if (eof-object? ch) #f ch)))
    (call-with-string-output-port
     (lambda (out)
       (let lp ()
         (let* ((chunk (next-chunk "%+" port))
                (which (read-char port)))
           (unless
            (eof-object? which)
            (display chunk out)
            (if (char=? which #\+)
                (display #\space out)
                (let ((one (read1)) (two (read1)))
                  (if (and one two)
                      (display (integer->char (hex one two)) out)
                      (for-each (lambda (x)
                                  (and x (display x out)))
                                (list which one two)))))
            (lp))))))))

(assert
  (urldecode "hello+there%20how%20are%20you%2") => "hello there how are you%2")

(define hex-values "0123456789ABCDEF")

(define (alphanumeric? ch)
  (or (and (char>=? ch #\a) (char<=? ch #\z))
      (and (char>=? ch #\A) (char<=? ch #\Z))
      (and (char>=? ch #\0) (char<=? ch #\9))))

(define (urlencode-display string . port)
  (let ((port (optional port (current-output-port))))
    (define (write-nibble n)
      (write-char (string-ref hex-values n) port))
    (string-for-each
     (lambda (ch)
       (cond ((alphanumeric? ch)
              (write-char ch port))
             ((char=? #\space ch)
              (write-char #\+ port))
             (else (let ((n (char->ascii ch)))
                     (write-char #\% port)
                     (write-nibble
                      (bitwise-and (arithmetic-shift n -4) 15))
                     (write-nibble (bitwise-and n 15))))))
     string)))

(define (urlencode string)
  (call-with-string-output-port
   (lambda (port)
     (urlencode-display string port))))

(assert
  (urlencode "hello there how's it going?") => "hello+there+how%27s+it+going%3F")

(define (e-unimplemented . args)
  (apply error
         "ducts: unimplemented"
         args))

(define (read-proc duct)
  (duct-get-property duct 'reader))

(define (make-byte-len-reader len port)
  (let ((buf (make-byte-vector len 0)) (idx -1))
    (lambda ()
      (and (< idx 0)
           (read-block buf 0 len port)
           (set! idx 0))
      (if (= idx len)
          (eof-object)
          (begin1
           (byte-vector-ref buf idx)
           (set! idx (+ idx 1)))))))

(define (find-port-parent duct)
  (let ((p (duct-parent duct)))
    (if (port? p)
        p
        (find-port-parent p))))

;;;; base64
(define padding-char #\=)

(define encoding-vector
  '#(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
     #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f
     #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
     #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))

(define decoding-vector
  (let ((dec (make-vector 128 #f)))
    (do ((enc (vector->list encoding-vector) (cdr enc))
         (i 0 (+ i 1)))
        ((null? enc) dec)
      (vector-set! dec (char->ascii (car enc)) i))))

(define (read-byte . maybe-in)
  (let ((x (apply read-char maybe-in)))
    (if (eof-object? x) x
	(char->ascii x))))

(define (write-byte b . maybe-out)
  (apply write-char (ascii->char b) maybe-out))

(define (bits-take x n)
  (bitwise-and x (- (arithmetic-shift 1 n) 1)))

(define (bits-drop x n)
  (arithmetic-shift x (- n)))

(define (make-base64-reader next-char)
  (let ((padding (char->ascii padding-char))
        (bits 0) (bits-count 0))
    (define (set b bc)
      (set! bits b)
      (set! bits-count bc))
    (define (skip b bc)
      (set b bc)
      (body))
    (define (body)
      (let ((ch (next-char)))
        (or (and (or (eof-object? ch)
                     (= ch padding))
                 (eof-object))
            (let ((six (vector-ref decoding-vector ch)))
              (if (not six)
                  (skip bits bits-count)
                  (let ((full-bits (bitwise-ior (arithmetic-shift bits 6) six))
                        (full-bits-count (+ bits-count 6)))
                    (if (not (>= full-bits-count 8))
                        (skip full-bits full-bits-count)
                        (let* ((carry-bits-count (- full-bits-count 8))
                               (byte (bits-drop full-bits carry-bits-count)))
                          (set (bits-take full-bits carry-bits-count)
                               carry-bits-count)
                          byte))))))))
    body))

(assert
 (let-string-ports
  "Zm9vYmFy"
  (let lp ((next (make-base64-reader read-byte)))
    (let ((ch (next)))
      (if (not (eof-object? ch))
          (begin (display (ascii->char ch))
                 (lp next)))))) => "foobar")

;;;; transfer-encoding: chunked
(define (string->hex-number string)
  (let-string-input-port
      (string-append "#x" string)
    (read)))

(assert (string->hex-number "ff") => 255)

(define (hex-string? x)
  (let ((r (string-fold
            (lambda (x hex)
              (and hex
                   (or (and (char>=? x #\a) (char<=? x #\f))
                       (and (char>=? x #\A) (char<=? x #\F))
                       (and (char>=? x #\0) (char<=? x #\9)))))
            'empty
            x)))
    (if (eq? r 'empty) #f r)))

(assert (hex-string? "44fd5")
        (not (hex-string? "44fd5g"))
        (not (hex-string? "")))

(define (read-http-chunk-len port)
  (let* ((len (read-crlf-line port))
         (len (if (string-null? len)
                  (read-crlf-line port)
                  len))
         (len (string-trim-both len)))
    (if (not (hex-string? len))
        0
        (string->hex-number len))))

(define (end-of-chunks) end-of-chunks)

(define (end-of-chunks? obj) (eq? obj end-of-chunks))

(define (open-chunked byte-len parent)
  (if (end-of-chunks? byte-len)
      (eof-object)
      (let ((len (read-http-chunk-len
                  (find-port-parent parent))))
        (if (zero? len)
            (eof-object)
            ((d/byte-len len) parent)))))

;;;; Definitions
(define (d/peek)
  (lambda (parent)
    (duct-extend*
     parent
     (name "peek")
     (buffer #f)
     (reader (lambda ()
               (let ((b buffer))
                 (set! buffer (duct-read parent))
                 (or b (reader)))))
     (peeker (lambda ()
               (or buffer (reader)))))))

(define (d/byte-len len)
  (lambda (parent)
    (duct-extend*
     parent
     (name "byte-len")
     (reader (make-byte-len-reader len (find-port-parent parent)))
     (buffer #f)
     (write-port (make-byte-vector-output-port))
     (writer (lambda (b) (display b write-port)))
     (closer (lambda ()
               (set! buffer
                     (byte-vector-output-port-output write-port)))))))


(define (d/ascii* name0 ascii->char char->ascii)
  (lambda (parent)
    (duct-extend*
     parent
     (name name0)
     (reader (lambda ()
               (let ((ch (duct-read parent)))
                 (or (and (eof-object? ch) ch)
                     (and (char? ch) ch)
                     (ascii->char ch)))))
     (writer (lambda (ch)
               (duct-write
                parent
                (or (and (char? ch) ch)
                    (char->ascii ch))))))))

(define (d/ascii) (d/ascii* "ascii" ascii->char char->ascii))

(define (d/characters) (d/ascii* "ascii" integer->char char->integer))

(define (d/base64)
  (lambda (parent)
    (duct-extend*
     parent
     (name "base64")
     (reader (make-base64-reader (read-proc parent)))
     (writer e-unimplemented))))

(define (d/urlencode)
  (lambda (parent)
    (duct-extend*
     parent
     (name "urlencode")
     (reader (urldecode (read-proc parent))))))

(define (d/http-chunked)
  (lambda (parent)
    (duct-extend*
     parent
     (name "http-chunked")
     (byte-len 'not-a-duct)
     (update (lambda ()
               (let ((new (open-chunked byte-len parent)))
                 (set! byte-len new)
                 (if (duct? new)
                     (duct-read new)
                     new))))
     (reader (lambda ()
               (if (not (duct? byte-len))
                   (update)
                   (let ((byte (duct-read byte-len)))
                     (if (not (eof-object? byte))
                         byte
                         (update)))))))))

(define (d/null)
  (lambda (parent)
    (duct-extend*
     parent
     (name "null")
     (reader eof-object))))

(assert
 (let-string-ports
     "9\r\nZm9vYmFyYmF6"
   (let ((out
          ((d/peek)
           ((d/ascii)
            ((d/base64)
             ((d/http-chunked)
              (port->duct (current-input-port))))))))
     (duct-for-each display out))) => "foobar")

(define (d/tee input-tee output-tee)
  (lambda (parent)
    (duct-extend*
     parent
     (name "tee")
     (reader (lambda ()
               (let ((ch (duct-read parent)))
                 (write ch input-tee)
                 ch)))
     (writer (lambda (ch)
               (write ch output-tee)
               (duct-write parent ch)))
     (closer (lambda ()
               (close-output-port input-tee)
               (close-output-port output-tee))))))

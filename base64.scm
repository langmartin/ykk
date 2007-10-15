(define (read-byte . maybe-in)
  (let ((x (apply read-char maybe-in)))
    (if (eof-object? x) x
	(char->ascii x))))

(define (write-byte b . maybe-out)
  (apply write-char (ascii->char b) maybe-out))

;; This module implements Base64 encoding and decoding as specified by
;; section 6.8 of RFC 2045.

(define ash arithmetic-shift)

;; Return a procedure to write a character to port, and produce a new
;; line after a multiple of line-len characters have been output.
(define (char-writer port line-len)
  (let ((col 0))
    (lambda (char)
      (write-char char port)
      (set! col (+ col 1))
      (when (>= col line-len)
            (newline port)
            (set! col 0)))))

;; Take the first n bits of x.
(define (bits-take x n)
  (bitwise-and x (- (ash 1 n) 1)))

;; Drop the first n bits of x.
(define (bits-drop x n)
  (ash x (- n)))

;; The padding character
(define pad #\=)

(define eof-object (read (make-string-input-port "")))

;;; Encoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define encoding-vector
  '#(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P
     #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z #\a #\b #\c #\d #\e #\f
     #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v
     #\w #\x #\y #\z #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))

;; (base64-encode-vector vector [out-port])
;;   --> string or port
(define (base64-encode-vector vector . rest)
  (let ((len (byte-vector-length vector)) (pos 0))
    (apply base64-encode-internal
           (lambda ()
             (if (< pos len)
                 (let ((byte (byte-vector-ref vector pos)))
                   (set! pos (+ pos 1))
                   byte)
                 eof-object))
           rest)))

;; (base64-encode-port port [out-port])
;;   --> string or port
(define (base64-encode-port in-port . rest)
  (apply base64-encode-internal (lambda () (read-byte in-port)) rest))

;; (base64-encode-string string [out-port])
;;  --> string or port
(define (base64-encode-string string . rest)
  (apply base64-encode-port (make-string-input-port string) rest))

(define (base64-encode-internal fetch-byte . rest)
  (let-optionals rest ((out-port (make-string-output-port)))
    (let ((output-char (char-writer out-port 76)))
      (let loop ((next-byte (fetch-byte)) (bits 0) (bits-count 0) (len 0))
        (cond ((>= bits-count 6)
               ;; Enough bits to output a character: do so, iterate.
               (let* ((carry-bits-count (- bits-count 6))
                      (next-6bits (bits-drop bits carry-bits-count)))
                 (output-char (vector-ref encoding-vector next-6bits))
                 (loop next-byte
                       (bits-take bits carry-bits-count)
                       carry-bits-count
                       len)))
              ((not (eof-object? next-byte))
               ;; Need more bits: use next byte.
               (loop (fetch-byte)
                     (bitwise-ior (ash bits 8) next-byte)
                     (+ bits-count 8)
                     (+ len 1)))
              (else
               ;; No data left: output remaining bits, if any, and padding.
               (unless (zero? bits-count)
                       (output-char (vector-ref encoding-vector
                                                (ash bits (- 6 bits-count)))))
               (case (remainder len 3)
                 ((1) (output-char pad) (output-char pad))
                 ((2) (output-char pad)))))))
    ;; Return port/output string.
    (if (null? rest) (string-output-port-output out-port) out-port)))

;;; Decoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define decoding-vector
  (let ((dec (make-vector 128 #f)))
    (do ((enc (vector->list encoding-vector) (cdr enc))
         (i 0 (+ i 1)))
        ((null? enc) dec)
      (vector-set! dec (char->ascii (car enc)) i))))

(define (base64-decode-string str . rest)
  (apply base64-decode-port (make-string-input-port str) rest))

;; (base64-decode in-port [out-port])
;;   --> string or port
(define (base64-decode-port in-port . rest)
  (let-optionals rest ((out-port (make-string-output-port)))
    (let loop ((bits 0) (bits-count 0))
      (let ((next-char (read-char in-port)))
        (when (not (or (eof-object? next-char) (char=? next-char pad)))
              (let ((next-6bits (vector-ref decoding-vector
                                            (char->ascii next-char))))
                (if (not next-6bits)
                    ;; Non-encoding character, skip it.
                    (loop bits bits-count)
                    ;; Encoding character, handle it.
                    (let ((full-bits (bitwise-ior (ash bits 6) next-6bits))
                          (full-bits-count (+ bits-count 6)))
                      (if (>= full-bits-count 8)
                          ;; Complete byte: output it, iterate.
                          (let* ((carry-bits-count (- full-bits-count 8))
                                 (byte (bits-drop full-bits carry-bits-count)))
                            (write-byte byte out-port)
                            (loop (bits-take full-bits carry-bits-count)
                                  carry-bits-count))
                          ;; No complete byte yet: iterate.
                          (loop full-bits full-bits-count))))))))
      ;; Return port/output string.
      (if (null? rest) (string-output-port-output out-port) out-port)))

(assert
 (base64-decode-string (base64-encode-string "foo bar baz blit")) =>
 "foo bar baz blit")

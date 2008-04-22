;; UUID generation
;; See: http://www.ietf.org/rfc/rfc4122.txt
;;
;; Version 4 UUID, see section 4.4

(define (%mask size) (bitwise-not (arithmetic-shift -1 size)))

(define (extract-bit-field size position n)
  (bitwise-and (%mask size) (arithmetic-shift n (- position))))

(define-syntax define-numeric-list-ops
  (syntax-rules ()
    ((_ width cons car cdr)
     (begin
       (define (cons n sum)
         (+ n (arithmetic-shift sum width)))
       (define (car sum)
         (extract-bit-field width 0 sum))
       (define (cdr sum)
         (arithmetic-shift sum (- width)))))))

(define-numeric-list-ops
  8 u8cons u8car u8cdr)

(define-numeric-list-ops
  4 u4cons u4car u4cdr)

;; (begin
;;  (define (u8list . args)
;;    (fold-right u8cons 0 args))
;;  (define (u8cddr sum)
;;    (u8cdr (u8cdr sum)))
;;  (define (u8caddr sum)
;;    (u8car (u8cddr sum)))
;;  (u8caddr (u8list 1 2 3 4 5)))

;; (begin
;;  (define (u4list . args)
;;    (fold-right u4cons 0 args))
;;  (define (u4cddr sum)
;;    (u4cdr (u4cdr sum)))
;;  (define (u4caddr sum)
;;    (u4car (u4cddr sum)))
;;  (u4caddr (u4list 1 2 3 4 5)))

(assert (u8car (u8cdr (u8cons 2 (u8cons 3 0)))) => 3)

(define (u8unfold gen len)
  (let lp ((n len))
    (if (zero? n)
        0
        (u8cons (gen n)
                (lp (- n 1))))))

(define (make-numeric-fold-right car cdr)
  (lambda (cons nil sum)
    (let lp ((nil nil) (sum sum))
      (if (zero? sum)
          nil
          (lp (cons (car sum) nil)
              (cdr sum))))))

(define (make-numeric-fold car cdr)
  (lambda (cons nil sum)
    (let lp ((sum sum))
      (if (zero? sum)
          nil
          (cons (car sum)
                (lp (cdr sum)))))))

(define u8fold (make-numeric-fold u8car u8cdr))

(define u8fold-right (make-numeric-fold-right u8car u8cdr))

(define u4fold (make-numeric-fold u4car u4cdr))

(define u4fold-right (make-numeric-fold-right u4car u4cdr))

(assert (u8fold-right cons '() (u8unfold (lambda (n) (+ n 3)) 16)) =>
        '(4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19))

(define (uuidgen)
  (u8unfold
   (lambda (n)
     (let ((rand (random-integer 256)))
       (case n
         ((6) (bitwise-ior #b10000000 (bitwise-xor #b10111111 rand)))
         ((8) (bitwise-ior #b01000000 (bitwise-xor #b01001111 rand)))
         (else rand))))
   16))

(define (make-display-rules format)
  (fold-right (lambda (c lst)
                (if (char=? c #\-)
                    (cons c (cdr lst))
                    (cons #f lst)))
              '()
              (string->list format)))

(define hex
  '#(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))

(define format (make-display-rules "0000-00-00-00-000000"))

(define vref vector-ref)

(define (num->hex n)
  (u4fold (lambda (n acc)
            (display (vref hex n)))
          #f
          n))

(define (uuid-number-display uuid)
  (u8fold (lambda (byte dashes)
            (if (car dashes)
                (display (car dashes)))
            (num->hex byte)
            (cdr dashes))
          format
          uuid))

(define (uuid-number->string uuid)
  (let-string-output-port
   (uuid-number-display uuid)))

(define (string-uuid->number string)
  (with-string-input-port
      (string-append "#x" (string-delete #\- string))
    read))

;; (let ((uuid (uuidgen-v1->hex-string)))
;;   (assert uuid => (uuid-number->string (uuid-string->number uuid))))

(define (uuid-string? s)
  (uuid-number?
   (string-uuid->number s)))

(define (uuid-number? n)
  (and ;; (= #b01 (extract-bit-field 2 (+ 1 (* 8 8)) n))
   (= #b01   (extract-bit-field 2
                                (- (- 128 (* 8 8)) 2)
                                n))
   (= #b0100 (extract-bit-field 4
                                (- (- 128 (* 8 6)) 4)
                                n))))

;; (uuid->hex-string (uuidgen))

;; (extract-bit-field
;;  4
;;  ;; (- (- 128 (* 8 8)) 2)
;;  (- (- 128 (* 8 6)) 4)
;;  (uuidgen)
;;  ;; (string-uuid->number
;; ;;   (uuidgen-v1->hex-string)
;; ;;   ;; "6AAF975E-5D43-4E4A-9CEF-32C8408C9C2A"
;; ;;   )
;;  )

(define (uuidgen-v1->hex-string)
  (let ((n1 (random-integer 65536))
        (n2 (random-integer 65536))
        (n3 (random-integer 65536))
        (n4 (random-integer 65536))
        (n5 (random-integer 65536))
        (n6 (random-integer 65536))
        (n7 (random-integer 65536))
        (n8 (random-integer 65536)))
    (string
     ;; time_lo
     (vector-ref hex (extract-bit-field 4 12 n1))
     (vector-ref hex (extract-bit-field 4  8 n1))
     (vector-ref hex (extract-bit-field 4  4 n1))
     (vector-ref hex (extract-bit-field 4  0 n1))
     (vector-ref hex (extract-bit-field 4 12 n2))
     (vector-ref hex (extract-bit-field 4  8 n2))
     (vector-ref hex (extract-bit-field 4  4 n2))
     (vector-ref hex (extract-bit-field 4  0 n2))
     #\-
     ;; time_mid
     (vector-ref hex (extract-bit-field 4 12 n3))
     (vector-ref hex (extract-bit-field 4  8 n3))
     (vector-ref hex (extract-bit-field 4  4 n3))
     (vector-ref hex (extract-bit-field 4  0 n3))
     #\-
     ;; time_hi_and_version
     (vector-ref hex #b0100)
     (vector-ref hex (extract-bit-field 4  8 n4))
     (vector-ref hex (extract-bit-field 4  4 n4))
     (vector-ref hex (extract-bit-field 4  0 n4))
     #\-
     ;; clock_seq_hi_and_reserved
     (vector-ref hex (bitwise-ior (extract-bit-field 2 12 n5) #b1000))
     (vector-ref hex (extract-bit-field 4  8 n5))
     ;; clock_seq_low
     (vector-ref hex (extract-bit-field 4  4 n5))
     (vector-ref hex (extract-bit-field 4  0 n5))
     #\-
     ;; node
     (vector-ref hex (extract-bit-field 4 12 n6))
     (vector-ref hex (extract-bit-field 4  8 n6))
     (vector-ref hex (extract-bit-field 4  4 n6))
     (vector-ref hex (extract-bit-field 4  0 n6))
     (vector-ref hex (extract-bit-field 4 12 n7))
     (vector-ref hex (extract-bit-field 4  8 n7))
     (vector-ref hex (extract-bit-field 4  4 n7))
     (vector-ref hex (extract-bit-field 4  0 n7))
     (vector-ref hex (extract-bit-field 4 12 n8))
     (vector-ref hex (extract-bit-field 4  8 n8))
     (vector-ref hex (extract-bit-field 4  4 n8))
     (vector-ref hex (extract-bit-field 4  0 n8)))))

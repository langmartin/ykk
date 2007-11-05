(define (e-unimplemented . args)
  (error "duct: unimplemented" args))

(define (e-illegal-parent . args)
  (error "duct: illegal parent duct" args))

(define-record-type duct rtd/duct
  (make-duct-rec parent attr)
  duct?
  (parent duct-parent)
  (attr duct-attr duct-set-attr!))

(define (port-duct-default-attr port)
  `((read . ,(lambda () (read-byte port)))
    (write . ,(lambda (b) (write-byte b port)))
    (close . ,(lambda () (close-port port)))))

(define (port->duct port . attr)
  (let ((attr (list->alist attr)))
    (set-port-text-codec! port us-ascii-codec)
    (make-duct-rec
     port
     (update-force-alist
      attr
      (port-duct-default-attr port)))))

(define (duct-get-property duct tag)
  (cond ((assv tag (duct-attr duct)) => cdr)
        (else #f)))

(define (duct-set-property! duct tag value)
  (duct-set-attr!
   (update-force-alist
    (duct-attr duct)
    (list (cons tag value)))))

(define (read-proc duct)
  (duct-get-property duct 'read))

(define (write-proc duct)
  (duct-get-property duct 'write))

(define (duct-close duct)
  ((duct-get-property duct 'close))
  (and-let* ((rent (duct-parent duct))
             ((duct? rent)))
    (duct-close rent)))

(define (duct-extend* duct property-list)
  (make-duct-rec duct property-list))

(define-syntax duct-extend
  (syntax-rules ()
    ((_ duct (tag val) ...)
     (duct-extend* duct
                   (list (cons 'tag val)
                         ...)))))

(define (duct->input-port duct)
  (make-buffered-input-port
   (make-buffered-input-port-handler
    (lambda (duct)
      `(duct-input-port ,duct))         ; discloser
    (lambda (duct)
      (duct-close duct))                ; closer
    (lambda (port wait?)
      ((read-proc (port-data port)))
      (maybe-commit))                   ; buffer-filler
    e-unimplemented                     ; ready?
    )
   duct
   (make-byte-vector 4096 0)
   0
   4096))

;;;; Specific ducts
(define (d/byte-len-leave-open parent len)
  (let ((port (duct-parent parent)))
    (if (not (port? port))
        (e-illegal-parent parent)
        (duct-extend
         parent
         (read (make-byte-len-reader len ))
         (close (lambda () #t))
         (write e-unimplemented)))))

(define (d/base64 parent)
  (duct-extend
   parent
   (read (lambda (prev)
           (make-base64-reader (read-proc prev))))
   (write e-unimplemented)))

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

;;; base64
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

(define ash arithmetic-shift)

(define (bits-take x n)
  (bitwise-and x (- (ash 1 n) 1)))

(define (bits-drop x n)
  (ash x (- n)))

(define (make-base64-reader next-char)
  (let ((bits 0) (bits-count 0))
    (define (set b bc)
      (set! bits b)
      (set! bits-count bc))
    (define (skip b bc)
      (set b bc)
      (body))
    (define (body)
      (let ((ch (next-char)))
        (or (and (or (eof-object? ch)
                     (char=? ch padding-char))
                 (eof-object))
            (let ((six (vector-ref decoding-vector (char->ascii ch))))
              (if (not six)
                  (skip bits bits-count)
                  (let ((full-bits (bitwise-ior (ash bits 6) six))
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
 (with-string-ports
  "Zm9vYmFy"
  (lambda ()
    (let lp ((next (make-base64-reader read-char)))
      (let ((ch (next)))
        (if (eof-object? ch)
            ch
            (begin (display (ascii->char ch))
                   (lp next))))))) => "foobar")

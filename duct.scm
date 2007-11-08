(define (e-unimplemented . args)
  (error "duct: unimplemented" args))

(define-record-type duct rtd/duct
  (make-duct-rec parent attr)
  duct?
  (parent duct-parent)
  (attr duct-attr duct-set-attr!))

(define-record-discloser rtd/duct
  (lambda (duct)
    `(duct ,(duct-get-property duct 'name))))

(define (port-duct-default-attr port)
  `((reader . ,(lambda () (read-byte port)))
    (writer . ,(lambda (b) (write-byte b port)))
    (closer . ,(lambda () (close-port port)))
    (name . "port/fundamental")))

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

(define (duct-get-first-prop duct tag)
  (if (not (duct? duct))
      (e-unimplemented tag)
      (or (duct-get-property duct tag)
          (duct-get-first-prop (duct-parent duct) tag))))

(define (duct-set-property! duct tag value)
  (duct-set-attr!
   (update-force-alist
    (duct-attr duct)
    (list (cons tag value)))))

(define (find-port-parent duct)
  (let ((p (duct-parent duct)))
    (if (port? p)
        p
        (find-port-parent p))))

(define (read-proc duct)
  (duct-get-first-prop duct 'reader))

(define (duct-read duct)
  ((read-proc duct)))

(define (write-proc duct)
  (duct-get-first-prop duct 'writer))

(define (duct-write duct)
  ((write-proc duct)))

(define (duct-close duct)
  ((duct-get-first-prop duct 'closer)))

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
      `(input-port ,duct))              ; discloser
    (lambda (duct)
      (duct-close duct))                ; closer
    (lambda (port wait?)
      (duct-read (port-data port))
      (maybe-commit))                   ; buffer-filler
    e-unimplemented                     ; ready?
    )
   duct
   (make-byte-vector 4096 0)
   0
   4096))

(define (duct-read-all duct)
  (let lp ()
    (let ((datum (duct-read duct)))
      (if (eof-object? datum)
          '()
          (cons datum (lp))))))

;;;; Specific ducts
(define (d/leave-open)
  (lambda (parent)
    (duct-extend
     parent
     (name "leave-open")
     (closer (lambda () #t)))))

(define (d/byte-len len)
  (lambda (parent)
    (duct-extend
     parent
     (name "byte-len")
     (reader (make-byte-len-reader len (find-port-parent parent))))))

(define (d/ascii)
  (lambda (parent)
    (duct-extend
     parent
     (name "ascii")
     (reader (lambda ()
               (let ((ch (duct-read parent)))
                 (if (eof-object? ch)
                     ch
                     (ascii->char ch)))))
     (writer (lambda (ch)
               (duct-write parent (char->ascii ch)))))))

(define (d/base64)
  (lambda (parent)
    (duct-extend
     parent
     (name "base64")
     (reader (make-base64-reader (read-proc parent)))
     (writer e-unimplemented))))

(define (d/unicode)
  (lambda (parent)
    (duct-extend
     parent
     (name "unicode")
     (reader (lambda ()
               (let ((ch (duct-read parent)))
                 (or (and (eof-object? ch) ch)
                     (and (scalar-value? ch) (scalar-value->char ch))
                     ch)))))))

;;;; generic support procs
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
  (let lp ((next (make-base64-reader read-byte)))
    (let ((ch (next)))
      (if (not (eof-object? ch))
          (begin (display (ascii->char ch))
                 (lp next)))))) => "foobar")

(assert
 (with-string-ports
  "Zm9vYmFyYmF6"
  (let ((out
         ((d/unicode)
          ((d/base64)
           ((d/byte-len 7)
            ((d/leave-open)
             (port->duct (current-input-port))))))))
    (for-each display (duct-read-all out)))) => "fooba")

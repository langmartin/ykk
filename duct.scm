(define (e-unimplemented . args)
  (error "duct: unimplemented" args))

(define (read-byte . maybe-in)
  (let ((x (apply read-char maybe-in)))
    (if (eof-object? x) x
	(char->ascii x))))

(define (write-byte b . maybe-out)
  (apply write-char (ascii->char b) maybe-out))

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

(define (duct-get-this-property duct tag)
  (cond ((assv tag (duct-attr duct)) => cdr)
        (else #f)))

(define (duct-get-property duct tag)
  (if (not (duct? duct))
      (e-unimplemented tag)
      (or (duct-get-this-property duct tag)
          (duct-get-property (duct-parent duct) tag))))

(define (duct-set-property! duct tag value)
  (duct-set-attr!
   (update-force-alist
    (duct-attr duct)
    (list (cons tag value)))))

(define (read-proc duct)
  (duct-get-property duct 'reader))

(define (duct-read duct)
  ((read-proc duct)))

(define (write-proc duct)
  (duct-get-property duct 'writer))

(define (duct-write duct)
  ((write-proc duct)))

(define (duct-close duct)
  ((duct-get-property duct 'closer)))

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

(define (duct-display duct . port)
  (let ((datum (duct-read duct)))
    (or (eof-object? datum)
        (begin
          (apply
           display datum port)
          (apply
           duct-display duct port)))))

(define (duct->string duct)
  (call-with-string-output-port
   (lambda (port)
     (duct-display duct port))))

(assert
 (duct->string
  (port->duct (make-string-input-port "test"))) => "116101115116")

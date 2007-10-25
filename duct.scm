(define-record-type duct rtd/duct
  (make-duct-rec port parent attr)
  duct?
  (port duct-port)
  (attr duct-attr duct-set-attr!))

(define (port->duct port . attr)
  (let ((attr (list->alist attr)))
    (make-duct-rec port #f
                  (update-force-alist
                   alist
                   `((read . ,read-char)
                     (write . ,write-char))))))

(define (duct-use duct tag)
  (cond ((assv tag (duct-attr duct)) => cdr)
        (else #f)))

(define (duct-extend! duct tag value)
  (duct-set-attr!
   (update-force-alist
    (duct-attr duct)
    `((,tag . ,(make-extended-attr
                tag
                (duct-use duct tag)
                value))))))

(define (make-extended-attr tag old new)
  (case tag
    ((READ)
     (lambda ()
       (new old)))
    ((WRITE)
     (lambda (x)
       (new x old)))
    (ELSE
     (error "unimplemented property"))))

(define (duct-extend-read! duct proc)
  (duct-extend! duct 'read proc))

(define (duct-extend-write! duct proc)
  (duct-extend! duct 'write proc))

(define *ducts* (make-table))

(define-syntax define-duct
  (syntax-rules ()
    ((_ (name arg ...) body ...)
     (table-set!
      *ducts* 'name
      (lambda (arg ...)
        (duct-body body ...))))
    ((_ name body ...)
     (table-set!
      *ducts* 'name
      (duct-body body ...)))))

(define-syntax duct-body
  (syntax-rules ()
    ((_ (tag val) ...)
     (cons (cons 'tag val)
           ...))))

(define (make-byte-len len)
  (let ((buf (make-byte-vector len 0)) (idx -1))
    (lambda (prev)
      (and (< idx 0)
           (read-block buf 0 len (duct-port prev))
           (set! idx 0))
      (if (= idx len)
          (eof-object)
          (begin
            (byte-vector-ref buf idx)
            (set! idx (+ idx 1)))))))

(define-duct (byte-len len)
  (read (make-byte-len len))
  (write duct-error))

(define-duct base64
  (read base64-encode-internal)
  (write duct-error))

(define (duct-tape-read port . reads)
  (let ((duct (port->duct port)))
    (for-each (lambda (r)
                (duct-extend! duct 'read r))
              reads)
    (duct->port duct)))

(duct-tape-read
 port
 (read:byte-len 786)
 (read:base64))

(duct-tape
 ((utf8)
  ((base64)
   ((byte-len 248)
    port))))

(define-syntax duct-tape
  (syntax-rules ()
    ((_ ((type ...) exp ...))
     (let ((prev (duct-tape exp ...)))
       (duct-extend-by-props! prev type ...)))
    ((_  port)
     (port->duct port))))

;;;; discard
;; (define (duct-open port . attr)
;;   (let ((attr (list->alist attr))
;;         (make-d2 port #f (cons ()) attr))))

;; (define-record-type duct rtd/duct
;;   (make-duct-struct reader writer ready)
;;   duct?
;;   (reader duct-reader)
;;   (writer duct-writer)
;;   (ready duct-ready))

;; (define (duct-tape parent reader writer ready)
;;   (make-duct-struct
;;    (lambda ()
;;      (reader (duct-reader next)))
;;    (lambda (x)
;;      ((duct-writer next) (writer x)))
;;    (lambda ()
;;      (ready (duct-ready next)))))

;; #;
;; (define-syntax duct-tape
;;   (syntax-rules (:)
;;     ((_ parent x : reader y : writer z : ready)
;;      (make-duct parent
;;                 (lambda (x) reader)
;;                 (lambda (y) writer)
;;                 (lambda (z) ready)))))

;; (define (port->byte-duct port)
;;   (set-port-text-codec! port us-ascii-codec)
;;   (make-duct-struct
;;    (lambda ()
;;      (read-char port))
;;    (lambda (x)
;;      (write-char x port))
;;    (lambda ()
;;      (char-ready? port))))

;; (define (duct->port duct)
;;   #t)

;;;; utilities
(define-syntax begin1
  (syntax-rules ()
    ((_ e1 e2 ...)
     (let ((result e1))
       e2 ...
       result))))

(define (find-first proc lst)
  (let lp ((lst lst))
    (if (null? lst)
        '()
        (or (proc (car lst))
            (lp (cdr lst))))))

(assert (find-first (lambda (x) (and (= x 4) x)) '(1 3 5 4 6)) => 4)

(define (update-alist alist pairs)
  (depth-first (lambda (x)
                 (and (pair? x)
                      (assq (car x) pairs)
                      (cons tag val)))
               alist))

(define (update-force-alist alist pairs)
  (let ((upd (apply update-alist alist pairs)))
    (if (eq? upd alist)
        (cons (cons tag val)
              alist)
        upd)))

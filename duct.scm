(define-record-type duct rtd/duct
  (make-duct-rec parent attr)
  duct?
  (parent duct-parent)
  (attr duct-attr duct-set-attr!))

(define (port->duct port . attr)
  (let ((attr (list->alist attr)))
    (set-port-text-codec! port us-ascii-codec)
    (make-duct-rec
     port
     (update-force-alist
      alist
      (cons (cons 'read-fn (lambda ()
                          (read-byte port)))
            (cons 'write-fn (lambda (b)
                           (write-byte b port))))))))

(define (duct-get-property duct tag)
  (cond ((assv tag (duct-attr duct)) => cdr)
        (else #f)))

(define (read-proc duct)
  (duct-get-property duct 'read-fn))

(define (write-proc duct)
  (duct-get-property duct 'write-fn))

(define (duct-set-property! duct tag value)
  (duct-set-attr!
   (update-force-alist
    (duct-attr duct)
    (list (cons tag value)))))

(define (duct-extend* duct property-list)
  (make-duct-rec duct property-list))

(define-syntax duct-extend
  (syntax-rules ()
    ((_ duct (tag val) ...)
     (duct-extend* duct
                   (cons (cons 'tag val)
                         ...)))))

(define (e-unimplemented . args)
  (error "duct: unimplemented" args))

(define (e-illegal-parent . args)
  (error "duct: illegal parent duct" args))

;;;; standard duct definitions for use by duct-tape
(define *ducts* (make-table))

(define (duct-define* name proc)
  (table-set! *ducts* name proc))

(define (duct-fetch* name)
  (table-ref *ducts* name))

(define-syntax define-duct
  (let-syntax ((def
                (syntax-rules ()
                  ((_ name (extra ...) body ...)
                   (duct-define*
                    'name
                    (lambda (parent extra ...)
                      (duct-extend parent body ...)))))))
    (syntax-rules ()
      ((_ (name arg ...) body ...)
       (def name (arg ...) body ...))
      ((_ (name) body ...)
       (def name () body ...))
      ((_ name body ...)
       (def name () body ...)))))

(define (require-port-parent duct thunk)
  (let ((port (duct-parent prev)))
    (if (not (port? port))
        (e-illegal-parent prev)
        (thunk))))

;; (duct-internal foo () (read 4))
;; (duct-internal foo (len) (read len))

(define-duct (byte-len len)
  (read (make-byte-len-reader len))
  (write e-unimplemented))

(define-duct base64
  (read (lambda (prev)
          (base64-decode-char (read-proc prev))))
  (write e-unimplemented))

;;;; external interface
(duct-tape
 ((utf8)
  ((base64)
   ((byte-len 248)
    port))))

(define-syntax duct-tape
  (syntax-rules ()
    ((_ ((type) exp ...))
     (duct->port 'type (duct-tape-ducts exp ...)))))

(define-syntax duct-tape-ducts
  (let-syntax ((extend
                (syntax-rules ()
                  ((_ type (arg ...) body ...)
                   (let ((prev (duct-tape body ...)))
                     (duct-extend*
                      prev
                      ((defined-duct type) arg ...)))))))
    (syntax-rules ()
      ((_ ((type arg ...) body ...))
       (extend type (arg ...) body ...))
      ((_ ((type) body ...))
       (extend type () body ...))
      ((_  port)
       (port->duct port)))))

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

(define (make-byte-len-reader len)
  (require-port-parent
   duct
   (lambda ()
     (let ((buf (make-byte-vector len 0)) (idx -1))
       (lambda (prev)
         (and (< idx 0)
              (read-block buf 0 len port) ; can't use (read-proc prev) now
              (set! idx 0))
         (if (= idx len)
             (eof-object)
             (begin1
              (byte-vector-ref buf idx)
              (set! idx (+ idx 1)))))))))

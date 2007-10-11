;;;; sexp comment syntax
(define (reader-sexp-comment ch port)
  (read-char port)
  (read port)
  (read port))

(define-sharp-macro #\; reader-sexp-comment)

;;;; http://srfi.schemers.org/srfi-10/srfi-10.html
(define reader-ctor-table '())

(define (lookup sym)
  (cond ((assoc sym reader-ctor-table) =>
         cdr)
        (else
         (reading-error "#, reader macro not found " sym))))

(define (define-reader-ctor sym proc)
  (set! reader-ctor-table
        (cons (cons sym proc)
              reader-ctor-table)))

(define (reader-ctor-dispatch sym . rest)
  (apply (lookup sym) rest))

(define (reading-handler ch port)
  (read-char port)
  (apply reader-ctor-dispatch (read port)))

(define-sharp-macro #\, reading-handler)

;;;; testing (can't unit-test it, these macros are global)

;; (define-reader-ctor
;;   'foo
;;   (lambda expr
;;     (cons 'foobar expr)))

;; (define-reader-ctor 'list list)

;; (read
;;  (make-string-input-port
;;   "(foo bar (baz (bif)) #,(list (+ 1 2) #,(foo bit) baz) blit)"))

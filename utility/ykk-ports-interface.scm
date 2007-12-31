(define-syntax let-port-rest
  (syntax-rules ()
    ((_ rest (port default) body ...)
     (call/datum-rest
      rest
      port?
      default
      (lambda (port rest)
        body ...)))))

(define (string/port->port obj)
  (if (port? obj)
      obj
      (make-string-input-port obj)))

(define (port? obj)
  (or (input-port? obj)
      (output-port? obj)))

(define (close-port port)
  (and (input-port? port) (close-input-port port))
  (and (output-port? port) (close-output-port port)))

(define with-current-output-port call-with-current-output-port)

;; (define-syntax define-let-variation
;;   (syntax-rules ()
;;     ((_ name proc arg ...)
;;      (define-syntax name
;;        (syntax-rules ()
;;          ((_ arg ... . body)
;;           (proc arg ... (define-let-variation body))))))
;;     ((_ (body ...))
;;      (lambda () body ...))))

;; (define-let-variation let-foo foo thing)

(define-syntax let-current-output-port
  (syntax-rules ()
    ((_ port body ...)
     (with-current-output-port port (lambda () body ...)))))

(define (maybe-current-output-port port thunk)
  (if (null? port)
      (thunk)
      (with-current-output-port
          (if (pair? port) (car port) port)
        thunk)))

(define-syntax let-maybe-current-output-port
  (syntax-rules ()
    ((_ port body ...)
     (maybe-current-output-port port (lambda () body ...)))))

(define with-current-input-port call-with-current-input-port)

(define-syntax let-current-input-port
  (syntax-rules ()
    ((_ port body ...)
     (with-current-input-port port (lambda () body ...)))))

(define (maybe-current-input-port port thunk)
  (if (null? port)
      (thunk)
      (with-current-input-port
          (if (pair? port) (car port) port)
        thunk)))

(define-syntax let-maybe-current-input-port
  (syntax-rules ()
    ((_ port body ...)
     (maybe-current-input-port port (lambda () body ...)))))

(define (with-string-output-port thunk)
  (call-with-string-output-port
   (lambda (port)
     (with-current-output-port
         port
       thunk))))

(define-syntax let-string-output-port
  (syntax-rules ()
    ((_ body ...)
     (with-string-output-port (lambda () body ...)))))

(define (with-string-input-port string thunk)
  (with-current-input-port
   (make-string-input-port string)
   thunk))

(define-syntax let-string-input-port
  (syntax-rules ()
    ((_ string body ...)
     (with-string-input-port string (lambda () body ...)))))

(define (call-with-u8-output-port receiver)
  (let ((port (make-byte-vector-output-port)))
    (receiver port)
    (byte-vector-output-port-output port)))

(define (with-u8-output-port thunk)
  (call-with-u8-output-port
   (lambda (port)
     (with-current-output-port
         port
       thunk))))

(define-syntax let-u8-output-port
  (syntax-rules ()
    ((_ body ...)
     (with-u8-output-port (lambda () body ...)))))

(define (with-string-ports input-string thunk)
  (let-string-output-port
   (with-string-input-port
       input-string
     thunk)))

(define-syntax let-string-ports
  (syntax-rules ()
    ((_ input body ...)
     (with-string-ports input (lambda () body ...)))))

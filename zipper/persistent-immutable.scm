;;;; vectors
(define-record-type rtd/zvector
  (direct-make-vector id vector)
  vector?
  (id vector-id)
  (vector r5-vector))

(define-record-discloser rtd/zvector
  (lambda (vec)
    `(zv ,(r5-vector vec))))

(define (make-vector-with-r5 proc . args)
  (let* ((id (uuidgen-v1->hex-string))
         (r5 (apply proc args))
         (vec (direct-make-vector id r5)))
    (write-vector vec)
    (direct-memory-set! id vec)
    vec))

(define (make-vector len . fill)
  (apply make-vector-with-r5 r5:make-vector len fill))

(define (vector . elements)
  (apply make-vector-with-r5 r5:vector elements))

(define (vector-length obj)
  (r5:vector-length (r5-vector obj)))

(define (vector-ref vector k)
  (r5:vector-ref (r5-vector vector) k))

(define (list->vector lst)
  (apply vector lst))

(define (for-each-number proc start stop)
  (fold-numbers (lambda (x acc)
                  (proc x))
                #f
                start
                stop
                1))

(define (vector-for-each proc vector)
  (for-each-number
   (lambda (idx)
     (proc (vector-ref vector idx)))
   0
   (vector-length vector)))

;;;; per-object logging bits
(define (recover-vector-body vector-el)
  (let ((vec (apply r5:vector vector-el)))
    (for-each-number (lambda (idx)
                       (let ((obj (direct-memory-ref (r5:vector-ref vec idx))))
                         (if obj
                             (vector-set! vec idx obj))))
                     0
                     (r5:vector-length vec))
    vec))

(define (write-vector vec)
  (write-log
   (lambda ()
     (disp "(v " (vector-id vec))
     (vector-for-each (lambda (el)
                        (display #\space)
                        (display (disclose-object el)))
                      vec)
     (disp ")"))))

(define (persistent-symbol sym)
  (table-ref *symbol* sym))

(define (persistent-symbol-set! sym val)
  (write-log
   (lambda ()
     (write (list '! sym (disclose-object val))))
   (call-ensuring-atomicity!
    (lambda ()
      (table-set! *symbol* sym val)))))

;;;; logging, "memory" store
(define (e-unkown-type obj)
  (error "unknown object type" obj))

(define *cdr* (make-string-table))

(define *symbol* (make-symbol-table))

(define (direct-memory-ref id)
  (if (not (string? id))
      #f
      (table-ref *cdr* id)))

(define (direct-memory-set! id datum)
  (call-ensuring-atomicity!
   (lambda ()
     (table-set! *cdr* id datum))))

(define (disclose-object obj)
  (if (vector? obj)
      (vector-id obj)
      obj))

(define *log* #f)

(define (log-set! port)
  (set! *log* port))

(define-syntax let-list
  (syntax-rules ()
    ((_ (formals lst) body ...)
     (apply (lambda formals
              body ...)
            lst))))

(define (replay-log-port port)
  (call-ensuring-atomicity!
   (lambda ()
     (port-fold (lambda (expr acc)
                  ;; (eval expr (interaction-environment))
                  (let-list ((head id . tail) expr)
                    (case head
                      ((v) (table-set!
                            *cdr*
                            id
                            (direct-make-vector id (recover-vector-body tail))))
                      ((!) (table-set!
                            *symbol*
                            id
                            (direct-memory-ref (car tail)))))))
                #f
                read
                port))))

(define (write-log thunk)
  (or (not *log*)
      (with-current-output-port
          *log*
        (call-ensuring-atomicity!
         thunk))))

(define (list-replay-test)
  (replay-log-port (open-input-file "/tmp/log")))

(define (cdr-table-count)
  (let ((count 0))
    (table-walk (lambda (k v)
                  (set! count (+ count 1)))
                *cdr*)
    count))

;;;; vectors
(define-record-type rtd/zvector
  (make-vector* id vector)
  vector?
  (id vector-id)
  (vector r5-vector))

(define-record-discloser rtd/zvector
  (lambda (vec)
    `(zv ,(r5-vector vec))))

(define (make-vector-with-r5 proc . args)
  (let* ((id (uuidgen))
         (r5 (apply proc args))
         (vec (make-vector* id r5)))
    (bury id vec)
    (write-vector vec)
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

;;;; logging, "memory" store
(define (e-unkown-type obj)
  (error "unknown object type" obj))

(define *cdr* (make-table))

(define (exhume id)
  (if (or (pair? id))
      #f
      (table-ref *cdr* id)))

(define (bury id datum)
  (table-set! *cdr* id datum))

(define (disclose-object obj)
  (if (vector? obj)
      (vector-id obj)
      obj))

(define (replay-log)
  (without-log
   (port-fold (lambda (expr acc)
                (let ((head (car expr)) (tail (cdr expr)))
                  (cond ((eq? 'v head)
                         (apply replay-vector! tail))
                        ((eq? '! head)
                         (table-set! *cdr*
                                     (car tail)
                                     (exhume (cadr tail)))))))
              #f
              read
              port)))

(define (write-log thunk)
  (or (not (current-log))
      (begin
        (with-current-output-port
           (current-log)
         thunk)
        (newline (current-log)))))

(define (replay-vector! id . vector-el)
  (let ((r5vec (apply r5:vector vector-el)))
   (for-each-number (lambda (idx)
                      (let ((obj (exhume (r5:vector-ref r5vec idx))))
                        (if obj
                            (vector-set! r5vec idx obj))))
                    0
                    (r5:vector-length r5vec))
   (bury id (make-vector* id r5vec))))

(define (write-vector vec)
  (write-log
   (lambda ()
     (disp "(v " (vector-id vec))
     (vector-for-each (lambda (el)
                        (display #\space)
                        (display (disclose-object el)))
                      vec)
     (disp ")"))))

(define (persistent-symbol-set! sym val)
  (write-log
   (lambda ()
     (write (list '! sym (disclose-object val))))))

(define (persistent-symbol sym)
  (table-ref *cdr* sym))

(define (list-replay-test)
  (replay-log-port (open-input-file "/tmp/log")))

(define (cdr-table-count)
  (let ((count 0))
    (table-walk (lambda (k v)
                  (set! count (+ count 1)))
                *cdr*)
    count))

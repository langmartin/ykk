;;;; lists
(define-record-type rtd/zcons
  (cons-cons id next car cdr)
  pair?
  (id cons-id)
  (next cons-next)
  (car cons-car)
  (cdr cons-cdr cons-set-cdr!))

(define-record-discloser rtd/zcons
  (lambda (cons)
    `(zc ,(cons-car cons)
         ,(cdr cons))))

(define null '())

(define null? r5:null?)

(define (cons car cdr)
  (let* ((cdr-loc (and (pair? cdr)
                       (cons-id cdr)))
         (cell (cons-cons (uuidgen)
                          cdr-loc
                          car
                          cdr)))
    (bury (cons-id cell) cell)
    (write-cell cell)
    cell))

(define (list . arguments)
  (r5:fold-right cons
                 null
                 arguments))

(define (list? obj)
  (or (null? obj)
      (pair? obj)))

(define car cons-car)

(define (cdr obj)
  (or (cons-cdr obj)
      (let* ((loc (cons-next obj))
             (val (if (not loc) null (exhume loc))))
        (cons-set-cdr! obj val)
        val)))

;;;; vectors
(define-record-type rtd/zvector
  (make-vector* id vector)
  vector?
  (id vector-id)
  (vector r5-vector))

(define-record-discloser rtd/zvector
  (lambda (vec)
    `(zv ,(r5-vector vec))))

(define (make-vector-general proc . args)
  (let ((vec (re-vector (uuidgen) (apply proc args))))
    (write-vector vec)
    vec))

(define (make-vector len . fill)
  (apply make-vector-general r5:make-vector len fill))

(define (vector . elements)
  (apply make-vector-general r5:vector elements))

(define (vector-length obj)
  (r5:vector-length (r5-vector obj)))

(define (vector-ref vector k)
  (r5:vector-ref (r5-vector vector) k))

(define (list->vector lst)
  (apply vector lst))

(define (for-each-number proc start stop)
  (let lp ((idx start))
    (if (not (= idx stop))
        (begin
          (proc idx)
          (lp (+ 1 idx))))))

(define (vector-for-each-index proc vector)
  (for-each-number proc 0 (vector-length vector)))

(define (copy-r5-vector vec)
  (let ((out (r5:make-vector (vector-length vec))))
    (vector-for-each-index
     (lambda (idx)
       (r5:vector-set! out idx (vector-ref vec)))
     vec)
    out))

(define (vector-set-r5 vec k obj)
  (r5:vector-set!
   (copy-r5-vector vec)
   k
   obj))

(define (vector-set vec k obj)
  (let ((new (vector-set-r5 vec k obj)))
    (re-update (uuidgen) (vector-id vec) k obj)
    (write-vector-update new vec k obj)
    new))

;;;; logging, "memory" store
(define (e-unkown-type obj)
  (error "unknown object type" obj))

(define *cdr* (make-table))

(define (exhume id)
  (table-ref *cdr* id))

(define (bury id datum)
  (table-set! *cdr* id datum))

(define (disclose-object obj)
  (cond ((list? obj)
         (cons-id obj))
        ((vector? obj)
         (vector-id obj))
        (else
         obj)))

(define-fluid (*log* #f)
  current-log
  with-log)

(define (set-log! port)
  (set-fluid! *log* port))

(define-syntax without-log
  (syntax-rules ()
    ((_ body ...)
     (with-log #f (lambda () body ...)))))

(define (reopen-log-file file-name)
  (if (current-log) (close-output-port (current-log)))
  (set-log! (open-output-file file-name)))

(define (replay-log-port port)
  (without-log
   (let ((top-id #f))
     (port-fold
      (lambda (expr)
        (if (string? expr)
            (set! top-id expr)
            (apply eval expr)))
      #f
      read
      port))
   (set! *top* (exhume top-id))
   *top*))

;;;; specific data type handlers
(define (write-cell cell)
  (or (not (current-log))
      (let-current-output-port
          (current-log)
        (write (list
                're-cons
                (cons-id cell)
                (cons-next cell)
                (disclose-object (cons-car cell))))
        (newline))))

(define (re-cons id next car)
  (bury id
        (cons-cons
         id
         next
         (if next car '())
         #f)))

(define (write-vector vec)
  (or (not (current-log))
      (let-current-input-port
          (current-log)
        (disp "(re-vector " (vector-id id))
        (vector-for-each-index (lambda (idx)
                                 (display #\space)
                                 (write (disclose-object
                                         (vector-ref vec idx))))
                               vec)
        (display ")")
        (newline))))

(define (write-vector-update new old k obj)
  (or (not (current-log))
      (let-current-output-port
          (current-log)
        (write (list 're-update new old k obj))
        (newline))))

(define (re-vector id . args)
  (bury id
        (apply make-vector*
               id
               (r5:map (lambda (el)
                         (or (exhume el)
                             el))
                       args))))

(define (re-update id old k obj)
  (bury id
        (vector-set-r5 (exhume old) k obj)))

;;;; top of the tree, a persistent-immutable list
(define *top* '())

(define (fold-pair->fold fold-pair cons nil lst)
  (fold-pair (lambda (lst acc)
               (cons (car lst) acc))
             nil
             lst))

(define (fold-pair cons nil lst)
  (if (null? lst)
      nil
      (fold-pair cons
                 (cons lst nil)
                 (cdr lst))))

(define (fold cons nil lst)
  (fold-pair->fold
   fold-pair cons nil lst))

(define (for-each proc lst)
  (fold (lambda (x acc)
           (proc x))
         #f
         lst))

(define (assoc/predicate pred? lst tag)
  (call-with-current-continuation
   (lambda (found)
     (for-each (lambda (pair)
                 (if (pred? tag (car pair))
                     (found pair)))
               lst)
     #f)))

(define (assq lst tag)
  (assoc/predicate eq? lst tag))

(define (top-log)
  (display (cons-id *top*) (current-log))
  (newline (current-log)))

(define (top-ref tag)
  (cond ((assq tag *top*) => cdr)
        (else #f)))

(define (top-set tag val)
  (let ((new
         (fold (lambda (x top)
                 (if (eq? tag (car x))
                     (cons (cons tag val)
                           top)
                     (cons x top)))
               '()
               *top*)))
    (set! *top* new)
    (top-log)))

(define (top-del tag)
  (fold (lambda (x top)
              (if (eq? tag (car x))
                  top
                  (cons x top)))
        '()
        *top*))

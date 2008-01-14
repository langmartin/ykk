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

(define *cdr* (make-table))

(define (exhume id)
  (table-ref *cdr* id))

(define (bury id datum)
  (table-set! *cdr* (cons-id cell) cell))

;;;; lists
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
    (record-cell cell)
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

(define (record-cell cell)
  (or (not (log))
      (write (list
              (cons-id cell)
              (cons-next cell)
              (record-car (cons-car cell)))
             (log))
      (newline (log))))

(define (restore-cell lst)
  (apply (lambda (id next car)
           (bury id
                 (cons-cons
                  id
                  next
                  (if next car '())
                  #f)))
         lst))

;;;; logging
(define (record-car val)
  (cond ((list? val)
         (list-id val))
        ((vector? val)
         (vector-id val))
        (else
         val)))

(define-fluid *log* #f
  log
  with-log)

(define (set-log! port)
  (set-fluid! *log* port))

(define-syntax without-log
  (syntax-rules ()
    ((_ body ...)
     (with-log #f body ...))))

(define (reopen-log-file file-name)
  (if (log) (close-output-port (log)))
  (set-log! (open-output-file file-name)))

(define (replay-log-port port)
  (without-log
   (let ((top-id #f))
     (let-current-input-port
         port
       (let lp ()
         (let ((next (read)))
           (if (not (eof-object? next))
               (begin
                 (cond ((not (pair? next)) (set! top-id next))
                       ((r5:vector? (cdr next))
                        (restore-vector next))
                       (else
                        (replay-cell next)))
                 (lp)))))))
   (set! *top* (exhume top-id))
   *top*))

(define *top* null)

(define (top-log)
  (display (cons-id *top*) (log))
  (newline (log)))

(define (fold-top cons)
  (let ((top (fold cons '() *top*)))
    (or (eq? top *top*)
        top)))

(define (top-ref tag)
  (cond ((assoc tag *top*) => cdr)
        (else #f)))

(define (top-set tag val)
  (let ((top
         (or (fold-top (lambda (x top)
                         (if (eq? tag (car x))
                             (cons (cons tag val)
                                   top)
                             (cons x top))))
             (cons (cons 'tag val)
                   *top*))))
    (set! *top* top)
    (top-log)))

(define (top-del tag)
  (fold-top (lambda (x top)
              (if (eq? tag (car x))
                  top
                  (cons x top)))))

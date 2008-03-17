(define (backslash-escape quote string)
  (let-string-output-port
   (display quote)
   (string-for-each (lambda (c)
                      (if (char=? c quote)
                          (display #\\))
                      (display c))
                    string)
   (display quote)))

(define (json-escape string)
  (backslash-escape #\' string))

(define (alist? obj)
  (and (pair? obj)
       (pair? (car obj))))

(define (json-atom val)
  (cond ((symbol? val) val)
        ((number? val) val)
        ((string? val) (json-escape val))
        ((alist? val) (alist->json val))
        ((null? val) "false")
        (else
         (error "bad value" val))))

(define (json-pair pair)
  (let ((key (car pair)) (val (cdr pair)))
    (disp (json-atom key)
          #\:
          (json-atom val))))

(define (alist->json alist)
  (let-string-output-port
   (disp "{" )
   (json-pair (car alist))
   (for-each (lambda (pair)
               (display ",")
               (json-pair pair))
             (cdr alist))
   (disp "}")))

(assert
 (alist->json '((success . ((a . 2) (b . 3))) (failure . ((a . 1)))))
 => "{success:{a:2,b:3},failure:{a:1}}")

;;;; Parsing
(define (next pred duct)
  (begin1
   (duct-next-chunk pred duct)
   (duct-read duct)))

(define (json-scalar duct)
  (let ((cur (duct-peek duct)))
    (case cur
      ((#\' #\") (json-str duct))
      (else
       (cond ((numeric? cur) (json-num duct))
             ((alphabetic? cur) (json-sym duct))
             (else #f))))))

(define (json-num duct)
  (string->number
   (next (lambda (c)
           (not (numeric? c)))
         duct)))

(define (json-str-esc duct)
  (let ((ch (duct-read duct)))
    (if (string-null? ch)
        #f
        (case ch
          ((n) #\newline)
          ((r) #\return)
          ((t) #\tab)
          (else ch)))))

(define (json-str duct)
  (define (next)
    (duct-next-chunk '(#\\ terminator) duct))
  (let-string-output-port
   (let ((terminator (duct-read duct)))
     (let lp ()
       (display (next))
       (or (char=? terminator (duct-read duct))
           (begin
             (display (json-str-esc duct))
             (lp)))))))

(define (json-sym duct)
  (next (lambda (ch)
          (case ch
            ((#\_) #t)
            (else (alphabetic? ch))))
        duct))

(define (json-key duct)
  (and-let* ((key (json-scalar))
             ((not (string-null? key)))
             ((char=? #\: (duct-peek duct)))
             ((duct-read duct)))
    key))

(define (json-obj duct cons unfold)
  (unfold not
          identity
          (lambda (seed)
            (and-let* ((key (json-key duct)))
              (cons key (json-any duct cons unfold))))))

(define (json-arr duct cons unfold)
  (unfold not
          identity
          (lambda (seed)
            (json-any duct cons unfold))))

(define (json-any duct . kons-unfolt)
  (let-optionals* kons-unfolt ((kons cons) (unfolt unfold))

    (and-let* ((cur (duct-peek duct))
               ((not (string-null? cur))))
      (or (and-let* ((proc (case cut
                             ((#\{) json-obj)
                             ((#\[) json-arr)
                             (else #f))))
            (duct-read)
            (proc duct kons unfolt))
          (json-scalar duct)))))

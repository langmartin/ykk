(define (alphanumeric? ch)
  (or (and (char>=? ch #\a) (char<=? ch #\z))
      (and (char>=? ch #\A) (char<=? ch #\Z))
      (and (char>=? ch #\0) (char<=? ch #\9))))

(define (numeric? ch)
  (and (char>=? ch #\0) (char<=? ch #\9)))

(define (alphabetic? ch)
  (or (and (char>=? ch #\a) (char<=? ch #\z))
      (and (char>=? ch #\A) (char<=? ch #\Z))))

(define (backslash-escape quote string)
  (let-string-output-port
   (display quote)
   (string-for-each (lambda (c)
                      (if (char=? c quote)
                          (display #\\))
                      (display c))
                    string)
   (display quote)))

(define (alist? obj)
  (and (pair? obj)
       (pair? (car obj))))

(define (duct-null? ch)
  (and (string? ch) (string-null? ch)))

;;;; json output
(define (json-escape string)
  (backslash-escape #\' string))

(define (json-atom val)
  (cond ((symbol? val) val)
        ((number? val) val)
        ((string? val) (json-escape val))
        ((alist? val) (alist->json-obj val))
        ((pair? val) (list->json-arr val))
        ((null? val) "false")
        (else
         (error "bad value" val))))

(define (json-pair pair)
  (let ((key (car pair)) (val (cdr pair)))
    (disp (json-atom key)
          #\:
          (json-atom val))))

(define (alist->json-obj alist)
  (let-string-output-port
   (disp "{" )
   (json-pair (car alist))
   (for-each (lambda (pair)
               (display ",")
               (json-pair pair))
             (cdr alist))
   (disp "}")))

(define (list->json-arr lst)
  (let-string-output-port
   (display #\[)
   (json-atom (car lst))
   (for-each (lambda (x)
               (display #\,)
               (json-atom x))
             lst)
   (display #\])))

(assert
 (alist->json-obj '((success . ((a . 2) (b . 3))) (failure . ((a . 1)))))
 => "{success:{a:2,b:3},failure:{a:1}}")

;;;; Parsing
(define (e-parse . args)
  (apply error "json parse error" args))

(define (not-whitespace? c)
  (not (whitespace? c)))

(define (eat-whitespace duct)
  (duct-next-chunk-for-each not-whitespace? identity duct #f))

(define (json-num duct)
  (string->number
   (duct-next-chunk
    (lambda (c)
      (not (numeric? c)))
    duct)))

(define (json-str-esc duct)
  (let ((ch (duct-read duct)))
    (or (case ch
          ((n) #\newline)
          ((r) #\return)
          ((t) #\tab)
          (else #f))
        (if (duct-null? ch)
            #f
            ch))))

(define (json-str duct)
  (eat-whitespace duct)
  (let ((terminator (duct-read duct)))
    (if (char? terminator)
        (let-string-output-port
         (define pred? (string-or-chars->predicate `(#\\ ,terminator)))
         (let lp ()
           (duct-next-chunk-for-each pred? display duct #f)
           (let ((c (duct-read duct)))
             (cond ((eof-object? c) #t)
                   ((char=? terminator c) #t)
                   (else
                    (display (json-str-esc duct))
                    (lp)))))))))

(define (json-sym duct)
  (string->symbol
   (duct-next-chunk
    (lambda (ch)
      (not (or (char=? ch #\_)
               (alphabetic? ch))))
    duct)))

(define (json-scalar duct)
  (let ((cur (duct-peek duct)))
    (if (eof-object? cur)
        cur
        (cond ((numeric? cur) (json-num duct))
              ((alphabetic? cur) (json-sym duct))
              ((and (char? cur)
                    (case cur
                      ((#\' #\") (json-str duct))
                      (else #f))) =>
                      identity)
              (else #f)))))

(define (json-assert-char char msg duct)
  (eat-whitespace duct)
  (let ((cur (duct-peek duct)))
    (and (not (eof-object? cur))
         (char=? char cur))))

(define (json-key duct)
  (let ((key (json-scalar duct)))
    (if (not (json-assert-char #\: key duct))
        (e-parse "not a key"))
    (duct-read duct)
    key))

(define eoo* (list 'end-of-object))
(define (eoo) eoo*)
(define (eoo? obj) (eq? obj eoo*))

(define (port-fold-right cons nil reader . port)
  (let ((current (apply reader port)))
    (if (or (eof-object? current) (eoo? current))
        nil
        (cons current
              (apply
               port-fold-right cons nil reader port)))))

(define (json-obj* cons nil duct ocons opair-pass opair json-key terminator)
  (let ((end #f))
    (port-fold-right
     ocons
     nil
     (lambda ()
       (if end (eof-object)
           (let* ((key (json-key duct))
                  (val (json-any cons nil duct ocons opair-pass)))
             (cond (end (eof-object))
                   ((json-assert-char #\, val duct)
                    (duct-read duct)
                    (opair key val))
                   ((json-assert-char terminator val duct)
                    (duct-read duct)
                    (set! end #t)
                    (opair key val))
                   (else
                    (e-parse "object")))))))))

(define (json-obj cons nil duct ocons opair)
  (json-obj* cons nil duct ocons opair
             opair
             json-key
             #\}))

(define (json-arr cons nil duct ocons opair)
  (json-obj* cons nil duct ocons opair
             (lambda (k v) v)
             (lambda (duct) #f)
             #\]))

(define (json-any cons nil duct ocons opair)
  (eat-whitespace duct)
  (let ((cur (duct-peek duct)))
    (cond ((eof-object? cur) nil)
          ((case cur
             ((#\{) json-obj)
             ((#\[) json-arr)
             (else #f)) =>
             (lambda (x)
               (duct-read duct)
               (x cons nil duct ocons opair)))
          (else
           (json-scalar duct)))))

(define (json-fold-right kons nil duct . obj-conses)
  (let-optionals* obj-conses ((ocons kons)
                              (opair cons))
    (json-any kons nil duct ocons opair)))

(assert
 (let-string-input-port
     "{success:{a:2,b:3},failure:{a:1},foo: [1, 2, 3]}"
   (let ((in ((d/ascii) (port->duct (current-input-port)))))
     (json-fold-right cons '() in))) =>
     '((success (a . 2) (b . 3)) (failure (a . 1)) (foo 1 2 3)))

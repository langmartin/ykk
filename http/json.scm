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
  (let-string-output-port
   (let ((terminator (duct-read duct)))
     (let lp ()
       (display (duct-next-chunk '(#\\ terminator) duct))
       (or (char=? terminator (duct-read duct))
           (begin
             (display (json-str-esc duct))
             (lp)))))))

(define (json-sym duct)
  (string->symbol
   (duct-next-chunk
    (lambda (ch)
      (not (or (char=? ch #\_)
               (alphabetic? ch))))
    duct)))

(define (json-scalar duct)
  (and-let* ((cur (duct-peek duct))
             ((not (eof-object? cur))))
    (cond ((numeric? cur) (json-num duct))
          ((alphabetic? cur) (json-sym duct))
          ((and (char? cur)
                (case cur
                  ((#\' #\") (json-str duct))
                  (else #f))) =>
                  identity)
          (else #f))))

(assert
 (let-string-input-port
     ;; "{success:{a:2,b:3},failure:{a:1}}"
     "'key':"
   (let ((in ((d/ascii) (port->duct (current-input-port)))))
     ;; (json-parse cons '() in)
     (json-str in)))
 )

(define (json-assert-char char msg duct)
  (duct-next-chunk not-whitespace duct)
  (or (and-let* ((cur (duct-read duct))
                 ((not (eof-object? cur)))
                 ((char=? char cur)))
        #t
        (error "json parse error (bad seperator)" msg))))

(define (json-key duct)
  (let ((key (json-scalar duct)))
    (json-assert-char #\: key duct)
    key))

(define (port-fold-right cons nil reader . port)
  (let ((current (apply reader port)))
    (if (not current)                   ; (eof-object? current)
        nil
        (cons current
              (port-fold-right cons nil reader)))))

(define (json-obj cons objcons nil duct)
  (port-fold-right
   cons
   nil
   (lambda ()
     (and-let* ((key (json-key duct))
                (val (json-any duct cons unfold)))
       (json-assert-char #\, val duct)
       (objcons key val)))))

(define (json-arr cons objcons nil duct)
  (port-fold-right
   cons
   nil
   (lambda ()
     (json-any duct cons unfold))))

(define (json-any cons objcons nil duct)
  (and-let* ((cur (duct-peek duct))
             ((not (duct-null? cur))))
    (or (and-let* ((proc (case cur
                           ((#\{) json-obj)
                           ((#\[) json-arr)
                           (else #f))))
          (duct-read)
          (proc cons objcons nil duct))
        (json-scalar duct))))

(define (json-parse cons nil duct . obj-pair-cons)
  (let-optionals* obj-pair-cons ((objcons cons))
    (and-let* ((cur (duct-peek duct))
               ((not (duct-null? cur))))
      (or (and-let* ((proc (case cur
                             ((#\{) json-obj)
                             ((#\[) json-arr)
                             (else #f))))
            (duct-read duct)
            (proc cons objcons nil duct))
          (json-scalar duct)))))




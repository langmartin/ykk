(define (days n)
  (* n 86400))

(define (when-make-days* . names)
  (map (lambda (x)
         (case x
           ((sun sunday) 0)
           ((mon monday) 1)
           ((tue tuesday) 2)
           ((wed wednesday) 3)
           ((thu thursday) 4)
           ((fri friday) 5)
           ((sat saturday) 6)
           (else (error "bad date format" x))))
       names))

(define-syntax when-make-days
  (syntax-rules ()
    ((_ days ...)
     (when-make-days* 'days ...))))

(define (maximum pred lst)
  (fold (lambda (x seed)
          (if (pred x seed)
              x
              seed))
        (car lst)
        (cdr lst)))

(define (least lst)
  (maximum < lst))

(assert (least '(3 7 48 0)) => 0)

(define (pick pred x y)
  (if (pred x y) x y))

(define (untime unix-time)
  (apply values
         (string-split #\space (seconds->date "%Y %m %d %w %U"))))

(define (leap-year? year)
  (cond
   ((= 0 (modulo year 400)) #t)
   ((= 0 (modulo year 100)) #f)
   ((= 0 (modulo year 4)) #t)
   (else #f)))

(assert (leap-year? 1900) => #f (leap-year? 2000) => #t)

(define (next-month year month)
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((2) (if (leap-year? year) 29 28))
    (else 30)))

(define (valid-day? year month day)
  (< day (next-month year month)))

(define (next-monthday year month day monthday)
  (if (< day monthday)
      (if (valid-day? year month monthday)
          (- monthday day)
          (+ (days-left-in-month year month day)
             monthday))
      (+ (days-left-in-month year month day)
             monthday)))

(define (next-weekday weekday weekday-list)
  (let* ((weekday-list (list-sort < weekday-list)))
    (cond ;; ((memq weekday weekday-list) => car)
          ((let lp ((lst weekday-list))
             (if (null? lst)
                 #f
                 (if (>= (car lst) weekday)
                     (car lst)
                     (lp (cdr lst))))) => identity)
          (else (car weekday-list)))))

(assert (next-weekday 6 '(0 3 5)) => 0
        (next-weekday 2 '(1 3 5)) => 3
        (next-weekday 5 '(1 3 5)) => 5)

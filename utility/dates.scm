(import-dynamic-externals "c-lib/dates")

;; C Functions
(import-lambda-definition strptime (str fmt) "strptime_for_s48")
(import-lambda-definition strftime (fmt time) "strftime_for_s48")
(import-lambda-definition difftime (tm1 tm2) "difftime_for_s48")

;; Errors
(define-condition date-error (error) date-error?)

;; Public Interface
(define (date->seconds date fmt)
  (let ((time (strptime date fmt)))
    (if time
        (make-time time)
        (date-error (string-append "date->time: failed parsing date `" date "' as `" fmt "'")))))

(define (date->time date fmt)
  (make-time (date->seconds date fmt)))

(define (seconds->date time fmt)
  (strftime fmt time))

(define (time->date time fmt)
  (seconds->date (time-seconds time) fmt))

(define (smart/date->seconds date)
  (let ((day "[0-9]\\{1,2\\}/[0-9]\\{1,2\\}/[0-9]\\{4\\}")
        (day-fmt "%m/%d/%Y")
        (time "[0-9]\\{1,2\\}:[0-9]\\{2\\}")
        (time-fmt "%H:%M")
        (am/pm "[AP]M")
        (am/pm-fmt "%p")
        (rx (lambda args
              (apply string-append
                     (append (list "^")
                             (intersperse " " args)
                             (list "$")))))
        (space (lambda args
                 (apply string-append (intersperse " " args)))))
    (or (case-posix-regex date
          ((rx day time am/pm)
           (date->seconds date (space day-fmt time-fmt am/pm-fmt)))
          ((rx day time)
           (date->seconds date (space day-fmt time-fmt)))
          ((rx day)
           (date->seconds (space date "0:0")  (space day-fmt time-fmt))))
        (date-error "smart/date->time: failed to detect date format for date `" date "'"))))

(define (smart/date->time date)
  (make-time (smart/date->seconds date)))

;; Tests
(assert (time->date (smart/date->time "3/20/2008") "%D") =>
        "03/20/08")

(assert (time->date (smart/date->time "3/20/2008 1:30") "%D %H:%M") =>
        "03/20/08 01:30")

(assert (time->date (smart/date->time "3/20/2008 1:30 PM") "%D %H:%M %p") =>
        "03/20/08 13:30 PM")

(assert (time->date (date->time "Wednesday, March 26, 2008 5:20" "%a, %b %d, %Y %H:%M") "%D %H:%M") =>
        "03/26/08 05:20")

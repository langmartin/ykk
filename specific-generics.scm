(define-generic page:)

(define-method (page: :value)
  :value)

(define-method (page: :date)
  (date->string :date))

(define-method (page: :locale-string)
  (locale-cell (current-locale) :locale-string))

(define-method (page: :content)
  `(div (@ (id "content"))
        ,@(map page: (content-children :content))))

(define-generic validate:)

(define-method (validate: :value)
  :value)

(define-method (validate: :date candidate)
  (if (string? candidate)
      (string->date candidate)
      candidate))

(define-generic csv:)

(define-method (csv: :value)
  :value)

(define-method (csv: :string)
  (let-output-string
   (display #\")
   (escape-by-doubling #\" :string)
   (display #\")))

(define-method (csv: :locale-string)
  (csv: (locale-string :locale-string)))

(define-generic page)

(define-method (page :value)
  :value)

(define-method (page :date)
  (date->string :date))

(define-method (page :content)
  `(div (@ (id "content"))
        ,@(map page (content-children :content))))

(define-generic csv)

(define-method (csv :value)
  :value)

(define-method (csv :string)
  (escape-and-quote #\" string))

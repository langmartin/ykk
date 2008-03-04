(define define-record-discloser s48:define-record-discloser)

(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type-name . stuff)
     (s48:define-record-type type-name type-name . stuff))))

;;; ; Identifiers
;; Identifiers are represented as symbols.

(define (identifier foo)
  (cond ((string? foo) (string->symbol foo))
        ((symbol? foo) foo)
        (else (error 'bad-identifier
                     "an identifier must be a string or a symbol"
                     foo))))

(define identifier? symbol?)

(define new-identifier uuidgen)
